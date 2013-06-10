open Tools.Ops
module Buffer = OcamlBuffer (* fixme *)

let init_top_view current_buffer_ref toplevel_buffer =
  let top_view = Gui.open_toplevel_view toplevel_buffer in
  (* marks in the top viewwhere message from different sources are
     printed. Useful for not mixing them, and keeping locations *)
  let stdout_mark, ocaml_mark, prompt_mark =
    let create_top_mark () =
      `MARK (toplevel_buffer#create_mark
          toplevel_buffer#end_iter
          ~left_gravity:false)
    in
    create_top_mark (), create_top_mark (), create_top_mark ()
  in
  ignore @@ toplevel_buffer#connect#changed ~callback:(fun () ->
      ignore @@ top_view#scroll_mark_onscreen prompt_mark);
  let replace_marks () =
    toplevel_buffer#insert ~iter:toplevel_buffer#end_iter "\n";
    toplevel_buffer#move_mark stdout_mark
      ~where:toplevel_buffer#end_iter#backward_char;
    toplevel_buffer#insert ~iter:toplevel_buffer#end_iter
      ~tags:[Buffer.Tags.invisible] " ";
    toplevel_buffer#move_mark ocaml_mark
      ~where:toplevel_buffer#end_iter#backward_char;
    toplevel_buffer#move_mark prompt_mark
      ~where:toplevel_buffer#end_iter;
  in
  let insert_top ?tags mark text =
    let iter = toplevel_buffer#get_iter_at_mark mark in
    toplevel_buffer#insert ~iter ?tags text
  in
  let display_top_query phrase =
    (* toplevel_buffer#insert ~iter:toplevel_buffer#end_iter "\n# "; *)
    insert_top ~tags:[Buffer.Tags.phrase] prompt_mark phrase;
    let phrase_len = String.length phrase in
    if phrase_len > 0 && phrase.[phrase_len - 1] <> '\n' then
      insert_top ~tags:[Buffer.Tags.phrase] prompt_mark "\n"
  in
  let display_stdout response =
    let rec disp_lines = function
      | [] -> ()
      | line::rest ->
          let offset =
            (toplevel_buffer#get_iter_at_mark stdout_mark)#line_offset
          in
          let line =
            if offset >= 1024 then ""
            else if offset + String.length line <= 1024 then line
            else
              let s = String.sub line 0 (1024 - offset) in
              String.blit "..." 0 s (1024 - offset - 3) 3;
              s
          in
          insert_top ~tags:[Buffer.Tags.stdout] stdout_mark line;
          if rest <> [] then
            (insert_top ~tags:[Buffer.Tags.stdout] stdout_mark "\n";
             disp_lines rest)
    in
    let delim = Str.regexp "\r?\n" in
    let lines = Str.split_delim delim response in
    disp_lines lines
  in
  let display_top_response response =
    let rec disp_lines = function
      | [] -> ()
      | line::rest ->
          if String.length line > 0 && line.[0] = '#' then
            insert_top ocaml_mark "\n";
          insert_top  (* ~tags:[Buffer.Tags.ocamltop] *) ocaml_mark line;
          if rest <> [] then
            (insert_top ocaml_mark "\n";
             disp_lines rest)
    in
    let delim = Str.regexp "\r?\n" in
    let lines = Str.split_delim delim response in
    disp_lines lines
  in
  let mark_error_in_source_buffer (gbuf:GSourceView2.source_buffer)
      ~start ~stop =
    let errmark = gbuf#create_source_mark ~category:"error" start in
    gbuf#apply_tag Buffer.Tags.error ~start ~stop;
    let mark_remover_id = ref None in
    let callback () =
      (match !mark_remover_id with
       | Some id -> gbuf#misc#disconnect id
       | None -> Tools.debug "Warning, unbound error unmarking callback";
           raise Exit);
      let start = gbuf#get_iter_at_mark errmark#coerce in
      let stop = start#forward_to_tag_toggle (Some Buffer.Tags.error) in
      gbuf#remove_tag Buffer.Tags.error ~start ~stop;
      (* buf#remove_source_marks ~category:"error" ~start ~stop ()
           -- may segfault sometimes (??!) *)
      gbuf#delete_mark errmark#coerce
    in
    mark_remover_id := Some (gbuf#connect#changed ~callback);
  in
  let handle_response response response_start_mark
      src_buf src_start_mark src_end_mark =
    (* returns false on errors, true otherwise *)
    let gbuf = src_buf.Buffer.gbuffer in
    (* todo:
       - split message (val/type/etc | error/warning | prompt)
       - colorise each part
       - for errors/warnings, add the marks to the source buffer
    *)
    let error_regex =
      Str.regexp "^Characters \\([0-9]+\\)-\\([0-9]+\\)"
    in
    let rec find_and_mark_error i =
      let i = Str.search_forward error_regex response i in
      let start_char, end_char =
        int_of_string @@ Str.matched_group 1 response,
        int_of_string @@ Str.matched_group 2 response
      in
      Tools.debug "Parsed error from ocaml: chars %d-%d" start_char end_char;
      (* mark in the ocaml buffer *)
      let top_start = toplevel_buffer#get_iter_at_mark response_start_mark in
      toplevel_buffer#apply_tag Buffer.Tags.ocamltop_err
        ~start:top_start
        ~stop:(toplevel_buffer#get_iter_at_mark ocaml_mark);
      (* mark in the source buffer *)
      let input_start = gbuf#get_iter_at_mark src_start_mark in
      mark_error_in_source_buffer gbuf
        ~start:(input_start#forward_chars start_char)
        ~stop:(input_start#forward_chars end_char);
      try find_and_mark_error (i+1) with Not_found -> false
    in
    try find_and_mark_error 0 with Not_found -> true
  in
  let topeval top =
    let buf = !current_buffer_ref in
    let gbuf = buf.Buffer.gbuffer in
    let rec get_phrases (start:GText.iter) (stop:GText.iter) =
      match start#forward_search ~limit:stop ";;" with
      (* TODO: parse comments/strings to ignore ';;' within strings and
         comments, and check that we don't send an unterminated block to the
         toplevel (after we do that, the only option is to press "Stop") *)
      | Some (a,b) ->
          (gbuf#get_text ~start ~stop:a (),
           Buffer.get_indented_text ~start ~stop:a buf,
           `MARK (gbuf#create_mark start), `MARK (gbuf#create_mark b))
          :: get_phrases b stop
      | None ->
          [gbuf#get_text ~start ~stop (),
           Buffer.get_indented_text ~start ~stop buf,
           `MARK (gbuf#create_mark start), `MARK (gbuf#create_mark stop)]
    in
    let should_update_eval_mark, (start, stop) =
      if gbuf#has_selection then
        false, gbuf#selection_bounds
      else
        true,
        let eval_point = gbuf#get_iter_at_mark buf.Buffer.eval_mark#coerce in
        let point = gbuf#get_iter `INSERT in
        let next_point =
          match
            (point#backward_find_char (fun c ->
                 not (Glib.Unichar.isspace c || c = Glib.Utf8.first_char ";")
               ))
            # forward_search ";;"
          with
          | None -> gbuf#end_iter
          | Some (_,b) -> b
        in
        gbuf#move_mark buf.Buffer.eval_mark_end#coerce ~where:next_point;
        if eval_point#offset < point#offset then
          eval_point, next_point
        else
          let last_point = match point#backward_search ";;" with
            | None -> gbuf#start_iter
            | Some (_,b) -> b
          in
          last_point, next_point
    in
    let cleanup_source_marks phrases =
      List.iter (fun (_,_,start_mark,stop_mark) ->
          gbuf#delete_mark start_mark;
          gbuf#delete_mark stop_mark)
        phrases
    in
    let rec eval_phrases = function
      | [] -> ()
      | (_,_,_,stop_mark) :: _ as phrases
        when should_update_eval_mark &&
             (gbuf#get_iter_at_mark stop_mark)#offset >
             (gbuf#get_iter_at_mark buf.Buffer.eval_mark_end#coerce)#offset
        ->
          (* eval_end has been moved back, meaning the code was just edited *)
          cleanup_source_marks phrases
      | (_,indented,start_mark,stop_mark) :: rest
        when String.trim indented = ""
        ->
          gbuf#delete_mark start_mark;
          gbuf#delete_mark stop_mark;
          eval_phrases rest
      | (phrase,indented,start_mark,stop_mark) :: rest as phrases ->
          display_top_query (String.trim indented);
          replace_marks ();
          let response_start_mark =
            `MARK
              (toplevel_buffer#create_mark
                 (toplevel_buffer#get_iter_at_mark ocaml_mark))
          in
          Top.query top phrase @@ fun response ->
            let success =
              handle_response response response_start_mark
                buf start_mark stop_mark
            in
            toplevel_buffer#delete_mark response_start_mark;
            if (gbuf#get_iter_at_mark stop_mark)#offset >
               (gbuf#get_iter_at_mark buf.Buffer.eval_mark_end#coerce)#offset
            then
              (* stop now if eval_mark_end was rolled back before stop_mark *)
              cleanup_source_marks phrases
            else
              (if should_update_eval_mark then
                 if success then
                   gbuf#move_mark buf.Buffer.eval_mark#coerce ~where:
                     (gbuf#get_iter_at_mark stop_mark)
                 else
                   (let where = gbuf#get_iter_at_mark start_mark in
                    gbuf#move_mark buf.Buffer.eval_mark#coerce ~where;
                    gbuf#move_mark buf.Buffer.eval_mark_end#coerce ~where);
               gbuf#delete_mark start_mark;
               gbuf#delete_mark stop_mark;
               if success then eval_phrases rest
               else cleanup_source_marks rest)
    in
    let phrases = get_phrases start stop in
    eval_phrases phrases
  in
  let top_ref = ref None in
  let rec top_start () =
    let schedule f = GMain.Idle.add @@ fun () ->
        try f (); false with
          e ->
            Printf.eprintf "Error in toplevel interaction: %s%!"
              (Tools.printexc e);
            raise e
    in
    let resp_handler = function
      | Top.Message m -> display_top_response m
      | Top.User u -> display_stdout u
      | Top.Exited ->
          let buf = !current_buffer_ref in
          buf.Buffer.gbuffer#move_mark
            buf.Buffer.eval_mark#coerce ~where:buf.Buffer.gbuffer#start_iter;
          toplevel_buffer#insert
            ~tags:[Buffer.Tags.ocamltop_warn]
            ~iter:toplevel_buffer#end_iter
            "\t\t*** restarting ocaml ***\n";
    in
    replace_marks ();
    let buf = !current_buffer_ref in
    buf.Buffer.gbuffer#move_mark
      buf.Buffer.eval_mark#coerce ~where:buf.Buffer.gbuffer#start_iter;
    Top.start schedule resp_handler status_change_hook
  and status_change_hook = function
    | Top.Dead ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.disable `STOP;
        top_ref := Some (top_start ())
    | Top.Ready ->
        Gui.Controls.enable `EXECUTE;
        Gui.Controls.disable `STOP
    | Top.Busy _ ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.enable `STOP
  in
  Gui.Controls.bind `EXECUTE (fun () ->
    topeval @@ match !top_ref with
      | Some top -> top
      | None -> let top = top_start () in
          top_ref := Some top; top);
  Gui.Controls.bind `STOP (fun () ->
    let buf = !current_buffer_ref in
    buf.Buffer.gbuffer#move_mark buf.Buffer.eval_mark_end#coerce
      ~where:(buf.Buffer.gbuffer#get_iter_at_mark buf.Buffer.eval_mark#coerce);
    match !top_ref with
    | Some top -> Top.stop top
    | None -> ());
  Gui.Controls.bind `RESTART (fun () ->
      toplevel_buffer#delete
        ~start:toplevel_buffer#start_iter
        ~stop:toplevel_buffer#end_iter;
      match !top_ref with
      | Some top ->
          Top.kill top
      | None -> ());
  Gui.Controls.bind `CLEAR (fun () ->
    toplevel_buffer#delete
      ~start:toplevel_buffer#start_iter
      ~stop:toplevel_buffer#end_iter);
  top_ref := Some (top_start ())
