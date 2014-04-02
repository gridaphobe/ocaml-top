(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Tools.Ops
module OBuf = OcamlBuffer

let _ = GtkMain.Main.init()

module Controls = struct
  type t = [ `NEW | `OPEN | `SAVE | `SAVE_AS | `SUBMIT
           | `EXECUTE | `EXECUTE_ALL | `STOP | `RESTART | `CLEAR | `CONFUSED
           | `SELECT_FONT | `SELECT_COLOR | `ZOOM_IN | `ZOOM_OUT | `FULLSCREEN
           | `QUIT ]

  let stock (*: t -> GtkStock.id*) = function
    | `RESTART -> `REFRESH
    | `EXECUTE_ALL -> `MEDIA_FORWARD
    | `CONFUSED -> `DIALOG_QUESTION
    | `SUBMIT -> `OK
    | #GtkStock.id as id -> id

  let icon t =
    let name = match t with
      | `NEW -> "new"
      | `OPEN -> "open"
      | `SAVE -> "save"
      | `SAVE_AS -> "save-as"
      | `EXECUTE -> "execute"
      | `EXECUTE_ALL -> "execute-all"
      | `STOP -> "stop"
      | `RESTART -> "restart"
      | `CLEAR -> "clear"
      | `CONFUSED -> "confused"
      | `SUBMIT -> "submit"
      | `SELECT_FONT -> "setup"
      | `SELECT_COLOR -> "setup"
      | `ZOOM_IN -> "zoom-in"
      | `ZOOM_OUT -> "zoom-out"
      | `FULLSCREEN -> "setup"
      | `QUIT -> "quit"
    in
    let file =
      let (/) = Filename.concat in
      !Cfg.datadir / "icons" / name ^ ".png"
    in
    let pixbuf = GdkPixbuf.from_file_at_size file ~width:22 ~height:22 in
    let img = GMisc.image ~pixbuf () in
    img

  let to_string command = GtkStock.convert_id (stock command)

  let help: t -> string * string = function
    | `NEW -> "New","Create a new file [Ctrl n]"
    | `OPEN -> "Open...","Select an existing file to edit [Ctrl o]"
    | `SAVE -> "Save","Save the current file [Ctrl s]"
    | `SAVE_AS -> "Save as...","Select a file to save the current program to"
    | `EXECUTE -> "Run","Run the current program up to the cursor, \
                         or the selection if any [Ctrl e]"
    | `EXECUTE_ALL -> "Run to end",
                      "Run the current program as far as possible"
    | `STOP -> "Stop","Stop ongoing program execution [Esc]"
    | `RESTART -> "Restart","Terminate the current toplevel and start a new one"
    | `CLEAR -> "Clear","Clear the toplevel window history"
    | `CONFUSED -> "Confused","I am confused about something!"
    | `SUBMIT -> "Submit","Submit this file for grading"
    | `SELECT_FONT -> "Font...","Change the display font"
    | `SELECT_COLOR -> "Color theme","Switch color theme"
    | `ZOOM_IN -> "Zoom in","Make the font bigger [Ctrl +]"
    | `ZOOM_OUT -> "Zoom out","Make the font smaller [Ctrl -]"
    | `FULLSCREEN -> "Fullscreen","Switch fullscreen mode"
    | `QUIT -> "Quit","Quit ocaml-top [Ctrl q]"

  (* We could use lablgtk's action groups as well. But better map from an open
     variant than from strings... *)
  let signal =
    let controls : (t,GAction.action) Hashtbl.t = Hashtbl.create 17
    in fun t ->
      try Hashtbl.find controls t
      with Not_found ->
          let c = GAction.action ~name:(to_string t) () in
          c#set_stock_id (stock t);
          Hashtbl.add controls t c;
          c

  let bind command action =
    let action x =
      try action x with e ->
          Tools.debug "Exception during action of command %s: %s\n%s"
            (to_string command)
            (Printexc.to_string e) (Printexc.get_backtrace ())
    in
    ignore @@ (signal command)#connect#activate ~callback:action

  let trigger command =
    Tools.debug "Event triggered: %s" @@ to_string command;
    ignore @@ (signal command)#activate ()

  let add_trigger command widget =
    (signal command)#connect_proxy (widget :> GObj.widget)

  let enable command =
    (signal command)#set_sensitive true

  let disable command =
    (signal command)#set_sensitive false

end

(* use `ALWAYS for vertical scrollbars, otherwise it is possible to trigger a
   bug in GTK that locks the mouse when resizing the panes *)
let main_view =
  GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`NEVER ()

let toplevel_view =
  GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`AUTOMATIC ()

let status_bar, top_msg, index_msg =
  let bar = GMisc.statusbar ~has_resize_grip:false () in
  let ctx_top = bar#new_context ~name:"top" in
  let ctx_index = bar#new_context ~name:"index" in
  bar,
  (fun msg ->
     ctx_top#pop ();
     if msg <> "" then ignore @@ ctx_top#push msg),
  (fun msg ->
     ctx_index#pop ();
     if msg <> "" then ignore @@ ctx_index#push msg)


type shortcut_mods = [ `CONTROL | `SHIFT | `META | `SUPER | `HYPER ]
let shortcuts = [
  ([`CONTROL], GdkKeysyms._n),      `NEW;
  ([`CONTROL], GdkKeysyms._o),      `OPEN;
  ([`CONTROL], GdkKeysyms._s),      `SAVE;
  ([`CONTROL], GdkKeysyms._e),      `EXECUTE;
  ([],         GdkKeysyms._Escape), `STOP;
  ([`CONTROL], GdkKeysyms._plus),   `ZOOM_IN;
  ([`CONTROL], GdkKeysyms._equal),  `ZOOM_IN;
  ([`CONTROL], GdkKeysyms._KP_Add), `ZOOM_IN;
  ([`CONTROL], GdkKeysyms._minus),  `ZOOM_OUT;
  ([`CONTROL], GdkKeysyms._KP_Subtract), `ZOOM_OUT;
  ([],         GdkKeysyms._F4),     `SELECT_FONT;
  ([],         GdkKeysyms._F5),     `SELECT_COLOR;
  ([],         GdkKeysyms._F11),    `FULLSCREEN;
  ([`CONTROL], GdkKeysyms._q),      `QUIT;
  ([`META],    GdkKeysyms._F4),     `QUIT;
]

let add objs container =
  List.iter (fun o -> container#add (o :> GObj.widget)) objs;
  container

let pack objs (container: GPack.box) =
  List.iter (fun o -> container#pack o) objs;
  container

let as_widget o = (o :> GObj.widget)

let main_window () =
  let logo = GdkPixbuf.from_file (Filename.concat !Cfg.datadir "logo.png") in
  let tooltips = GData.tooltips () in
  let mkbutton ctrl =
    let label,text = Controls.help ctrl in
    let btn = GButton.tool_button ~stock:(Controls.stock ctrl) ~label () in
    Controls.add_trigger ctrl btn;
    btn#set_label label;
    btn#set_icon_widget (Controls.icon ctrl :> GObj.widget);
    tooltips#set_tip ~text (btn :> GObj.widget);
    (* ignore @@ btn#connect#clicked ~callback:(fun () -> Controls.trigger ctrl); *)
    (btn :> GObj.widget)
  in
  let win =
    GWindow.window
      ~title:("ocaml-top "^Cfg.version)
      ~height:600 ~allow_shrink:true (* ~width:800 ~show:true *)
      ~icon:logo
      ()
    |> add [
      GPack.vbox ()
      |> pack [
        GButton.toolbar ~style:`ICONS ()
        |> add [
          mkbutton `NEW;
          mkbutton `OPEN;
          mkbutton `SAVE;
          mkbutton `SAVE_AS;
          mkbutton `SUBMIT;
          (GButton.separator_tool_item () :> GObj.widget);
          mkbutton `EXECUTE;
          mkbutton `STOP;
          mkbutton `RESTART;
          mkbutton `EXECUTE_ALL;
          (* mkbutton `CLEAR; *)
          mkbutton `CONFUSED;
          (GButton.tool_item ~expand:true () :> GObj.widget);
          mkbutton `QUIT;
        ]
        |> as_widget;
      ]
      |> add [
        GPack.paned `HORIZONTAL ()
        |> add [
          main_view;
          toplevel_view;
        ];
      ]
    |> pack [ status_bar |> as_widget ];
    ]
  in
  ignore @@ win#event#connect#delete
    ~callback:(fun _ -> Controls.trigger `QUIT; true);
  ignore @@ win#connect#destroy ~callback:GMain.quit;
  ignore @@ win#event#connect#key_press ~callback:(fun ev ->
    let state = GdkEvent.Key.state ev
      |> List.filter (function #shortcut_mods -> true | _ -> false)
    in
    let keyval = GdkEvent.Key.keyval ev in
    match
      List.filter
        (fun ((st,kv),_) ->
           keyval = kv && List.for_all (fun k -> List.mem k state) st)
        shortcuts
    with
    | (_,action)::_ -> Controls.trigger action; false
    | [] -> false);
  win

let set_window_title window fmt =
  Printf.ksprintf
    (fun s -> window#set_title (s ^ " - ocaml-top " ^ Cfg.version))
    fmt

let open_text_view buffer =
  Tools.debug "open text view";
  let view =
    GSourceView2.source_view
      ~source_buffer:buffer
      ~auto_indent:true
      ~highlight_current_line:true
      ~indent_on_tab:false
      ~indent_width:2
      ~accepts_tab:false
      ~wrap_mode:`CHAR
      ~show_right_margin:false
      ~show_line_marks:true
      ~show_line_numbers:false
      ()
  in
  List.iter main_view#remove main_view#children;
  let _set_mark_categories =
    let (/) = Filename.concat in
    let icon name = GdkPixbuf.from_file (!Cfg.datadir/"icons"/name^".png") in
    view#set_mark_category_pixbuf ~category:"block_mark"
      (Some (icon "block_marker"));
    view#set_mark_category_pixbuf ~category:"end_block_mark"
      (if Tools.debug_enabled then Some (icon "end_block_marker")
       else None);
    view#set_mark_category_pixbuf ~category:"eval_next"
      (if Tools.debug_enabled then Some (icon "eval_marker_next")
       else None);
    view#set_mark_category_pixbuf ~category:"eval"
      (Some (icon "eval_marker"));
    view#set_mark_category_pixbuf ~category:"error"
      (Some (icon "err_marker"));
    view#set_mark_category_priority ~category:"end_block_mark" 0;
    view#set_mark_category_priority ~category:"block_mark" 1;
    view#set_mark_category_priority ~category:"eval_next" 3;
    view#set_mark_category_priority ~category:"eval" 4;
    view#set_mark_category_priority ~category:"error" 5;
  in
  view#misc#modify_font_by_name !Cfg.font;
  main_view#add (view :> GObj.widget);
  view#misc#set_size_request ~width:(!Cfg.char_width * 83) ();
  view#misc#grab_focus ();
  view

let open_toplevel_view top_buf =
  Tools.debug "open top view";
  let view =
    GSourceView2.source_view
      ~source_buffer:top_buf
      ~auto_indent:false
      ~highlight_current_line:false
      ~indent_on_tab:false
      ~indent_width:2
      ~accepts_tab:false
      ~wrap_mode:`NONE
      ~cursor_visible:false
      ~editable:false
      ()
  in
  view#misc#modify_font_by_name !Cfg.font;
  toplevel_view#add (view :> GObj.widget);
  view#misc#set_size_request ~width:578 ();
  view#misc#set_can_focus false;
  view

let set_font
    (src_view:GSourceView2.source_view)
    (top_view:GSourceView2.source_view)
    str
  =
  let font = new GPango.font_description (GPango.font_description str) in
  Cfg.font := font#to_string;
  src_view#misc#modify_font font#fd;
  top_view#misc#modify_font font#fd;
  Cfg.char_width :=
    GPango.to_pixels
      (src_view#misc#pango_context#get_metrics ())#approx_char_width;
  main_view#misc#set_size_request ~width:(!Cfg.char_width * 82 + 30) ();
  Tools.debug "Font set: %S (char width: %d)"
    font#to_string !Cfg.char_width

let switch_fullscreen =
  let full = ref false in
  fun window ->
    if !full then window#unfullscreen ()
    else window#fullscreen ();
    full := not !full

module Dialogs = struct

  (* Return type of a function that would return 'a, but is in CPS form *)
  type 'a cps = ('a -> unit) -> unit

  let choose_file action ?(cancel = fun () -> ()) k =
    let title, button_label = match action with
      | `OPEN -> "Please choose file to load", "Load"
      | `SAVE -> "Please choose file to save to", "Save"
    in
    let dialog =
      GWindow.file_chooser_dialog
        ~title
        ~action:(action :> GtkEnums.file_chooser_action) ()
    in
    let callback x =
      match x with
      | `CANCEL | `DELETE_EVENT -> dialog#destroy (); cancel ()
      | `APPLY ->
        match dialog#filename with
        | None -> dialog#destroy (); cancel ()
        | Some name -> dialog#destroy (); name |> k
    in
    dialog#add_filter @@
      GFile.filter ~name:"OCaml source (*.ml)" ~patterns:["*.ml";"*.ml?"] ();
    dialog#add_filter @@
      GFile.filter ~name:"All files" ~patterns:["*"] ();
    dialog#add_select_button_stock (action :> GtkStock.id) `APPLY;
    dialog#add_button_stock `CANCEL `CANCEL;
    ignore @@ dialog#connect#response ~callback;
    dialog#show ()

  let confused buf =
    let dialog =
      GWindow.message_dialog
        ~title:"Confused"
        ~message:"Please explain why you are confused."
        ~message_type:`QUESTION
        ~use_markup:true
        ~buttons:GWindow.Buttons.ok_cancel
        ~destroy_with_parent:true
        ()
    in
    let entry =
      GText.view
        ~editable:true
        ~border_width:2
        (* ~editable:true *)
        (* ~show:true *)
        ()
    in
    let callback = function
      | `CANCEL | `DELETE_EVENT -> dialog#destroy ()
      | `OK -> Trace.trace (Trace.Confused (entry#buffer#get_text ())) buf;
              dialog#destroy ()
    in
    dialog#vbox#add (entry :> GObj.widget);
    ignore @@ dialog#connect#response ~callback;
    dialog#show ()

  let submit buf ~save =
    save ();
    Trace.trace Trace.Submit buf;
    let cmd = Format.sprintf "turnin -c cs130s %s 2>&1" (OBuf.filename_default buf) in
    let ic, oc = Unix.open_process cmd in
    let buf = Buffer.create 16 in
    (try
        while true do
          Buffer.add_channel buf ic 1
        done
      with End_of_file -> ());
    let _ = Unix.close_process (ic, oc) in
    let out = Buffer.contents buf
    in
    let dialog =
      GWindow.message_dialog
        ~title:"Turnin"
        ~message:out
        ~message_type:`INFO
        ~buttons:GWindow.Buttons.ok
        ~use_markup:true
        ~destroy_with_parent:true
        ()
    in
    let callback = function
      | `OK | `DELETE_EVENT -> dialog#destroy ()
    in
    ignore @@ dialog#connect#response ~callback;
    dialog#show ()

  let error ~title message =
    let dialog = GWindow.message_dialog
      ~title
      ~message
      ~use_markup:true
      ~message_type:`ERROR
      ~buttons:GWindow.Buttons.close
      ()
    in
    (* ignore @@ dialog#connect#response; *)
    ignore @@ dialog#run ();
    dialog#destroy ()

  let quit filename ~save ~quit =
    let filename = match filename with
      | Some f -> Printf.sprintf "File %S" f
      | None -> "Current buffer"
    in
    let dialog = GWindow.dialog ~title:"Quit" () in
    let txt =
      let frame = GBin.frame ~border_width:40 ~shadow_type:`NONE () in
      frame#add
        (GMisc.label
           ~markup:(filename^" contains unsaved changes. What to do ?") ()
         :> GObj.widget);
      (frame :> GObj.widget)
    in
    dialog#vbox#add txt;
    dialog#add_button_stock `SAVE `SAVE;
    dialog#add_button_stock `QUIT `QUIT;
    dialog#add_button_stock `CANCEL `CANCEL;
    ignore @@ dialog#connect#response ~callback:(function
      | `SAVE -> dialog#destroy (); save () @@ quit
      | `QUIT -> dialog#destroy (); quit ()
      | `CANCEL | `DELETE_EVENT -> dialog#destroy ());
    dialog#show ()

  let confirm ~title message ?(no = fun () -> ()) k =
    let dialog = GWindow.message_dialog
      ~title
      ~message
      ~use_markup:true
      ~message_type:`QUESTION
      ~buttons:GWindow.Buttons.yes_no
      ()
    in
    ignore @@ dialog#connect#response ~callback:(function
        | `YES -> dialog#destroy () |> k
        | `NO | `DELETE_EVENT -> dialog#destroy () |> no);
    dialog#show ()

  let choose_font
      (src_view:GSourceView2.source_view) (top_view:GSourceView2.source_view)
      ~on_font_change
      ()
    =
    let dialog = GWindow.font_selection_dialog () in
    dialog#selection#set_font_name !Cfg.font;
    ignore @@ dialog#connect#response ~callback:(function
      | `APPLY ->
          set_font src_view top_view dialog#selection#font_name;
          on_font_change ()
      | `OK ->
          set_font src_view top_view dialog#selection#font_name;
          dialog#destroy ();
          on_font_change ()
      | `CANCEL | `DELETE_EVENT -> dialog#destroy ());
    dialog#show ()
end
