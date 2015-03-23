(* build with
ocamlbuild -cflags -thread -cflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml -cflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml/4.01.0/site-lib/lablgtk2 -cflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml/4.01.0/site-lib/ocp-indent/lib/ -cflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml/4.01.0/site-lib/ocp-indent/utils/ -cflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml/4.01.0/site-lib/ezjsonm/ -lflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml -lflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml/4.01.0/site-lib/lablgtk2 -lflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml/4.01.0/site-lib/ocp-indent/lib -lflags -I,/Users/gridaphobe/.nix-profile/lib/ocaml/4.01.0/site-lib/ocp-indent/utils -lflags -thread -libs unix,str,threads,lablgtk,ocp-indent.lib,ocp-indent.utils,pos get_phrases.native
 *)

module OBuf = OcamlBuffer

let fst4 (a,_,_,_) = a

let read_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let iter_of_offset buf off =
  buf.OBuf.gbuffer#get_iter (`OFFSET off)

let _ =
  let start = int_of_string Sys.argv.(1) in
  (* print_endline (string_of_int start); *)
  let stop  = int_of_string Sys.argv.(2) in
  (* print_endline (string_of_int stop); *)
  let body  = read_file Sys.argv.(3) in
  (* print_endline body; *)
  (* print_endline "-- done args --"; *)
  let buf   = OBuf.create ~name:"tmp" ~contents:body () in
  OBuf.setup_indent buf;
  let phrases = (TopUi.get_phrase_strs
                   buf
                   (iter_of_offset buf start)
                   (iter_of_offset buf stop)) in
  (* print_endline (List.hd phrases) *)
  List.map (fun phr -> print_string phr; print_endline ";;") phrases
