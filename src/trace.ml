open Printf
open Ezjsonm
module OBuf = OcamlBuffer

type region = GText.iter * GText.iter

type kind =
  | Eval of region
  | Abort
  | Restart
  | Timer
  | Confused of string

type event = {
  time : float;
  file : string;
  kind : kind;
  body : string;
  cursor : GText.iter;
}

(* let json_of_iter i = `A [`Float (float_of_int i#line) *)
(*                         ;`Float (float_of_int i#line_offset)] *)
let json_of_iter i = `Float (float_of_int i#offset)

let json_of_region (start, stop) = `O [("start", json_of_iter start)
                                      ;("stop", json_of_iter stop)]

let json_of_kind = function
  | Eval r  -> `O [("type", `String "eval"); ("region", json_of_region r)]
  | Abort   -> `O [("type", `String "abort")]
  | Restart -> `O [("type", `String "restart")]
  | Timer   -> `O [("type", `String "timer")]
  | Confused s -> `O [("type", `String "confused"); ("why", `String s)]

let json_of_event evt =
  `O [("time", `Float evt.time)
     ;("file", `String evt.file)
     ;("event", json_of_kind evt.kind)
     ;("body", `String evt.body)
     ;("cursor", json_of_iter evt.cursor)]

let traceFile = Filename.concat (Sys.getenv "HOME") ".ocaml-top-trace"

let trace evt buf =
  let tm = Unix.gettimeofday () in
  let bd = OBuf.contents buf in
  let c  = buf.OBuf.gbuffer#get_iter_at_mark `INSERT in
  let f  = OBuf.filename_default buf in
  let s  = to_string (json_of_event { time=tm; file=f; kind=evt; body=bd; cursor=c }) in
  Tools.debug "%s\n" s;
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 traceFile in
  fprintf oc "%s\n" s;
  close_out oc;
