open Printf
open Ezjsonm

type event = {
  time : float;
  kind : string;
  body : string;
}

let json_of_event evt =
  `O [("time", `Float evt.time)
     ;("kind", `String evt.kind)
     ;("body", `String evt.body)]

let traceFile = Filename.concat (Sys.getenv "HOME") ".ocaml-top-trace"

let trace evt body =
  let tm = Unix.gettimeofday () in
  let s  = to_string (json_of_event { time=tm; kind=evt; body=body }) in
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o640 traceFile in
  fprintf oc "%s\n" s;
  close_out oc;
