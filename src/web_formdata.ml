class type _formdata = object
  method append : (string -> string -> unit[@mel.meth])
  (* method append_blob : string -> Web_blob.t -> string -> unit *)
end[@bs]

type t = _formdata Js.t

external create : unit -> t = "FormData" [@@mel.new]

let append key value f = f##append key value
