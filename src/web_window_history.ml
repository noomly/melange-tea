

type t = <
  length : int [@mel.get];
  back : unit -> unit [@mel.meth];
  forward : unit -> unit [@mel.meth];
  go : int -> unit [@mel.meth];
  pushState : Js.Json.t -> string -> string -> unit [@mel.meth];
  replaceState : Js.Json.t -> string -> string -> unit [@mel.meth];
  state : Js.Json.t [@mel.get];
> Js.t


let length window = match Js.Undefined.toOption window##history with
  | None -> -1
  | Some history -> history##length

let back window = match Js.Undefined.toOption window##history with
  | None -> ()
  | Some history -> history##back

let forward window = match Js.Undefined.toOption window##history with
  | None -> ()
  | Some history -> history##forward

let go window to' = match Js.Undefined.toOption window##history with
  | None -> ()
  | Some history -> history##go to'

let pushState window state title url = match Js.Undefined.toOption window##history with
  | None -> ()
  | Some history -> history##pushState state title url

let replaceState window state title url = match Js.Undefined.toOption window##history with
  | None -> ()
  | Some history -> history##replaceState state title url

let state window = match Js.Undefined.toOption window##history with
  | None -> Js.Undefined.empty
  | Some history -> history##state
