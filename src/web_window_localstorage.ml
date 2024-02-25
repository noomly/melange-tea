type t =
  < length : int [@mel.get]
  ; clear : unit -> unit [@mel.meth]
  ; key : int -> string [@mel.meth]
  ; getItem : string -> string [@mel.meth]
  ; removeItem : string -> unit [@mel.meth]
  ; setItem : string -> string -> unit [@mel.meth] >
  Js.t

let length window =
  match Js.Undefined.toOption window##localStorage with
  | None -> None
  | Some localStorage -> Some localStorage##length

let clear window =
  match Js.Undefined.toOption window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##clear ())

let key window idx =
  match Js.Undefined.toOption window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##key idx)

let getItem window key =
  match Js.Undefined.toOption window##localStorage with
  | None -> None
  | Some localStorage -> (
      try Some (localStorage##getItem key) with _ -> None)

let removeItem window key =
  match Js.Undefined.toOption window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##removeItem key)

let setItem window key value =
  match Js.Undefined.toOption window##localStorage with
  | None -> None
  | Some localStorage -> Some (localStorage##setItem key value)
