

type t = <
> Js.t


type date_obj = <
  now : unit -> float [@mel.meth];
> Js.t


external create_date : unit -> t = "Date" [@@mel.new]

external date_obj : date_obj = "Date" [@@mel.val]


let now () = date_obj##now ()
