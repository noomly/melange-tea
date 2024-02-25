type style =
  < setProperty : Web_json.t Js.undefined
        [@mel.get] (* TODO:  Revamp this and the next line... *)
  ; setProperty__ : string -> string Js.null -> string Js.null -> unit
        [@mel.meth] >
  Js.t

external getStyle : style -> string -> string Js.null = "" [@@mel.get_index]

external setStyle : style -> string -> string Js.null -> unit = ""
[@@mel.set_index]

type t =
  < style : style [@mel.get]
  ; value : string Js.undefined [@mel.set] [@mel.get]
  ; checked : bool Js.undefined [@mel.set] [@mel.get]
  ; childNodes : t Js.Array.t [@mel.get]
  ; firstChild : t Js.Null.t [@mel.get]
  ; appendChild : t -> t [@mel.meth]
  ; removeChild : t -> t [@mel.meth]
  ; insertBefore : t -> t -> t [@mel.meth]
  ; remove : unit -> unit [@mel.meth]
  ; setAttributeNS : string -> string -> string -> unit [@mel.meth]
  ; setAttribute : string -> string -> unit [@mel.meth]
  ; removeAttributeNS : string -> string -> unit [@mel.meth]
  ; removeAttribute : string -> unit [@mel.meth]
  ; addEventListener : string -> t Web_event.cb -> Web_event.options -> unit
        [@mel.meth]
  ; removeEventListener : string -> t Web_event.cb -> Web_event.options -> unit
        [@mel.meth]
  ; focus : unit -> unit [@mel.meth]
  ; (* Text Nodes only *)
  nodeValue : string [@mel.set] [@mel.get { null }] >
  Js.t

external document_node : t = "document" [@@mel.val]

type event = t Web_event.t
type event_cb = t Web_event.cb

external getProp_asEventListener : t -> 'key -> t Web_event.cb Js.undefined = ""
[@@mel.get_index]

external setProp_asEventListener :
  t -> 'key -> t Web_event.cb Js.undefined -> unit = ""
[@@mel.set_index]

external getProp : t -> 'key -> 'value = "" [@@mel.get_index]
external setProp : t -> 'key -> 'value -> unit = "" [@@mel.set_index]

let style n = n##style
let getStyle n key = getStyle n##style key
let setStyle n key value = setStyle n##style key value

let setStyleProperty n ?(priority = false) key value =
  let style = n##style in
  match Js.Undefined.toOption style##setProperty with
  | None ->
      setStyle n key
        value (* TODO:  Change this to setAttribute sometime, maybe... *)
  | Some _valid ->
      style##setProperty__ key value
        (if priority then Js.Null.return "important" else Js.Null.empty)

let childNodes n = n##childNodes
let firstChild n = n##firstChild
let appendChild n child = n##appendChild child
let removeChild n child = n##removeChild child
let insertBefore n child refNode = n##insertBefore child refNode
let remove n child = n##remove child
let setAttributeNS n namespace key value = n##setAttributeNS namespace key value
let setAttribute n key value = n##setAttribute key value

let setAttributeNsOptional n namespace key value =
  match namespace with
  | "" -> n##setAttribute key value
  | ns -> n##setAttributeNS ns key value

let removeAttributeNS n namespace key = n##removeAttributeNS namespace key
let removeAttribute n key = n##removeAttribute key

let removeAttributeNsOptional n namespace key =
  match namespace with
  | "" -> n##removeAttribute key
  | ns -> n##removeAttributeNS ns key

let addEventListener n typ listener options =
  n##addEventListener typ listener options

let removeEventListener n typ listener options =
  n##removeEventListener typ listener options

let focus n = n##focus ()

(* Text Nodes only *)

let set_nodeValue n text = n ## nodeValue #= text
let get_nodeValue n = n##nodeValue

(* Polyfills *)

let remove_polyfill : unit -> unit =
 fun () ->
  [%mel.raw
    {|
  // remove polyfill
  (function() {
    if (!('remove' in Element.prototype)) {
      Element.prototype.remove = function() {
        if (this.parentNode) {
          this.parentNode.removeChild(this);
        }
      };
    };
  }())
  |}]
