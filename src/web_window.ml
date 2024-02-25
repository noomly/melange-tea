(* TODO:  Polyfill window if it is missing, like on node or in native *)

module History = Web_window_history
module LocalStorage = Web_window_localstorage

type timeoutHandlerID = int

type t =
  < history : History.t Js.Undefined.t [@mel.get]
  ; location : Web_location.t [@mel.get]
  ; clearTimeout : timeoutHandlerID -> unit [@mel.meth]
  ; requestAnimationFrame : (float -> unit) -> int [@mel.meth]
  ; cancelAnimationFrame : int -> unit [@mel.meth]
  ; setInterval : (unit -> unit) -> float -> timeoutHandlerID [@mel.meth]
  ; setTimeout : (unit -> unit) -> float -> timeoutHandlerID [@mel.meth]
  ; addEventListener :
      string -> Web_node.t Web_event.cb -> Web_event.options -> unit
        [@mel.meth]
  ; removeEventListener :
      string -> Web_node.t Web_event.cb -> Web_event.options -> unit
        [@mel.meth]
  ; localStorage : LocalStorage.t Js.Undefined.t [@mel.get] >
  Js.t

external window : t = "window" [@@mel.val]

let history () = window##history
let localStorage () = window##localStorage
let location () = window##location

(* requestAnimationFrame callback is a float timestamp in milliseconds *)
let requestAnimationFrame callback = window##requestAnimationFrame callback
let cancelAnimationFrame id = window##cancelAnimationFrame id
let clearTimeout id = window##clearTimeout id
let setInterval cb msTime = window##setInterval cb msTime
let setTimeout cb msTime = window##setTimeout cb msTime

let addEventListener typ listener options =
  window##addEventListener typ listener options

let removeEventListener typ listener options =
  window##removeEventListener typ listener options

(* Polyfills *)

let requestAnimationFrame_polyfill : unit -> unit =
 fun () ->
  [%mel.raw
    {|
  // requestAnimationFrame polyfill
  (function() {
      var lastTime = 0;
      var vendors = ['ms', 'moz', 'webkit', 'o'];
      for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
          window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
          window.cancelAnimationFrame = window[vendors[x]+'CancelAnimationFrame']
                                     || window[vendors[x]+'CancelRequestAnimationFrame'];
      }

      if (!window.requestAnimationFrame)
          window.requestAnimationFrame = function(callback, element) {
              var currTime = new Date().getTime();
              var timeToCall = Math.max(0, 16 - (currTime - lastTime));
              var id = window.setTimeout(function() { callback(currTime + timeToCall); },
                timeToCall);
              lastTime = currTime + timeToCall;
              return id;
          };

      if (!window.cancelAnimationFrame)
          window.cancelAnimationFrame = function(id) {
              clearTimeout(id);
          };
  }())
  |}]
