let focus id =
  Tea_cmd.call (fun _enqueue ->
      let ecb _ =
        match
          Webapi.Dom.Document.getElementById id Webapi.Dom.document
          |. Option.bind Webapi.Dom.HtmlElement.ofElement
        with
        | None -> Js.log ("Attempted to focus a non-existant element of: ", id)
        (* | Some elem -> Web.Node.focus elem *)
        | Some elem -> Webapi.Dom.HtmlElement.focus elem
      in
      (* One to get out of the current render frame*)
      let cb _ = ignore (Web.Window.requestAnimationFrame ecb) in
      (* And another to properly focus *)
      ignore (Web.Window.requestAnimationFrame cb);
      ())
