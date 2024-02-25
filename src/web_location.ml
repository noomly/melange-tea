

type t = <
  href : string [@mel.get] [@mel.set];
  protocol : string [@mel.get] [@mel.set];
  host : string [@mel.get] [@mel.set];
  hostname : string [@mel.get] [@mel.set];
  port : string [@mel.get] [@mel.set];
  pathname : string [@mel.get] [@mel.set];
  search : string [@mel.get] [@mel.set];
  hash : string [@mel.get] [@mel.set];
  username : string [@mel.get] [@mel.set];
  password : string [@mel.get] [@mel.set];
  origin : string [@mel.get];
> Js.t

let getHref location = location##href
let setHref location value = location##href #= value

let getProtocol location = location##protocol
let setProtocol location value = location##protocol #= value

let getHost location = location##host
let setHost location value = location##host #= value

let getHostname location = location##hostname
let setHostname location value = location##hostname #= value

let getPort location = location##port
let setPort location value = location##port #= value

let getPathname location = location##pathname
let setPathname location value = location##pathname #= value

let getSearch location = location##search
let setSearch location value = location##search #= value

let getHash location = location##hash
let setHash location value = location##hash #= value

let getUsername location = location##username
let setUsername location value = location##username #= value

let getPassword location = location##password
let setPassword location value = location##password #= value

let getOrigin location = location##origin


type location =
  { href : string
  ; protocol : string
  ; host : string
  ; hostname : string
  ; port : string
  ; pathname : string
  ; search : string
  ; hash : string
  ; username : string
  ; password : string
  ; origin : string
  }

let asRecord location =
  { href = location##href
  ; protocol = location##protocol
  ; host = location##host
  ; hostname = location##hostname
  ; port = location##port
  ; pathname = location##pathname
  ; search = location##search
  ; hash = location##hash
  ; username = location##username
  ; password = location##password
  ; origin = location##origin
  }
