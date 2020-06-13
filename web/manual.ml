open Js_of_ocaml

let () =
  let u = new%js Speech.utterance_fromString (Js.string "hello world") in
  Speech.Unsafe.synthesis##speak u
