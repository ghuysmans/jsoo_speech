let () =
  let u = Speech.utterance "hello world" in
  Speech.engine##speak u
