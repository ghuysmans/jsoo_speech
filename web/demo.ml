open Js_of_ocaml
open Lwt.Infix

let () =
  Lwt.async (fun () -> 
    Speech.voices >>= function
    | [| |] -> failwith "no voice"
    | a ->
      (* a |> Array.iter (fun v -> print_endline (Js.to_string v##.name)); *)
      let l = a.(0) in
      Printf.printf "voice: %s\n" (Js.to_string l##.name);
      Speech.await @@ Speech.utterance "hello world" >>= fun dt ->
      Printf.printf "elapsed: %f\n" dt;
      Lwt.return_unit
  )
