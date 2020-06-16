(* TODO add documentation from https://wicg.github.io/speech-api/ *)
open Js_of_ocaml

class type voice = object
  method default: bool Js.t Js.readonly_prop
  method lang: Js.js_string Js.t Js.readonly_prop
  method localService: bool Js.t Js.readonly_prop
  method name: Js.js_string Js.t Js.readonly_prop
  method voiceURI: Js.js_string Js.t Js.readonly_prop
end

class type utterance = object
  method lang: Js.js_string Js.t Js.prop (* FIXME required? *)
  method pitch: float Js.prop
  method rate: float Js.prop
  method text: Js.js_string Js.t Js.prop (* TODO handle SSML *)
  method voice: voice Js.t Js.prop (* FIXME optional? *)
  method volume: float Js.prop
end

let utterance : (Js.js_string Js.t -> utterance Js.t) Js.constr =
  Js.Unsafe.global##.SpeechSynthesisUtterance


class type synthesis = object
  method paused: bool Js.readonly_prop
  method pending: bool Js.readonly_prop
  method speaking: bool Js.readonly_prop
  method cancel: unit Js.meth
  method getVoices: voice Js.t Js.js_array Js.t Js.meth (* FIXME sequence? *)
  method pause: unit Js.meth
  method resume: unit Js.meth
  method speak: utterance Js.t -> unit Js.meth
end

let engine : synthesis Js.t = Js.Unsafe.global##.window##.speechSynthesis

module Event = struct
  class type event = object
    (* FIXME can I even do that? I think there's a target, etc. *)
    inherit [event] Dom.event
    method utterance: utterance Js.t Js.readonly_prop
    method charIndex: int Js.readonly_prop
    method charLength: int Js.readonly_prop
    method elapsedTime: float Js.readonly_prop
    method name: Js.js_string Js.t Js.readonly_prop
  end

  let start : event Js.t Dom.Event.typ = Dom.Event.make "start"
  let end_ : event Js.t Dom.Event.typ = Dom.Event.make "end"
  let pause : event Js.t Dom.Event.typ = Dom.Event.make "pause"
  let resume : event Js.t Dom.Event.typ = Dom.Event.make "resume"
  let mark : event Js.t Dom.Event.typ = Dom.Event.make "mark"
  let boundary : event Js.t Dom.Event.typ = Dom.Event.make "boundary"

  type error_code =
    | Canceled
    | Interrupted
    | Audio_busy
    | Audio_hardware
    | Network
    | Synthesis_unavailable
    | Synthesis_failed
    | Language_unavailable
    | Voice_unavailable
    | Text_too_long
    | Invalid_argument
    | Not_allowed

  let error_code_of_string = function
    | "canceled" -> Canceled
    | "interrupted" -> Interrupted
    | "audio-busy" -> Audio_busy
    | "audio-hardware" -> Audio_hardware
    | "network" -> Network
    | "synthesis-unavailable" -> Synthesis_unavailable
    | "synthesis-failed" -> Synthesis_failed
    | "language-unavailable" -> Language_unavailable
    | "voice-unavailable" -> Voice_unavailable
    | "text-too-long" -> Text_too_long
    | "invalid-argument" -> Invalid_argument
    | "not-allowed" -> Not_allowed
    | _ -> failwith "Speech.error_of_string"

  class type error = object
    inherit event
    method error : Js.js_string Js.t Js.readonly_prop (* FIXME? *)
  end

  let error : error Js.t Dom.Event.typ = Dom.Event.make "error"

  let voicesChanged : event Js.t Dom.Event.typ = Dom.Event.make "voiceschanged"
end

let voices =
  let t, r = Lwt.wait () in
  let resolve () = Lwt.wakeup r (Js.to_array engine##getVoices) in
  (if engine##getVoices##.length = 0 then
    let h = Dom.handler @@ fun _ -> resolve (); Js._false in
    ignore @@ Dom.addEventListener engine Event.voicesChanged h Js._true
  else
    resolve ());
  t

exception Error of Event.error_code

let utterance ?lang ?(pitch=1.0) ?(rate=1.0) ?voice ?(volume=1.0) s =
  let u = new%js utterance (Js.string s) in
  (match lang with None -> () | Some l -> u##.lang := l);
  u##.pitch := pitch;
  u##.rate := rate;
  (match voice with None -> () | Some v -> u##.voice := v);
  u##.volume := volume;
  u

let await u =
  let t, r = Lwt.wait () in
  (* FIXME ensure that elapsedTime is meaningful *)
  let h = Dom.handler @@ fun e -> Lwt.wakeup r e##.elapsedTime; Js._false in
  let open Event in
  let eh = Dom.handler @@ fun e ->
    Lwt.wakeup_exn r (Error (error_code_of_string (Js.to_string e##.error)));
    Js._false
  in
  ignore @@ Dom.addEventListener u end_ h Js._true;
  ignore @@ Dom.addEventListener u error eh Js._true;
  engine##speak u;
  t
