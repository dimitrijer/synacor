open Synacor

let from_file fn = Stdio.In_channel.read_all fn |> Bytes.of_string |> State.of_bytes

let () =
  let init_state = from_file "challenge/challenge.bin" in
  ignore @@ Op.run init_state
;;
