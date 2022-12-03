open Synacor

let from_file fn = Stdio.In_channel.read_all fn |> Bytes.of_string |> Vm.of_bytes

let () =
  let init_state = from_file "challenge/challenge.bin" in
  ignore @@ Vm.run init_state
;;
