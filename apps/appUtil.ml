let split s =
  Core.Std.String.split ~on:' ' s
  |> List.map Core.Std.String.strip
  |> List.filter ((<>) "")
