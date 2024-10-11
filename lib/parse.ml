let f file =
  let ic = open_in file in
  let lbuf = Lexing.from_channel ic in
  Location.init lbuf file;
  try Parser.parse Lexer.tokenize lbuf |> Reserr.ok
  with Lexer.LexError e -> Reserr.error e
