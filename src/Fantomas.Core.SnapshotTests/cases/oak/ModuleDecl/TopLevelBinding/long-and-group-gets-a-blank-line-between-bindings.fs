let rec parseExpression tokens = match tokens with | [] -> failwith "unexpected end" | t :: rest -> parseTerm t rest
and parseTerm token rest = match token with | Number n -> Literal n, rest | _ -> parseExpression rest
