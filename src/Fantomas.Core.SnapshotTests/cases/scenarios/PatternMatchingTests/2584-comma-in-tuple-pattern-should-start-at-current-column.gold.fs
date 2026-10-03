let existingCount inputA inputB =
    match (inputA, inputB) with
    | """line1
line2
line3
"""   ,
      inputB -> "InputB=" + inputB
    | _ -> failwith "Invalid query"
