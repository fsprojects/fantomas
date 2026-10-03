let str =
    $"""
    {let square x = x * x
     let isOdd x = x % 2 <> 0
     let oddSquares = List.filter isOdd >> List.map square
     oddSquares [ 1..0 ]}"""
