let FromZero () : 'T =
    (get32 0 :?> 'T) when 'T: BigInteger = BigInteger.Zero
