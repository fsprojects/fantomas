// check for match
if nargTs <> haveArgTs.Length then
    false (* method argument length mismatch *)
else if

    // If a known-number-of-arguments-including-object-argument has been given then check that
    (match knownArgCount with
     | ValueNone -> false
     | ValueSome n -> n <> (if methInfo.IsStatic then 0 else 1) + nargTs)
then
    false
else

    let res = typesEqual (resT :: argTs) (haveResT :: haveArgTs)
    res
