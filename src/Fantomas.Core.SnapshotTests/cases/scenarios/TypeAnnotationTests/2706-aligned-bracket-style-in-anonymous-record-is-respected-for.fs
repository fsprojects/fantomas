let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |},{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |}
    >
