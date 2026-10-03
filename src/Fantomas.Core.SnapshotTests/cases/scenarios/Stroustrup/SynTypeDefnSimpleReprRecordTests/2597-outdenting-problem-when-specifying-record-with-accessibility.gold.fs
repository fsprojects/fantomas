module OutdentingProblem =
    type Configuration = private {
        Setting1: int
        Setting2: bool
    }

    let withSetting1 value configuration = { configuration with Setting1 = value }

    let withSetting2 value configuration = { configuration with Setting2 = value }
