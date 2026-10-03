root.SetAttribute(
    "driverVersion",
    "AltCover.Recorder "
    + System.Diagnostics.FileVersionInfo
        .GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly().Location)
        .FileVersion
)
