let result = Instrument.I.instrumentationVisitor state' visited

test
  <@
    { result with
        RecordingMethodRef =
          { Visit = null
            Push = null
            Pop = null } }
      =
      { state' with
          ModuleId = def.MainModule.Mvid.ToString()
          RecordingMethod = visit
          RecordingMethodRef =
            { Visit = null
              Push = null
              Pop = null } }
  @>
