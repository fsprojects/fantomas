type internal Handler() =
    class
        [<
          #if NETCOREAPP2_1
          Builder.Object;
          #else
          #endif
          DefaultValue(true)>]
        val mutable mainWindow: Window
    end
