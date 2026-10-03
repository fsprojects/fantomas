let EnableHeapTerminationOnCorruption() =
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows) &&  Environment.OSVersion.Version.Major >= 6 && // If OS is Vista or higher
            Environment.Version.Major < 3) then // and CLR not 3.0 or higher
            let HeapEnableTerminationOnCorruption = 1u : uint32
            if not (HeapSetInformation(GetProcessHeap(), HeapEnableTerminationOnCorruption, UIntPtr.Zero, UIntPtr.Zero)) then
                  raise (System.Security.SecurityException(
                            "Unable to enable unmanaged process execution option TerminationOnCorruption. " +
                            "HeapSetInformation() returned FALSE; LastError = 0x" +
                            GetLastError().ToString("X").PadLeft(8, '0') + "."))
