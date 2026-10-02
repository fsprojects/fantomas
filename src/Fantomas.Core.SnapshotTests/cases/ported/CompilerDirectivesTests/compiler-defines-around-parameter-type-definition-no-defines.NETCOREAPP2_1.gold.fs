let UpdateUI
    (theModel:
        #if NETCOREAPP2_1
        ITreeModel
    #else
    #endif
    )
    (info: FileInfo)
    ()
    =
    // File is good so enable the refresh button
    h.refreshButton.Sensitive <- true
    // Do real UI work here
    h.classStructureTree.Model <- theModel
    h.codeView.Buffer.Clear()
    h.mainWindow.Title <- "AltCover.Visualizer"
    updateMRU h info.FullName true
