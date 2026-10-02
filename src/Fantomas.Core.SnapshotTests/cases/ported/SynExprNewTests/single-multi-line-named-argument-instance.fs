let myInstance =
        new EvilBadRequest(Content = new StringContent("File was too way too large, as in waaaaaaaaaaaaaaaaaaaay tooooooooo long",
                                                      System.Text.Encoding.UTF16,
                                                      "application/text"))
