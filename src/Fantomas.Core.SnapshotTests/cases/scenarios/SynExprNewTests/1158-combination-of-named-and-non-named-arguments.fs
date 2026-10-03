    let private sendTooLargeError () =
        new HttpResponseMessage(HttpStatusCode.RequestEntityTooLarge,
                                Content =
                                    new StringContent("File was too way too large",
                                                      System.Text.Encoding.UTF16,
                                                      "application/text"))
