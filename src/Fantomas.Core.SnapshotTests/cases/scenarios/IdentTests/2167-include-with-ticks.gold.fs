match req.``include`` with
| None -> tc.TestItems()
| Some includedTests -> includedTests.ToArray()
