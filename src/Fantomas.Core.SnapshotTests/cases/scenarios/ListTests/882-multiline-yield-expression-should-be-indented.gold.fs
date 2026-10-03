let choices: Foo list =
    [ yield! getMore 9
      yield
          // Test
          Foo 2 ]
