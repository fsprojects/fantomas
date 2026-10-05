let nestedList: obj list =
    [| "11111111aaaaaaaaa"
       "22222222aaaaaaaaa"
       "33333333aaaaaaaaa"
       [| "11111111bbbbbbbbbbbbbbb"
          "22222222bbbbbbbbbbbbbbb"
          "33333333bbbbbbbbbbbbbbb"
          // this case looks weird but seen rarely
          |] |]
