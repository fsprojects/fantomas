let private validateLocation =
    createValidatorFor<LocationAdded> () {
        validate (fun l -> l.Id) [ isNotEmptyGuid ]
        validate (fun l -> l.Name)
            [ isNotEmpty
              hasMinLengthOf 3 ]
        validate (fun l -> fst l.Location) [ isValidLatitude ]
        validate (fun l -> snd l.Location) [ isValidLongitude ]
        validate (fun l -> l.Price) [ isGreaterThan 0. ]
        validate (fun l -> l.Date) [ isNotMinDate ]
        validate (fun l -> l.Creator) [ isNotEmpty ]
    }
