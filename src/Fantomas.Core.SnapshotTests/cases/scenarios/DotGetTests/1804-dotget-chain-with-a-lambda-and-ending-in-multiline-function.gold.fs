db.Schema.Users.Query
    .Where(fun x -> x.Role)
    .Matches(function
        | Role.User companyId -> companyId
        | _ -> __)
    .In(db.Schema.Companies.Query.Where(fun x -> x.LicenceId).Equals(licenceId).Select(fun x -> x.Id))
