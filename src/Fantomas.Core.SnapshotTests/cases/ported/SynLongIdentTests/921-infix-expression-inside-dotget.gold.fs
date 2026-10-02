let variable =
    (DataAccess.getById moduleName.readData { Id = createObject.Id }
     |> Result.okValue)
        .Value
