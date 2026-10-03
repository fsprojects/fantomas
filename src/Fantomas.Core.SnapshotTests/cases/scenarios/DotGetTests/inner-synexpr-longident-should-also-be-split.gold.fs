let firstName =
    define.Attribute
        .ParsedRes(FirstName.value, FirstName.create)
        .Get(fun u -> u.FirstName)
        .SetRes(userSetter User.setFirstName)
