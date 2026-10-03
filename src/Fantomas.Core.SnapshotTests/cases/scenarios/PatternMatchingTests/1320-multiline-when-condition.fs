module Foo =
    module Bar =
            let buildUsage argInfos =
                match v.IsMember, v.IsInstanceMember, v.LogicalName, v.DisplayName with
                // Ordinary functions or values
                | false, _, _, name when
                    not (hasAttribute<RequireQualifiedAccessAttribute> v.ApparentEnclosingEntity.Attributes) ->
                    name + " " + parArgs
                // Ordinary static members or things (?) that require fully qualified access
                | _, _, _, name -> name + parArgs
