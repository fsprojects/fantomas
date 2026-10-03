(*---
# Tracked in https://github.com/fsprojects/fantomas/issues/1588
---*)
            if
              // Don't support implicit [<ReflectedDefinition>] on generated members, except the implicit members
              // for 'let' bound functions in classes.
              (not v.IsCompilerGenerated || v.IsIncrClassGeneratedMember) &&

              (// Check the attributes on any enclosing module
               env.reflect ||
               // Check the attributes on the value
               HasFSharpAttribute g g.attrib_ReflectedDefinitionAttribute v.Attribs ||
               // Also check the enclosing type for members - for historical reasons, in the TAST member values
               // are stored in the entity that encloses the type, hence we will not have noticed the ReflectedDefinition
               // on the enclosing type at this point.
               HasFSharpAttribute g g.attrib_ReflectedDefinitionAttribute v.TopValDeclaringEntity.Attribs) then

                ()
