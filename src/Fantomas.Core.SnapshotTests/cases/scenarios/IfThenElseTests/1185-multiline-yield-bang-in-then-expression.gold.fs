let lessonsForm (f: ValidatedForm<Request.CreateLessons>) dispatch =

    Html.div
        [ Bulma.field.div
              [ Bulma.fieldBody
                    [ Bulma.button.button
                          [ color.isPrimary
                            prop.text "Přidat lekce"
                            if f.IsLoading then
                                yield! [ button.isLoading; prop.disabled true ]
                            prop.onClick (fun _ -> CreateLessons |> dispatch) ] ] ] ]
