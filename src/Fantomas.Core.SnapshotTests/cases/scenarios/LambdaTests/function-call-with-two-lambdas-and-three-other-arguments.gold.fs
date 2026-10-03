SettingControls.toggleButton
    (fun _ ->
        UpdateOption(key, MultilineFormatterTypeOption(o, key, "character_width"))
        |> dispatch)
    (fun _ ->
        UpdateOption(key, MultilineFormatterTypeOption(o, key, "number_of_items"))
        |> dispatch)
    "CharacterWidth"
    "NumberOfItems"
    key
    (v = "character_width")
