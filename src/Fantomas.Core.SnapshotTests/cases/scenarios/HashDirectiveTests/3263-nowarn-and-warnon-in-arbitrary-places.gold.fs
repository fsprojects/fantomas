let SingleChoiceTextValueType =
    Define.Object<CustomValue>(
        name = "SingleChoiceTextValue",
        fields =
            [
                #nowarn 25 // Incomplete pattern match
                Define.Field("options", ListOf StringType, fun _ (SingleChoiceTextValue sctv) -> sctv.Options)
                Define.Field(
                    "optionsTranslations",
                    ListOf SelectTypeOptionsTranslationsType,
                    fun _ (SingleChoiceTextValue sctv) -> sctv.OptionsTranslations
                )
                Define.Field("value", StructNullable StringType, fun _ (SingleChoiceTextValue sctv) -> sctv.Value)
                Define.Field(
                    "renderType",
                    StructNullable CustomFieldRenderTypeType,
                    fun _ (SingleChoiceTextValue sctv) -> sctv.RenderType
                #warnon 25 // Incomplete pattern match
                )
            ]
    )
