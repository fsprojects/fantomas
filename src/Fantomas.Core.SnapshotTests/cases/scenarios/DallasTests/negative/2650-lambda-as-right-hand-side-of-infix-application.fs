let answerToUniverse =
    question
    |> fun value ->
        TransformersModule.tryTransformToAnswerToUniverse value
        |> Option.defaultValue 42
