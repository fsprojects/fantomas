type IntersectionOptions
    private
    (
        primary: bool,
        ?root: Element,
        ?rootMargin: string,
        ?threshold: ResizeArray<float>,
        ?triggerOnce: bool
    ) =

    new
        (
            ?root: Element,
            ?rootMargin: string,
            ?threshold: ResizeArray<float>,
            ?triggerOnce: bool
        ) =

        IntersectionOptions(true)
