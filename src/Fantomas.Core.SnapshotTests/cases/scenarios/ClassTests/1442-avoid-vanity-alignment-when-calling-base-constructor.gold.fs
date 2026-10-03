type public DerivedExceptionWithLongNaaaaaaaaameException
    (
        message: string,
        code: int,
        originalRequest: string,
        originalResponse: string
    ) =
    inherit
        BaseExceptionWithLongNaaaameException(
            message,
            code,
            originalRequest,
            originalResponse
        )
