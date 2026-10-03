(
    (* comment before gets swallowed *)
    fun x -> x * 42
)

(
    fun x -> x * 42
    (* comment after is OK *)
)

(   (* comment on first line is OK too *)
    fun x -> x * 42
)
