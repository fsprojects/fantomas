Html.h1 42

Html.div "Hello there!"

Html.div [ Html.h1 "So lightweight" ]

Html.ul [
    Html.li "One"
    Html.li [ Html.strong "Two" ]
    Html.li [ Html.em "Three" ]
]
