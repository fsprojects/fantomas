let WebApp =
    route "/ping"
    >=> authorized
    >=> text "pong"
