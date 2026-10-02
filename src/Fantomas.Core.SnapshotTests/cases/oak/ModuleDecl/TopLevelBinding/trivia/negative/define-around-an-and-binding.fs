let rec f x = g x
#if DEBUG
and g x = f x
#else
and g x = x
#endif
