let rec f x = g x
#if DEBUG
#else
and g x = x
#endif
