let handle =
    if n < weakThreshhold then
        assert onStrongDiscard.IsNone // it disappeared
        Weak(WeakReference(v))
    else
        Strong(v)
