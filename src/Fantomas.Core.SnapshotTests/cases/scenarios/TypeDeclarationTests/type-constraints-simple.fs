type Class1<'T when 'T :> System.Exception> =
    class end

type Class2<'T when 'T :> System.IComparable> =
    class end

type Class3<'T when 'T : null> =
    class end

type Class8<'T when 'T : not struct> =
   class end

type Class9<'T when 'T : enum<uint32>> =
   class end

type Class10<'T when 'T : comparison> =
   class end

type Class11<'T when 'T : equality> =
   class end

type Class12<'T when 'T : delegate<obj * System.EventArgs, unit>> =
   class end

type Class13<'T when 'T : unmanaged> =
   class end

type Class14<'T,'U when 'T : equality and 'U : equality> =
    class end