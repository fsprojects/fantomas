type ViewBinding<'model, 'msg> = string * Variable<'model, 'msg>
and ViewBindings<'model, 'msg> = ViewBinding<'model, 'msg> list

and Variable<'model, 'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model, 'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model, 'msg>
    | BindCmd of Execute<'model, 'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model, 'msg>
    | BindMap of Getter<'model> * (obj -> obj)
