if m.Success then
    Some(List.tail [ for x in m.Groups -> x.Value ])
else
    None
