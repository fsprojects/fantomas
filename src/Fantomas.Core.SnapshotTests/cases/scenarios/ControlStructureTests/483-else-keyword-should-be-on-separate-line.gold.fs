if i.OpCode = OpCodes.Switch then
    AccumulateSwitchTargets i targets
    c
else
    let branch = i.Operand :?> Cil.Instruction
    c + (Option.nullable branch.Previous)
