(*---
max_line_length = 80
---*)
namespace Test

module OrderProcessing =
  type ValidateOrder =
    CheckProductCodeExists    // dependency
      -> CheckAddressExists   // dependency
      -> UnvalidatedOrder     // input
      -> Result<ValidatedOrder,ValidationError>  // output (Result b/c one of deps returns a Result)
