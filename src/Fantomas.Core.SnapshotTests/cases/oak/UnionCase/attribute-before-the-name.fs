type Argument =
  | [<MandatoryAttribute>] Action of string
  | [<MandatoryAttribute>] ProjectFile of string
  | PackageId of string
