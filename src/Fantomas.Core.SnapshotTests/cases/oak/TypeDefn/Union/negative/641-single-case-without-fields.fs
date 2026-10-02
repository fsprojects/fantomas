(*---
# A single case without fields keeps its bar: `type DU = Record` would be an abbreviation.
---*)
type Record = { Name: string }
type DU = | Record
