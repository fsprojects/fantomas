(*---
# A comment at the end of the line belongs to the exception, not to the last field.
---*)
module M

exception FileNameNotResolved of string (*description of searched locations*)  * string * range (*filename*)

exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
