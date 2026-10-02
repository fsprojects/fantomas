module M

exception FileNameNotResolved of string (*description of searched locations*) * string * range (*filename*)

exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
