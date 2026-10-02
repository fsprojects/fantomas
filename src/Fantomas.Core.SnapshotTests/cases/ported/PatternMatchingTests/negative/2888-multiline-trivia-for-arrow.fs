match subcategory with
| BuildPhaseSubcategory.Internal
// Getting here means the compiler has ICE-d. Let's not pile on by showing the unknownSubcategory assert below.
// Just treat as an unknown-to-LanguageService error.
 -> false
