/// The JSON the daemon reads and writes, by hand, without reflection.
///
/// The encoding is the one `Fantomas.Client` has always spoken, which is what Newtonsoft.Json makes
/// of these F# types: a union as `{"Case": ..., "Fields": [...]}`, an option as `null` or a union
/// case named `Some`, and a record property that is `null` left out. Editors ship their own
/// `Fantomas.Client`, often an older one, so the daemon has to go on sending exactly that.
///
/// Written out rather than left to a serializer because Native AOT cannot run the reflection a
/// serializer would need, and System.Text.Json's source generator is not available to F#.
module internal Fantomas.DaemonJson

open System.Text.Json

/// Options that know how to read every request the daemon accepts and write every response and
/// notification it sends, and nothing else. A type they do not know fails loudly rather than
/// falling back to reflection.
val serializerOptions: unit -> JsonSerializerOptions
