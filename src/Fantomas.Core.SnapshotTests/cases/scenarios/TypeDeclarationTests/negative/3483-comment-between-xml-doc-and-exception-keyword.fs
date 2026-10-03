/// Signal that there is still an unresolved overload in the constraint problem. The
/// unresolved overload constraint remains in the constraint state, and we skip any
/// further processing related to whichever overall adjustment to constraint solver state
/// is being processed.
///
// NOTE: The addition of this abort+skip appears to be a mistake which has crept into F# type inference,
// and its status is currently under review. See https://github.com/dotnet/fsharp/pull/8294 and others.
//
exception AbortForFailedMemberConstraintResolution
