(*---
fsharp_multiline_bracket_style = stroustrup
---*)
type UserInfo = {
    /// A unique identifier assigned to this user.
    UserId: UserId.T
    AcsId: AcsId.T
    // Roles: Role list
    // NetworkStatus: UserStatus
    // ancillary info
    // Descr: UserDetails
}
