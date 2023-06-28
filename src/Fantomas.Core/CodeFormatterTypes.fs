namespace Fantomas.Core

open Fantomas.FCS.Text

type FormatResult =
    {
        /// Formatted code
        Code: string
        /// New position of the input cursor.
        /// This can be None when no cursor was passed as input or no position was resolved.
        Cursor: pos option
    }
