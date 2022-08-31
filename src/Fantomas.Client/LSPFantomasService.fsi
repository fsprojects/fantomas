module Fantomas.Client.LSPFantomasService

type LSPFantomasService =
    interface Contracts.FantomasService

    new: unit -> LSPFantomasService
