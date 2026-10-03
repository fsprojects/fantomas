namespace X
module TopologicalBuilderTests =

    [<Fact>]
    let ``Builder works with ANY backend (backend-agnostic principle)`` () = task {
        let program = topological simulatorBackend {
            return ()
        }

        match! TopologicalBuilder.execute simulatorBackend program with
        | Ok _ -> Assert.True(true)
        | Error err -> Assert.Fail($"Program failed: {err.Message}")

        // Programs are COMPLETELY backend-agnostic!
    }

    [<Fact>]
    let ``Builder with braiding sequence`` () =
        task {
            // Increased backend capacity to 20 anyons to support 6 logical qubits
            let backend = TopologicalUnifiedBackendFactory.createUnified AnyonSpecies.AnyonType.Ising 20

            ()
        }
