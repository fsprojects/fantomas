let private parseModel (modelSrc: string) : Result<MyReturnType, string list> =
    if not (File.Exists(modelSrc)) then
        Error [ sprintf "Provided modelSrc \"%s\" does not exist" modelSrc ]
    else

    let graph = new QueryableGraph()
    graph.LoadFromFile(modelSrc, TurtleParser())

    let xsdPath = Path.Combine(Directory.GetCurrentDirectory(), "Some.xsd")

    let ontologyPath = Path.Combine(Directory.GetCurrentDirectory(), "Some.ttl")

    let ontoGraph = new OntologyGraph()
    FileLoader.Load(ontoGraph, ontologyPath)

    let validationErrors =
        GraphValidation.validate xsdPath ontoGraph "http://company.com/meh" graph

    if List.isEmpty validationErrors then
        Ok graph
    else
        Error validationErrors
