type Test =
    abstract RunJobs: folder:string * ?jobs:string * ?ctm:string * ?createDuplicate:bool * ?hold:bool * ?ignoreCriteria:bool * ?independentFlow:bool * ?orderDate:string * ?orderIntoFolder:string * ?variables:Dictionary<string, string>[] * ?waitForOrderDate:bool
     -> string

    override this.RunJobs(folder: string, ?jobs: string, ?ctm: string, ?createDuplicate: bool, ?hold: bool,
                          ?ignoreCriteria: bool, ?independentFlow: bool, ?orderDate: string, ?orderIntoFolder: string,
                          ?variables: Dictionary<string, string>[], ?waitForOrderDate: bool) =
        ""
