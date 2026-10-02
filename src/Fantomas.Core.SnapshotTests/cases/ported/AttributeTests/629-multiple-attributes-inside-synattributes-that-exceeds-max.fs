(*---
fsharp_max_infix_operator_expression = 40
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
---*)
//[<ApiExplorerSettings(IgnoreApi = true)>]
[<Route("api/v1/admin/import")>]
type RoleAdminImportController(akkaService: AkkaService) =
  inherit Controller()

  [<HttpGet("jobs/all");
    ProducesResponseType(typeof<bool>, 200);
    ProducesResponseType(404);
    Authorize(AuthorizationScopePolicies.Read)>]
  member _.ListJobs(): Task<UserCmdResponseMsg> =
    task {
      return!
        akkaService.ImporterSystem.ApiMaster <? ApiMasterMsg.GetAllJobsCmd
    }

  [<HttpPost("jobs/create");
    DisableRequestSizeLimit;
    RequestFormLimits(MultipartBodyLengthLimit = 509715200L);
    ProducesResponseType(typeof<RoleChangeSummaryDto list>, 200);
    ProducesResponseType(404);
    Authorize(AuthorizationScopePolicies.Write)>]
  member _.StartJob(file: IFormFile, [<FromQuery>] args: ImporterJobArgs) =
    let importer = akkaService.ImporterSystem

    ActionResult.ofAsyncResult <| asyncResult {
      let! state =
        (LowerCaseString.create args.State, file)
        |> pipeObjectThroughValidation [ (fst, [stateIsValid]); (snd, [(fun s -> Ok s)]) ]

      let! filePath = FormFile.downloadAsTemp file

      let job =
        { JobType = EsriBoundaryImport
          FileToImport = filePath
          State = state
          DryRun = args.DryRun }

      importer.ApiMaster <! StartImportCmd job
      return Ok job
    }
