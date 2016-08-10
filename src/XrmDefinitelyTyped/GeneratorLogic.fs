namespace DG.XrmDefinitelyTyped

open System
open System.IO
open System.Reflection
open System.Collections.Concurrent

open Microsoft.Xrm.Sdk
open Microsoft.Xrm.Sdk.Metadata
open Microsoft.Xrm.Sdk.Client

open IntermediateRepresentation
open Utility

open InterpretEntityMetadata
open InterpretBpfJson
open InterpretFormXml
  
open CreateEntityDts
open CreateEntityRestDts
open CreateOptionSetDts
open CreateIPageDts
open CreateFormDts

open TsStringUtil


module GeneratorLogic =

  type XrmAuthentication = {
    url: Uri
    username: string
    password: string
    domain: string option
    ap: AuthenticationProviderType option
  }

  type RawState = {
    metadata: EntityMetadata[]
    bpfData: Entity[]
    formData: Map<string,Entity[]>
  }

  type InterpretedState = {
    outputDir: string
    tsv: int * int
    entities: XrmEntity[]
    forms: XrmForm[]
    bpfControls: Map<string,ControlField list>
    moduleName:string
  }


  (** Reference helpers *)

  let makeRefPlain = sprintf "/// <reference path=\"%s\" />"

  (** Resource helpers *)

  let getResourceLines resName =
    let assembly = Assembly.GetExecutingAssembly()
    use res = assembly.GetManifestResourceStream(resName)
    use sr = new StreamReader(res)

    seq {
      while not sr.EndOfStream do yield sr.ReadLine ()
    } |> List.ofSeq

  let getDeclarationFile resName tsv moduleName =
    getResourceLines resName
    |> fun lines ->
          match tsv >= (1,4) with
          | true -> lines
          | false -> lines |> List.map (fun s -> s.Replace("const enum", "enum"))
    |> List.map(fun s -> 
        if(System.String.IsNullOrWhiteSpace(moduleName))
        then s
        else s.Replace("/*ModuleName*/", moduleName + "."))
            



  (** Generation functionality *)

  /// Clear any previously output files
  let clearOldOutputFiles out =
    printf "Clearing old files..."
    let rec emptyDir path =
      Directory.EnumerateFiles(path, "*.d.ts") |> Seq.iter File.Delete
      let dirs = Directory.EnumerateDirectories(path, "*") 
      dirs |> Seq.iter (fun dir ->
        emptyDir dir
        try Directory.Delete dir
        with ex -> ())

    Directory.CreateDirectory out |> ignore
    emptyDir out
    printfn "Done!"


  /// Generate the required output folder structure
  let generateFolderStructure out =
    printf "Generating folder structure..."
//    Directory.CreateDirectory (sprintf "%s/IPage" out) |> ignore
    Directory.CreateDirectory (sprintf "%s/Entities" out) |> ignore
    Directory.CreateDirectory (sprintf "%s/Enums" out) |> ignore
    Directory.CreateDirectory (sprintf "%s/Forms" out) |> ignore
//    Directory.CreateDirectory (sprintf "%s/_internal" out) |> ignore
//    Directory.CreateDirectory (sprintf "%s/_internal/EntityEnum" out) |> ignore
    printfn "Done!"

  // Proxy helper that makes it easy to get a new proxy instance
  let proxyHelper xrmAuth () =
    let ap = xrmAuth.ap |? AuthenticationProviderType.OnlineFederation
    let domain = xrmAuth.domain |? ""
    CrmAuth.authenticate
      xrmAuth.url ap xrmAuth.username 
      xrmAuth.password domain
    ||> CrmAuth.proxyInstance


  /// Connect to CRM with the given authentication
  let connectToCrm xrmAuth =
    printf "Connecting to CRM..."
    let proxy = proxyHelper xrmAuth ()
    printfn "Done!"
    proxy



  /// Retrieve version from CRM
  let retrieveCrmVersion mainProxy =
    printf "Retrieving CRM version..."

    let version = 
      CrmBaseHelper.retrieveVersion mainProxy

    printfn "Done!"
    version

  /// Retrieve all the necessary CRM data
  let retrieveCrmData sdkVersion mainProxy rawEntityMetadata=
//    let rawEntityMetadata = 
//      retrieveEntityMetadata entities mainProxy proxyGetter
    
    printf "Fetching BPF metadata from CRM..."
    let bpfData = 
      match checkVersion (6,0,0,0) sdkVersion with
      | true  -> CrmDataHelper.getBpfData mainProxy
      | false -> [||]
    printfn "Done!"

    printf "Fetching FormXmls from CRM..."
    let formData =
      rawEntityMetadata
      |> Array.map (fun (em:EntityMetadata) -> 
        em.LogicalName, 
        CrmDataHelper.getEntityForms mainProxy em.LogicalName)
      |> Map.ofArray
        
    let rowState ={ RawState.metadata = rawEntityMetadata; RawState.bpfData = bpfData;RawState.formData = formData }
    rowState


  /// Gets all the entities related to the given solutions and merges with the given entities
  let getFullEntityList entities solutions proxy =
    printf "Figuring out which entities should be included in the context.."
    let solutionEntities = 
      match solutions with
      | Some sols -> 
        sols 
        |> Array.map (CrmBaseHelper.retrieveSolutionEntities proxy)
        |> Seq.concat
        |> Set.ofSeq
      | None -> Set.empty

    let allEntities =
      match entities with
      | Some ents -> Set.union solutionEntities (Set.ofArray ents)
      | None -> solutionEntities

    printfn "Done!"
    match allEntities.Count with
    | 0 -> 
      printfn "Creating context for all entities"
      None
    | _ -> 
      let entities = allEntities |> Set.toArray 
      printfn "Creating context for the following entities: %s" (String.Join(",", entities))
      entities
      |> Some

  /// Interprets the raw CRM data into an intermediate state used for further generation
  let interpretCrmData out tsv moduleName (rawState:RawState) =
    printf "Interpreting data..."
    let nameMap = 
      rawState.metadata
      |> Array.Parallel.map (fun em -> em.LogicalName, em.SchemaName)
      |> Map.ofArray

    let entityMetadata =
      rawState.metadata |> Array.Parallel.map (interpretEntity nameMap)

    let bpfControls = interpretBpfs rawState.bpfData

    let forms = interpretFormXmls entityMetadata rawState.formData bpfControls
    printfn "Done!"

    { InterpretedState.entities = entityMetadata
      bpfControls = bpfControls
      forms = forms
      outputDir = out
      tsv = tsv
      moduleName = moduleName }


  /// Generate the files stored as resources
  let generateResourceFiles state =
    getDeclarationFile "base.d.ts" state.tsv state.moduleName
    |> fun lines -> File.WriteAllLines(sprintf "%s/base.d.ts" state.outputDir, lines)

//    getDeclarationFile "metadata.d.ts" state.tsv
//    |> fun lines -> 
//      File.WriteAllLines(
//        sprintf "%s/metadata.d.ts" state.outputDir, lines)
//      
//    getDeclarationFile "dg.xrmquery.d.ts" state.tsv
//    |> fun lines -> 
//      File.WriteAllLines(sprintf "%s/dg.xrmquery.d.ts" state.outputDir, lines)


  /// Generate a few base files
  let generateBaseFiles (state) =
    // Blank entity interfaces
    state.entities
    |> fun list -> getBlankEntityInterfaces(list, state.moduleName)
    |> fun lines -> 
      File.WriteAllLines(
        sprintf "%s/entities.d.ts" state.outputDir, 
        lines)

    // REST file
//    state.entities
//    |> getFullRestModule
//    |> fun lines ->  
//      File.WriteAllLines(
//        sprintf "%s/rest.d.ts" state.outputDir, 
//        lines)


  /// Generate the Enum files
  let generateEnumFiles  (refFilePaths:ConcurrentQueue<string>) (state) =
    printf "Writing Enum files..."
    state.entities
    |> getUniquePicklists
    |> Array.Parallel.map (fun os ->
        getOptionSetEnum(state.tsv, os, state.moduleName), os.displayName)
    |> Array.iter(fun (lines, displayName) ->
          let filePath =sprintf  "Enums/%s.d.ts" displayName
          refFilePaths.Enqueue(filePath)
          File.WriteAllLines(
            sprintf "%s/%s" state.outputDir filePath, lines))
    printfn "Done!"

  let generateReferenceFiles state (files)=
    files
    |> Seq.map makeRefPlain
    |> fun lines -> 
        File.WriteAllLines(sprintf "%s/index.d.ts" state.outputDir, lines)



  /// Generate the Entity files
  let generateEntityFiles  (refFilePaths:ConcurrentQueue<string>) (state) =
    printf "Writing Entity files..."
    state.entities
    |> Array.Parallel.map (fun e -> e.logicalName, getEntityInterfaces(e, state.moduleName))
    |> Array.Parallel.iter (fun (name, lines) ->
      let filePath = sprintf "Entities/%s.d.ts" name
      refFilePaths.Enqueue(filePath)
      File.WriteAllLines(sprintf "%s/%s" state.outputDir filePath, 
        lines))
    printfn "Done!"


  /// Generate the IPage files
  let generateIPageFiles (refFilePaths:ConcurrentQueue<string>) state =
    printf "Writing IPage files..."
    state.entities
    |> Array.Parallel.map (fun e -> e.logicalName, getIPageContext state.moduleName e)
    |> Array.Parallel.iter 
      (fun (name, lines) -> 
        let filePath = sprintf "Forms/%s.d.ts" name
        refFilePaths.Enqueue(filePath)
        File.WriteAllLines(sprintf "%s/%s" state.outputDir filePath, 
          lines))
    printfn "Done!"


  /// Generate the Form files
  let generateFormFiles (refFilePaths:ConcurrentQueue<string>) (state) =
    printf "Writing Form files..."
    state.forms
    |> Array.Parallel.map (fun xrmForm-> xrmForm, (sprintf "Form/%s/%s" xrmForm.entityName xrmForm.formType))
    |> Array.Parallel.iter(fun (xrmForm, path) ->
      Directory.CreateDirectory (sprintf "%s/%s" state.outputDir path) |> ignore
      let filePath = sprintf "%s/%s.d.ts" path xrmForm.name
      refFilePaths.Enqueue(filePath)

      // TODO: check for forms with same name
      let lines = xrmForm |> getFormDts state.moduleName
      File.WriteAllLines(sprintf "%s/%s" state.outputDir  filePath, 
        lines)
    )
    printfn "Done!"