namespace DG.XrmDefinitelyTyped

open System
open Utility
open GeneratorLogic

type XrmDefinitelyTyped private () =

  static member GetContext(url, username, password, ?domain, ?ap, ?out, ?tsv, ?entities, ?solutions, ?moduleName:string) =
    let xrmAuth =
      { XrmAuthentication.url = Uri(url);
        username = username;
        password = password;
        domain = domain
        ap = ap
      }

    let settings =
      { XrmDefinitelyTypedSettings.out = out
        tsv = tsv
        entities = entities
        solutions = solutions
        sdkVersion = None
        moduleName = defaultArg moduleName null
      }
    XrmDefinitelyTyped.GetContext(xrmAuth, settings)


  static member GetContext(xrmAuth, settings) =
    #if !DEBUG
    try
    #endif
      let out = settings.out |? "."
      let tsv = settings.tsv |? (Int32.MaxValue, Int32.MaxValue)

      // Pre-generation tasks
      clearOldOutputFiles out
      generateFolderStructure out

      let mainProxy = connectToCrm xrmAuth
      let proxyGetter = proxyHelper xrmAuth

      let sdkVersion =
        if settings.sdkVersion.IsNone then retrieveCrmVersion mainProxy
        else settings.sdkVersion.Value

      let entities = 
        getFullEntityList settings.entities settings.solutions mainProxy

      // Connect to CRM and interpret the data
      let data = 
        (mainProxy, proxyGetter)
        ||> retrieveCrmData sdkVersion entities
        |> interpretCrmData out tsv

      // Generate the files
      data
      |>> generateResourceFiles
      |>> fun d -> generateBaseFiles(d, settings.moduleName)
      |>> fun d -> generateEnumFiles(d, settings.moduleName)
      |>> generateEntityEnumFiles
      |>> fun d -> generateEntityFiles(d, settings.moduleName)
      |>> generateIPageFiles
      |>  generateFormFiles

      printfn "\nSuccessfully generated all TypeScript declaration files."

    #if !DEBUG
    with _ as ex ->
      failwithf "\nUnable to generate TypeScript files: %s" ex.Message
    #endif

