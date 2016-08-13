namespace DG.XrmDefinitelyTyped

open System
open Utility
open GeneratorLogic
open System.Collections.Concurrent

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
        |> interpretCrmData out tsv settings.moduleName

      // Pre-generation tasks
      clearOldOutputFiles out
      generateFolderStructure out

      //let realData =
      let fileList = new ConcurrentQueue<string>()

      // Generate the files
      data
      |>> generateResourceFiles
      |>> generateBaseFiles
      |>> generateEnumFiles fileList
      //|>> generateEntityEnumFiles
      |>> generateEntityFiles fileList
      //|>> generateIPageFiles fileList
      |>  generateFormFiles fileList
      
      
      fileList
      |> generateReferenceFiles data

      printfn "\nSuccessfully generated all TypeScript declaration files."

    #if !DEBUG
    with _ as ex ->
      failwithf "\nUnable to generate TypeScript files: %s" ex.Message
    #endif

