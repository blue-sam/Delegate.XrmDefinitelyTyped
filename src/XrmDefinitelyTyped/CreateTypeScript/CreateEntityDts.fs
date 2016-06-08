﻿namespace DG.XrmDefinitelyTyped

open TsStringUtil
open IntermediateRepresentation

module internal CreateEntityDts =

  (** Interface name helper functions *)
  let strConcat = sprintf "%s%s"
  let baseName x = strConcat x "Base"
  let resultName x = strConcat x "Result"
  let selectName x = strConcat x "_Select"
  let filterName x = strConcat x "_Filter"
  let expName x = strConcat x "_Expand"

  (** Type helper functions *)
  let arrayOf = Type.Custom >> Type.Array

  let attribute t = Type.Generic("Attribute",t)
  let expandable t u = Type.Generic("Expandable", sprintf "%s,%s" (selectName t) (selectName u))
  let valueContainer t = Type.SpecificGeneric("ValueContainerFilter", t)
  let results = sprintf "%sResult" >> fun x -> Type.Generic("SDK.Results", x) 

  (** TypeScript helper functions *)
  let getSelectVariables selectName (list: XrmAttribute list) = 
    list |> List.map (fun v -> 
      Variable.Create(v.schemaName, attribute selectName))


  let getExpandVariables entityName (list: XrmRelationship list) = 
    list |> List.map (fun r -> 
      Variable.Create(r.schemaName, expandable entityName r.relatedEntity))


  let getOrgVariables (list: XrmAttribute list) = 
    list |> List.map (fun v -> 
      let vType = 
        match v.specialType with
        | SpecialType.OptionSet       -> Type.SpecificGeneric("SDK.OptionSet", v.varType)
        | SpecialType.Money           -> Type.Custom "SDK.Money"
        | SpecialType.EntityReference -> Type.Custom "SDK.EntityReference"
        | _ -> v.varType
      Variable.Create(v.schemaName, vType))

  let getFilterVariables (list: XrmAttribute list) = 
    list |> List.map (fun v -> 
      let vType = 
        match v.specialType with
        | SpecialType.OptionSet       -> valueContainer v.varType
        | SpecialType.Money           -> valueContainer Type.Number
        | SpecialType.EntityReference -> Type.Custom "EntityReferenceFilter"
        | SpecialType.Guid            -> Type.Custom "Guid"
        | _ -> v.varType
      Variable.Create(v.schemaName, vType))


  let getRelationshipVariables isResult (list: XrmRelationship list) =
    list |> List.map (fun r -> 
      let rType = 
        match r.referencing, isResult with
        | true, _       -> Type.Custom r.relatedEntity
        | false, false  -> arrayOf r.relatedEntity
        | false, true   -> results r.relatedEntity
      Variable.Create(r.schemaName, rType)
    )


  (** Code creation methods *)

  /// Create entity interfaces
  let getEntityInterfaces (e, moduleName) = 
    let baseName = baseName e.schemaName
    let selName = selectName e.schemaName
    let expName = expName e.schemaName
    let resultName = resultName e.schemaName
    let filterName = filterName e.schemaName

    let mapping = 
      Variable.Create(
        e.schemaName, Type.Generic ("QueryMapping", 
          (sprintf "%s,%s,%s,%s,%s" e.schemaName selName expName filterName resultName)))

    [ Interface.Create(baseName, export = Export,
        vars = (e.attr_vars |> getOrgVariables),
        superClass = "Entity")
      Interface.Create(e.schemaName, export = Export,
        vars = (e.rel_vars |> getRelationshipVariables false),
        superClass = baseName)
      Interface.Create(resultName, export = Export,
        vars = (e.rel_vars |> getRelationshipVariables true),
        superClass = baseName)

      // XrmQuery interfaces
//      Interface.Create(selName, export = Export,
//        vars = (e.attr_vars |> getSelectVariables selName),
//        superClass = expName)
//      Interface.Create(filterName, export = Export,
//        vars = (e.attr_vars |> getFilterVariables))
//      Interface.Create(expName, export = Export,
//        vars = (e.rel_vars |> getExpandVariables e.schemaName))

//      Interface.Create("Entities",export = Export, vars = [mapping])
    ]
    |> fun list -> 
        let m=Module.Create("Entities",declare = true, interfaces = list)
        if(System.String.IsNullOrWhiteSpace(moduleName))
        then m
        else
            Module.Create(moduleName, modules=[m])
    |> moduleToString

//  let getEntityContext (e, moduleName) =
//    getEntityInterfaces(e, moduleName) 
  

  /// Create blank interfaces for entities.d.ts
  let getBlankEntityInterfaces (es: XrmEntity[], moduleName) = 
    let queryMapping = 
      Interface.Create("QueryMapping<O, S, E, F, R>")

    es
        |> Array.map (fun e ->
          let baseName = sprintf "%sBase" e.schemaName
          [ Interface.Create(baseName,export = Export, superClass = "Entity")
            Interface.Create(sprintf "%sResult" e.schemaName,export = Export, superClass = baseName)
            //Interface.Create(sprintf "%s_Select" e.schemaName, export = Export)
            Interface.Create(e.schemaName, export = Export, superClass = baseName) ])
        |> List.concat
        |> fun list -> Interface.Create("Entity",export = Export) :: list 
        //|> fun list -> queryMapping :: Interface.Create("Entity",export = Export) :: list 
        |> fun list -> 
            let m = Module.Create("Entities", declare = true, interfaces = list)
            if(System.String.IsNullOrWhiteSpace(moduleName))
            then m
            else
                Module.Create(moduleName, modules=[m])
        |> moduleToString


  /// Create entity enums
  let getEntityEnums moduleName (e:XrmEntity): string list =
    let enums =
      e.opt_sets
      |> List.map (fun os ->
        Enum.Create(os.displayName, 
          os.options 
            |> Array.map (fun o -> o.label, Some o.value) |> List.ofArray,
          export = true))
      |> List.fold (fun acc os -> 
        if List.exists (fun (x:Enum) -> os.name = x.name) acc then acc 
        else os::acc) []

    let m= Module.Create(
    sprintf "Enums.%s" e.schemaName,
    export = ExportType.Export,
    enums = enums) 
    if(System.String.IsNullOrWhiteSpace(moduleName))
    then m
    else
        Module.Create(moduleName, modules=[m])
    |> moduleToString

