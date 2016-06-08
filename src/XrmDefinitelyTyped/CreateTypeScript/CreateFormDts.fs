namespace DG.XrmDefinitelyTyped

open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

open TsStringUtil
open Utility
open IntermediateRepresentation

module internal CreateFormDts =

  /// Translate internal attribute type to corresponding TypeScript interface.
  let getAttributeInterface = function
    | AttributeType.OptionSet ty  -> Type.SpecificGeneric ("Forms.OptionSetAttribute", ty)
    | AttributeType.Default ty    -> Type.SpecificGeneric ("Forms.Attribute", ty)
    | x                           -> Type.Custom (sprintf "Forms.%AAttribute" x)
 
  /// Gets the corresponding enum of the option set if possible
  let getOptionSetType = function
    | Some (AttributeType.OptionSet ty) -> ty
    | _ -> Type.Number

  /// Translate internal control type to corresponding TypeScript interface.
  let getControlInterface cType aType =
    match aType, cType with
    | None, ControlType.Default       -> Type.Custom "Forms.BaseControl"
    | Some (AttributeType.Default Type.String), ControlType.Default
                                      -> Type.Custom "Forms.StringControl"
    | Some at, ControlType.Default    -> Type.SpecificGeneric ("Forms.Control", getAttributeInterface at) 
    | aType, ControlType.OptionSet    -> Type.SpecificGeneric ("Forms.OptionSetControl", getOptionSetType aType)
    | _, x                            -> Type.Custom (sprintf "Forms.%AControl" x)

  /// Default collection functions which also use the "get" function name.
  let defaultCollectionFuncs emptyType defaultType typeParam = 
    if(System.String.IsNullOrWhiteSpace(typeParam))
    then []
    else
        [Function.Create("get", 
        [ Variable.Create("name", Type.Custom typeParam) ], 
        Type.Custom defaultType)]
    @
    [ 
        Function.Create("get", 
            [ Variable.Create("name", Type.String) ], 
            Type.Custom emptyType)
        Function.Create("get", [], Type.Array (Type.Custom defaultType))
        Function.Create("get", 
            [Variable.Create("index", Type.Number)], Type.Custom defaultType)
        Function.Create("get", 
            [Variable.Create("chooser", 
              Type.Function(
                [ Variable.Create("item", Type.Custom defaultType)
                  Variable.Create("index", Type.Number) ], 
                Type.Boolean))], 
              Type.Array (Type.Custom defaultType))
    ]
        
      
//    Function.Create("get", 
//        [ Variable.Create("name", Type.String) ], 
//        Type.Custom emptyType)
//
//      Function.Create("get", [], Type.Array (Type.Custom defaultType))

//      Function.Create("get", 
//        [Variable.Create("index", Type.Number)], Type.Custom defaultType)
//      Function.Create("get", 
//        [Variable.Create("chooser", 
//          Type.Function(
//            [ Variable.Create("item", Type.Custom defaultType)
//              Variable.Create("index", Type.Number) ], 
//            Type.Boolean))], 
//          Type.Array (Type.Custom defaultType))]


  /// Generate Xrm.Page.data.entity.attributes.get(<string>) functions.
  let getAttributeCollection (attributes:XrmFormAttribute list) =
    let getFuncs = 
      attributes
      |> List.map (fun (name,ty) ->
        let paramType = getConstantType name
        let returnType = getAttributeInterface ty
        Function.Create("get", [Variable.Create("name", paramType)], returnType))

    let typeParam = if attributes.Length > 0 then "attributes" else ""
    let defaultFuncs = defaultCollectionFuncs "void" "Forms.Attribute<any>" typeParam
    Interface.Create("Attributes", superClass = "Forms.AttributeCollectionBase",
      funcs = getFuncs @ defaultFuncs)

  let getAttributesType (attributes:XrmFormAttribute list) =
      attributes
      |> List.map (fun (name,ty) ->
         getNameConst name)


  /// Generate Xrm.Page.ui.controls.get(<string>) functions.
  let getControlCollection (controls:XrmFormControl list) =
    let getFuncs = 
      controls
      |> List.map (fun (name, aType, cType) ->
        let paramType = getConstantType name
        let returnType = getControlInterface cType aType          
        Function.Create("get", [Variable.Create("name", paramType)], returnType))

    let typeParam = if controls.Length > 0 then "controls" else ""
    let defaultFuncs = defaultCollectionFuncs "void" "Forms.BaseControl" typeParam
    Interface.Create("Controls", superClass = "Forms.ControlCollectionBase",
      funcs = getFuncs @ defaultFuncs)

  let getControlsType (controls:XrmFormControl list) =
      controls
      |> List.map (fun (name,ty,t) ->
         getNameConst name)
  let getTabsType (tabs:XrmFormTab list) =
      tabs
      |> List.map (fun (name,ty,t) ->
         getNameConst name)


  /// Generate Xrm.Page.ui.tabs.get(<string>) functions.
  let getTabCollection (tabs:XrmFormTab list) =
    let getFuncs =
      tabs
      |> List.map (fun (iname, name, sections) ->
        let paramType = getConstantType name
        let returnType = sprintf "Forms.PageTab<Tabs.%s>" iname |> Type.Custom
        Function.Create("get", [Variable.Create("name", paramType)], returnType))
    let typeParam = if tabs.Length > 0 then "tabs" else ""
    let defaultFuncs = 
      defaultCollectionFuncs 
        "void" 
        "Forms.PageTab<Forms.Collection<Forms.PageSection>>"
        typeParam

    Interface.Create("Tabs", superClass = "Forms.TabCollectionBase",
      funcs = getFuncs @ defaultFuncs)


  /// Generate Xrm.Page.ui.tabs.get(<someTab>).sections.get(<string>) functions.
  let getSectionCollections (tabs:XrmFormTab list) =
    let getFuncs sections = 
      sections
      |> List.map (fun name -> 
        let paramType = getConstantType name
        Function.Create("get", [ Variable.Create("name", paramType) ], 
          Type.Custom "Forms.PageSection"))

    let defaultFuncs = defaultCollectionFuncs "void" "Forms.PageSection" ""
    tabs |> List.map (fun (iname, name, sections) ->
      Interface.Create(iname, superClass = "Forms.SectionCollectionBase",
        funcs = getFuncs sections @ defaultFuncs))



  /// Generate Xrm.Page.getAttribute(<string>) functions.
  let getAttributeFuncs (attributes:XrmFormAttribute list) addtl =
    let attrFuncs = 
      attributes
      |> List.map (fun (name, ty) ->
        let paramType = getConstantType name
        let returnType = getAttributeInterface ty
        Function.Create("getAttribute", 
          [ Variable.Create("attributeName", paramType) ], returnType))

    let defaultFunc =
      Function.Create("getAttribute", 
        [ Variable.Create("attributeName", Type.String) ], 
        Type.Custom "void")
    attrFuncs @ addtl @ [ defaultFunc ]


  
  /// Generate Xrm.Page.getControl(<string>) functions.
  let getControlFuncs (controls:XrmFormControl list) addtl =
    let ctrlFuncs = 
      controls
      |> List.map (fun (name, aType, cType) ->
        let paramType = getConstantType name
        let returnType = getControlInterface cType aType
        Function.Create("getControl", 
          [ Variable.Create("controlName", paramType) ], returnType))

    let defaultFunc =
      Function.Create("getControl", 
        [ Variable.Create("controlName", Type.String) ], 
        Type.Custom "void")
    ctrlFuncs @ addtl @ [ defaultFunc ]



  /// Generate internal module for keeping track all the collections.
  let getFormSubmodule (form:XrmForm) =
    Module.Create(form.name,
      interfaces = 
        [ getAttributeCollection form.attributes 
          getControlCollection form.controls 
          getTabCollection form.tabs ],
      modules = 
        [ Module.Create("Tabs", interfaces = getSectionCollections form.tabs) ],
      types = 
        [ TypeDef.Create("controls", getControlsType form.controls)
          TypeDef.Create("attributes", getAttributesType form.attributes)
          TypeDef.Create("tabs", getTabsType form.tabs)])


  /// Generate the interface for the Xrm.Page of the form.
  let getFormInterface (form:XrmForm) =
    let superClass = 
      sprintf "Forms.PageBase<%s.Attributes,%s.Tabs,%s.Controls>"
        form.name form.name form.name
    let attrFunction = if(form.attributes.Length > 0) then [Function.Create("getAttribute", 
            [ Variable.Create("attributeName", Type.String) ], 
            Type.Custom (form.name + ".attributes"))] else []
    let ctrlFunction = if(form.controls.Length > 0) then [Function.Create("getControl", 
            [ Variable.Create("controlName", Type.String) ], 
            Type.Custom (form.name + ".controls"))] else []
    let afs = getAttributeFuncs form.attributes attrFunction
    let cfs = getControlFuncs form.controls ctrlFunction
    Interface.Create(form.name, export = ExportType.Export, superClass = superClass, 
      funcs = afs@cfs)


  /// Generate the module containing all the form interface and internal 
  /// module for collections.
  let getFormDts moduleName (form:XrmForm) = 
    let m = Module.Create(
      sprintf "Forms.%s.%s" (form.entityName |> Utility.sanitizeString) form.formType, 
      export = ExportType.Export,
      modules = [ getFormSubmodule form ],
      interfaces = [ getFormInterface form ]) 
    
    if(System.String.IsNullOrWhiteSpace(moduleName))
    then m
    else
        Module.Create(moduleName, modules=[m])
    |> moduleToString

