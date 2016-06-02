namespace DG.XrmDefinitelyTyped

open TsStringUtil
open IntermediateRepresentation

module internal CreateOptionSetDts =
 
  let getOptionSetEnum (tsv:int*int,os:OptionSet, moduleName) =
    [Enum.Create(
        os.displayName,
        os.options 
        |> Array.Parallel.map (fun o -> o.label, Some o.value) 
        |> List.ofArray,
        constant = (tsv >= (1,4)) )]
    |>fun enums-> Module.Create(moduleName,declare = true, enums=enums)
    |> moduleToString


  let getUniquePicklists (es:XrmEntity[]) =
    es
    |> Array.Parallel.map (fun e -> e.opt_sets) |> List.concat
    |> Seq.distinctBy (fun os -> os.displayName) |> Array.ofSeq
