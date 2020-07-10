module Pulumi.FSharp.Kubernetes.Core

open Microsoft.FSharp.Reflection
open Pulumi.FSharp
open Pulumi

type IOArg<'a> =
    | Object of 'a
    | Name of string
    | IO of Output<string>

let inline getName ioArg = 
    match ioArg with
    | Object o -> (^a : (member Name: Output<string>) (o)) |> io
    | Name n -> input n
    | IO i -> io i
    
let getUnionCaseName (case : 'a) = 
    match FSharpValue.GetUnionFields(case, typeof<'a>) with
    | case, _ -> case.Name
    
type KubernetesResourceArgs = {
    Name: string
}

[<AbstractClass>]
type AzureResource internal () =

    static member Zero = {
         Name = "" // This needs to be an option or mandatory
     }
