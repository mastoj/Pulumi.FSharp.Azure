namespace Pulumi.FSharp.Kubernetes

open Pulumi.FSharp.Kubernetes.Myriad

[<assembly: Generator.Pulumi>]
    do()
    
module private ForceChange =
    let _ = 18736