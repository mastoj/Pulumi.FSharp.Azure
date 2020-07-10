namespace Pulumi.FSharp.Kubernetes
    
type KubernetesResourceArgs = {
    Name: string
}

[<AbstractClass>]
type KubernetesResource internal () =

    static member Zero = {
         Name = "" // This needs to be an option or mandatory
     }
