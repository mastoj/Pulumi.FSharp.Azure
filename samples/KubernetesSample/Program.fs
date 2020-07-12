module Program

open Pulumi.FSharp
open Pulumi.FSharp.Ops
open Pulumi.FSharp.Kubernetes.Apps.V1
open Pulumi.FSharp.Kubernetes.Types.Inputs.Apps.V1
open Pulumi.Kubernetes.Types.Inputs.Apps.V1
open Pulumi.Kubernetes.Types.Inputs.Meta.V1
open Pulumi.Kubernetes.Types.Inputs.Core.V1
// open Pulumi.Kubernetes.Types.Inputs.Core.V1
// open Pulumi.Kubernetes.Types.Inputs.Apps.V1
// open Pulumi.Kubernetes.Types.Inputs.Meta.V1
// open Pulumi.Kubernetes.Types.Inputs.ApiExtensions.V1Beta1

type X = { Y: Y }
and Y = int

let infra () =

  let appLabels = inputMap ["app", input "nginx" ]

  let deploySpec =
    DeploymentSpecArgs
      (Selector = input (LabelSelectorArgs(MatchLabels = appLabels)),
       Replicas = input 1,
       Template = input (
         PodTemplateSpecArgs
          (Metadata = input (ObjectMetaArgs(Labels = appLabels)),
           Spec = input (
              PodSpecArgs
                (Containers = 
                  inputList [
                    input (
                      ContainerArgs
                        (Name = input "nginx",
                         Image = input "nginx",
                         Ports = 
                          inputList [
                            input (
                             ContainerPortArgs
                               (ContainerPortValue = input 80))]))])))))

  

  let appDeploy = 
    deployment {
      name "MyDeploy"
      spec (deploymentSpec {
        replicas 1
      })
    }
    // Pulumi.Kubernetes.Apps.V1.Deployment("nginx",
    //   DeploymentArgs
    //     (Spec = input (
      // DeploymentSpecArgs
      //     (Selector = input (LabelSelectorArgs(MatchLabels = appLabels)),
      //      Replicas = input 1,
      //      Template = input (
      //        PodTemplateSpecArgs
      //         (Metadata = input (ObjectMetaArgs(Labels = appLabels)),
      //          Spec = input (
      //             PodSpecArgs
      //               (Containers = 
      //                 inputList [
      //                   input (
      //                     ContainerArgs
      //                       (Name = input "nginx",
      //                        Image = input "nginx",
      //                        Ports = 
      //                         inputList [
      //                           input (
      //                            ContainerPortArgs
      //                              (ContainerPortValue = input 80))]))]))))))))

  let name = 
    appDeploy.Metadata
    |> Outputs.apply(fun (metadata) -> metadata.Name)
  dict [("name", name :> obj)]

[<EntryPoint>]
let main _ =
  Deployment.run infra
