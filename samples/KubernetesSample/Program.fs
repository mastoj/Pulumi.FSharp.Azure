module Program

open Pulumi.FSharp
open Pulumi.FSharp.Ops
open Modified.Apps.V1
open Modified.Types.Inputs.Apps.V1
// open Pulumi.FSharp.Kubernetes.Types.Inputs.Meta.V1
open Pulumi.Kubernetes.Types.Inputs.Apps.V1
open Pulumi.Kubernetes.Types.Inputs.Meta.V1
open Pulumi.Kubernetes.Types.Inputs.Core.V1
//open Pulumi.FSharp.Kubernetes.Types.Inputs.Core.V1
// open Pulumi.Kubernetes.Types.Inputs.Core.V1
// open Pulumi.Kubernetes.Types.Inputs.Apps.V1
// open Pulumi.Kubernetes.Types.Inputs.Meta.V1
// open Pulumi.Kubernetes.Types.Inputs.ApiExtensions.V1Beta1

type X = { Y: Y }
and Y = int

let infra () =

  let appLabels = inputMap ["app", input "nginx" ]

  let appDeployStd =
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

  let appDeployCe = 
    deployment {
      name "MyDeploy"
      yield deploymentSpec {
        replicas 1
      }
      // deploymentSpec {
      //   replicas 1
      //   selector (labelSelector {
      //     matchLabels ["app", input "nginx" ]
      //   })
      //   template (podTemplateSpec {
      //     metadata (ObjectMetaArgs(Labels = appLabels))
      //     spec (podSpec {
      //       containers [
      //         input (container {
      //           name "nginx"
      //           image "nginx"
      //           ports [
      //             input (containerPort {
      //               containerPortValue 80
      //             })
      //           ]
      //         })
      //       ]
      //     })
      //   })
      // }
    }

  let name = 
    appDeployCe.Metadata
    |> Outputs.apply(fun (metadata) -> metadata.Name)
  dict [("name", name :> obj)]

[<EntryPoint>]
let main _ =
  Deployment.run infra
