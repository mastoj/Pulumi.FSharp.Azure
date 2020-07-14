module Nested

open Pulumi.FSharp
open Pulumi.Kubernetes
open Pulumi.Kubernetes.Types.Inputs.Apps.V1
open Pulumi.Kubernetes.Types.Inputs.Core.V1
open Pulumi.Kubernetes.Types.Inputs.Meta.V1

type ContainerPort() =
  member __.Yield(_) = ContainerPortArgs()
  member __.Combine (x, y) = x
  member __.Delay f = f()
  member __.Zero() = ContainerPortArgs()
  
  [<CustomOperation("containerPortValue")>]
  member __.ContainerPortValue(state: ContainerPortArgs, value: int) = ContainerPortArgs()

let containerPort = ContainerPort()

type Container() =
  member __.Yield(port: ContainerPortArgs) = ContainerArgs()
  member __.Yield(_) = ContainerArgs()
  member __.Combine (x: ContainerArgs, y: ContainerArgs) = x
  member __.Delay f = f()
  member __.Zero() = ContainerArgs()
  member __.For(a, b) = ContainerArgs()
  [<CustomOperation("name")>]
  member __.Name(state: ContainerArgs, name: string) = state
  [<CustomOperation("image")>]
  member __.Image(state, image) = state

let container = Container()

type PodSpec() =
  member __.Yield(container: ContainerArgs) = PodSpecArgs()
  member __.Combine (x: PodSpecArgs, y: PodSpecArgs) = x
  member __.Delay f = f()
  member __.Zero() = PodSpecArgs()

let podSpec = PodSpec()

type ObjectMeta() =
  member __.Yield(_) = ObjectMetaArgs()
  member __.Combine (x, y) = x
  member __.Delay f = f()
  member __.Zero() = ObjectMetaArgs()
  [<CustomOperation("labels")>]
  member __.Name(state, name) = state

let metadata = ObjectMeta()

type PodTemplateSpec() =
  member __.Yield(spec: PodSpecArgs) = PodTemplateSpecArgs(Spec = input spec)
  member __.Yield(meta: ObjectMetaArgs) = PodTemplateSpecArgs()
  member __.Combine (x, y) = x
  member __.Delay f = f()
  member __.Zero() = PodTemplateSpecArgs()
  
let podTemplateSpec = PodTemplateSpec()

type LabelSelector() =
  member __.Yield(_) = LabelSelectorArgs()
  member __.Combine (x, y) = x
  member __.Delay f = f()
  member __.Zero() = LabelSelectorArgs()
  [<CustomOperation("matchLabels")>]
  member __.MatchLabels(state, name) = state

let selector = LabelSelector()

type DeploymentSpec() =
  member __.Yield(spec: PodTemplateSpecArgs) = DeploymentSpecArgs()
  member __.Yield(selector: LabelSelectorArgs) = DeploymentSpecArgs()
  member __.Yield(_) = DeploymentSpecArgs()
  member __.Combine (x, y) = x
  member __.Delay f = f()
  member __.Zero() = DeploymentSpecArgs()
  member __.For(a, b) = DeploymentSpecArgs()
  [<CustomOperation("replicas")>]
  member __.Replicas(state, number: int) = state

let deploymentSpec = DeploymentSpec()

type Deployment(name) =
  member __.Yield(spec: DeploymentSpecArgs) = DeploymentArgs()
  member __.Combine (x, y) = x @ y
  member __.Delay f = f()
  member x.Zero() = ()

let deployment n = Deployment n


let appLabels = "todo"

let d = deployment "mydep" {
    deploymentSpec {
        replicas 1
        podTemplateSpec {
            metadata {
                labels appLabels
            }
            podSpec {
                container {
                    name "nginx"
                    image "nginx"
                    containerPort { 
                        containerPortValue 80
                    }
                }
            }
        }
        selector {
            matchLabels appLabels
        }
    }
}