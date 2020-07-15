module Pulumi.FSharp.Kubernetes.Myriad

open System
open FSharp.Data
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree
open FsAst
open Myriad.Core

// Version needs to match NuGet package
[<Literal>]
let private pulumiSchemaUrl =
    "https://raw.githubusercontent.com/pulumi/pulumi-kubernetes/master/provider/cmd/pulumi-resource-kubernetes/schema.json"
    // "https://raw.githubusercontent.com/pulumi/pulumi-azure/v3.11.0/provider/cmd/pulumi-resource-azure/schema.json"

type private PulumiProvider =
    JsonProvider<pulumiSchemaUrl>

[<RequireQualifiedAccess>]
module Generator =
    type PulumiAttribute() =
        inherit Attribute()

let private createAttribute name =
    SynAttributeList.Create(SynAttribute.Create(name))

let private createModuleWithAttributes attributes name content =
    let componentInfo =
        { SynComponentInfoRcd.Create [ Ident.Create name ] with 
              Attributes = attributes }
    SynModuleDecl.CreateNestedModule(componentInfo, content)

let private createAutoOpenModule = createModuleWithAttributes [(createAttribute "AutoOpen")]

let private createModule = createModuleWithAttributes []

let private createNamespace name content =
    { SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong name)
                with Declarations = content }

let private createAttributeWithArg (name : string) (arg : string) =
    let o : SynAttribute = { TypeName = LongIdentWithDots.CreateString(name)
                             ArgExpr = SynExpr.CreateParen(SynExpr.CreateConstString(arg))
                             Target = None
                             AppliesToGetterAndSetter = false
                             Range = range.Zero }
    SynAttributeList.Create(o)
    
let private createOuterModule name content =
    { SynModuleOrNamespaceRcd.CreateModule(Ident.CreateLong name)
          with Declarations = content
               Attributes = [ createAttribute "AutoOpen" ] }

let private createOpen namespaceOrModule =
    LongIdentWithDots.CreateString(namespaceOrModule) |>
    SynModuleDecl.CreateOpen

let private inheritType name =
    SynMemberDefn.ImplicitInherit (SynType.Create name, SynExpr.CreateUnit, None, range.Zero)

let private implicitCtor () =
    SynMemberDefn.CreateImplicitCtor()

let private createPattern name args =
    SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), args)
    
let private createPatternTyped name args (typeName : string) =
    SynPatRcd.CreateTyped(SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString(name), args),
                          SynType.CreateLongIdent(typeName))

let private createMember name args attrs expr =
    let letBinding =
        { SynBindingRcd.Null with
              Pattern = createPattern ("_." + name) args
              Expr = expr
              Attributes = attrs }

    SynMemberDefn.CreateMember(letBinding)
    
let private createYield args =
    [
        LongIdentWithDots.CreateString("KubernetesResource.Zero") |> SynExpr.CreateLongIdent
        args
    ] |>
    SynExpr.CreateTuple |>
    createMember "Yield" [SynPatRcd.CreateWild] []
    
let private createTuple items withParen =
    if withParen then
        SynPatRcd.CreateParen(SynPatRcd.CreateTuple(items))
    else
        SynPatRcd.CreateTuple(items)
    
let private argsTuple withParen typeName =
    createTuple [ createPattern "_cargs_" []
                  createPatternTyped "_args_" [] typeName ] withParen
    
let private createRun typeName =
    createMember "Run" [argsTuple true typeName] []

let private (|FirstLetter|) (p:string) =
    p.[0], (p.Substring(1))
    
let private changeInitial change value =
    let (FirstLetter(x, xs)) =
        value
    
    sprintf "%c%s" (change x) xs

let private toSnakeCase =
    changeInitial Char.ToLower
    
let private toPascalCase =
    changeInitial Char.ToUpper
    
let private createOperation' name coName argName typeName hasAttribute =
    let attributes =
        if hasAttribute then
            [ createAttributeWithArg "CustomOperation" coName ]
        else
            []
    
    createMember name [createTuple [ argsTuple true typeName; createPattern argName [] ] true] attributes
    
let private createOperation name typeName hasAttribute =
    let snakeCaseName =
        toSnakeCase name
    
    let coName =
        match snakeCaseName with
        | "resourceGroupName" -> "resourceGroup"
        | "name" -> "resourceName"
        | x -> x
    
    createOperation' (coName |> toPascalCase) coName snakeCaseName typeName hasAttribute

let private createArgsReturn _ =
    SynExpr.CreateIdentString("_args_")
//    createTuple [createPatternTyped "args" [] typeName] 

let private createInstance name args =
    let identifier =
        LongIdentWithDots.CreateString name |>
        SynExpr.CreateLongIdent
        
    SynExpr.CreateApp(identifier, args)
    
let private createOperationsFor name pType argsType tupleArgs =
    let setRights =
        match pType with
        | "string"
        | "integer"
        | "number"
        | "boolean" -> [ SynExpr.CreateIdentString("input"); SynExpr.CreateIdentString("io") ]
        | "oneOf" -> [ SynExpr.CreateIdentString("inputUnion1Of2"); SynExpr.CreateIdentString("inputUnion2Of2") ]
        | "array" -> [ SynExpr.CreateIdentString("inputList") ]
        | "object" -> [ SynExpr.CreateIdentString("inputMap") ]
        // What to do here?
        | "complex" -> [ SynExpr.CreateIdentString("input") ]
        | x -> (name, x) ||> sprintf "Missing match case: %s, %s" |> failwith
    
    let setExpr setRight =
        SynExpr.Set (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("_args_." + name)),
                     SynExpr.CreateApp(setRight),
                     range.Zero)
        
    let expr setExpr =
        SynExpr.CreateSequential([
            setExpr
            SynExpr.CreateTuple(tupleArgs (SynExpr.CreateIdentString("_cargs_")))
        ])        
        
    setRights 
    |> List.map (fun sr -> sr, SynExpr.CreateIdentString(name |> toSnakeCase)) 
    |> List.map (setExpr >> expr)
    |> List.mapi (fun i e -> createOperation name argsType (i = 0) e) 
    |> Array.ofList
    
let private createBuilderClass createRunReturn name props =
    let typeName =
        name + "Builder" |>
        Ident.CreateLong
        
    let tupleArgs fst =
        [fst
         SynExpr.CreateIdentString("_args_")]
       
    let runArgs =
        SynExpr.CreateParenedTuple(tupleArgs (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString ("_cargs_.Name"))))
        
    let argsType =
        name + "Args"
        
    let operations =
        props |>
        Array.collect (fun (prop, t) -> createOperationsFor (prop |> toPascalCase) t argsType tupleArgs) |>
        List.ofArray
    
    let field = RecordFieldName(LongIdentWithDots.CreateString("Name"), true)
    let updates = [(field, SynExpr.CreateIdentString("name") |> Some, None)]
    let recordUpdate = SynExpr.CreateRecordUpdate(SynExpr.CreateIdentString("_cargs_"), updates)    
    let returnTuple = SynExpr.CreateTuple(tupleArgs (recordUpdate))
    
    SynModuleDecl.CreateType(SynComponentInfoRcd.Create(typeName),
                             [
                                 implicitCtor ()
                                 
                                 createYield (createInstance argsType SynExpr.CreateUnit)
                                 createRun argsType (createRunReturn name runArgs argsType)
                                 createOperation' "Name" "name" "name" argsType true returnTuple
                             ] @ operations)

let private createLet name expr =
    SynModuleDecl.CreateLet
        [ { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString name, [])
                Expr = expr } ]
                
#nowarn "25"

let private createType (namespaceMap: Map<string, string>) propertiesField (fqType : string, jValue : JsonValue) =
    let getComplexType (v : JsonValue) =
        // provider.Types.JsonValue.Properties() 
        // |> Array.tryFind (fun (t, _) -> ("#/types/" + t) = v.AsString())
        // |> ignore
        "complex"
    
    let [| tProvider; category; resourceType |] = fqType.Split(":")
    let typeName = toPascalCase resourceType
    let properties = jValue.GetProperty(propertiesField).Properties()

    let serviceProvider = namespaceMap.[category]
    
    let ns = sprintf "Pulumi.%s.%s" (toPascalCase tProvider) serviceProvider

    let nameAndType (name, jValue : JsonValue) =
        try
            let tName =
                match jValue.Properties() |>
                      Array.tryFind (fun (p, _) -> p = "language") |>
                      Option.bind (fun (_, v) -> v.Properties() |>
                                                 Array.tryFind (fun (p, _) -> p = "csharp") |>
                                                 Option.map snd) |>
                      Option.map (fun v -> v.GetProperty("name").AsString()) with
                | Some name -> name
                | None      -> name

            let pType =
                jValue.Properties()
                |> Array.choose (fun (p, v) ->
                    match p with
                    | "type" -> v.AsString() |> Some // Array type has also "items"
                    | "$ref" -> getComplexType v |> Some
                    | "oneOf" -> "oneOf" |> Some 
                    (*| "description"*)
                    | _ -> None)
                |> Array.head
            
            (tName, pType)
        with
        | ex ->
            let message = sprintf "Failed to create type for %A: %A" fqType jValue
            raise (Exception(message, ex))
    
    ns, typeName, properties, nameAndType, serviceProvider

[<MyriadGenerator("k8sgenerator")>]
type K8sGenerator() =
    interface IMyriadGenerator with
        member __.Generate(namespace', _) =
            let provider = PulumiProvider.GetSample()
            
            let moduleWithType createRunReturn (ns, typeName, properties, nameAndType, (fullServiceProvider: string)) =
                let [|serviceProvider; version|] = fullServiceProvider.Split(".")
                let moduleName = typeName
                (serviceProvider, version), (moduleName, [
                    createAutoOpenModule (moduleName) [
                        createOpen ns
                        createOpen (sprintf "Pulumi.Kubernetes.Types.Inputs.%s" fullServiceProvider)
                        createBuilderClass createRunReturn typeName (properties |> Array.map (nameAndType))
                        createLet (toSnakeCase typeName) (createInstance (typeName + "Builder") SynExpr.CreateUnit)             
                    ]
                ])
            
            let concatModules (moduleName: string) modules =
                createModule moduleName (modules |> List.concat)

            let resourceTypeNames = 
                provider.Resources.JsonValue.Properties()
                |> Array.map fst
                |> List.ofArray

            let typesToIgnore = 
                [
                    "ControllerRevision"
                    "CustomResourceSubresources" // json
                    "JSONSchemaProps"
                    "ManagedFieldsEntry"
                    // "RollingUpdateDaemonSet" // OneOf
                    // "RollingUpdateDeployment"
                    // "HTTPGetAction"
                    // "ServicePort"
                    // "TCPSocketAction"
                    // "IngressBackend"
                    // "NetworkPolicyPort"
                    // "PodDisruptionBudgetSpec"
                    "DeploymentRollback" // Deprecated
                    "Scale" // Not sure
                    "ScaleSpec"
                    "ScaleStatus"
                    "TokenRequestStatus"
                    "UserInfo"
                    "TokenReviewStatus"
                    "NonResourceRule"
                    "ResourceRule"
                    "SubjectAccessReviewStatus"
                    "APIGroup"
                    "APIResource"
                    "APIResourceList"
                    "APIVersions"
                    "APIGroupList"
                    "DeleteOptions"
                    "GroupVersionForDiscovery"
                    "Preconditions"
                    "ServerAddressByClientCIDR"
                    "WatchEvent"
                    "SubjectRulesReviewStatus"
                    "Info" // Unknown namespace?
                    "Eviction"
                ]

            let typesToInclude = 
                [
//                    "RollingUpdateDaemonSet"
                    // "Deployment"
                    // "DeploymentSpec"
                ]
//            let typeModules = []
                
            let namespaceMap =
                provider.Language.Csharp.Namespaces.JsonValue.Properties()
                |> Array.map (fun (p, jv) -> (p, jv.AsString()))
                |> Map.ofArray

            let toList i = [i]
            let typeModules = 
                provider.Types.JsonValue.Properties()
                |> Array.filter (fun (n, _) -> resourceTypeNames |> List.contains n |> not)
                |> Array.Parallel.map (createType namespaceMap "properties")
                |> Array.filter (fun (_, typeName, _, _, _) -> 
                    if List.isEmpty typesToInclude
                    then
                        List.contains typeName typesToIgnore |> not
                    else
                        List.contains typeName typesToIgnore |> not ||
                        typesToInclude |> List.contains typeName
                )
                |> Array.Parallel.map (moduleWithType (fun _ _ argsType -> createArgsReturn argsType))
                |> Array.groupBy (fst >> fst)
                |> Array.Parallel.map (fun (serviceProvider, grouped) ->
                    let innerModules =
                        grouped
                        |> Array.groupBy (fst >> snd)
                        |> Array.map (fun (version, groupedModules) ->
                            let modules = groupedModules |> Array.map (snd >> snd)
                            concatModules version (modules)                    
                        )
                        |> List.ofArray
                    createModule serviceProvider innerModules)
                |> List.ofArray
                |> createModule "Inputs"
                |> toList
                |> createModule "Types"
                |> toList

            let resourceModules =
                provider.Resources.JsonValue.Properties()
                |> Array.Parallel.map (createType namespaceMap "inputProperties")
                |> Array.filter (fun (_, typeName, _, _, _) -> 
                    if List.isEmpty typesToInclude
                    then
                        List.contains typeName typesToIgnore |> not
                    else
                        List.contains typeName typesToIgnore |> not ||
                        typesToInclude |> List.contains typeName
                )
                |> Array.Parallel.map (moduleWithType (fun name args _ -> createInstance name args))
                |> Array.groupBy (fst >> fst)
                |> Array.Parallel.map (fun (serviceProvider, grouped) ->
                    let innerModules =
                        grouped
                        |> Array.groupBy (fst >> snd)
                        |> Array.map (fun (version, groupedModules) ->
                            let modules = groupedModules |> Array.map (snd >> snd)
                            concatModules version (modules)                    
                        )
                        |> List.ofArray
                    createModule serviceProvider innerModules)
                |> List.ofArray

            let namespacesToOpen = 
                [
                    "Pulumi.FSharp"
                    "Pulumi.FSharp.Kubernetes"
                ]
                |> List.map createOpen

            createNamespace ("Pulumi.FSharp.Kubernetes") (namespacesToOpen @ typeModules @ resourceModules)