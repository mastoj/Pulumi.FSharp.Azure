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
    createTuple [ createPattern "cargs" []
                  createPatternTyped "args" [] typeName ] withParen
    
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
        | "array" -> [ SynExpr.CreateIdentString("inputList") ]
        | "object" -> [ SynExpr.CreateIdentString("inputMap") ]
        // What to do here?
        | "complex" -> [ SynExpr.CreateIdentString("input") ]
        | x -> (name, x) ||> sprintf "Missing match case: %s, %s" |> failwith
    
    let setExpr setRight =
        SynExpr.Set (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString("args." + name)),
                     SynExpr.CreateApp(setRight),
                     range.Zero)
        
    let expr setExpr =
        SynExpr.CreateSequential([
            setExpr
            SynExpr.CreateTuple(tupleArgs (SynExpr.CreateIdentString("cargs")))
        ])        
        
    setRights |>
    List.map (fun sr -> sr, SynExpr.CreateIdentString(name |> toSnakeCase)) |>
    List.map (setExpr >> expr) |>
    List.mapi (fun i e -> createOperation name argsType (i = 0) e) |>
    Array.ofList
    
let private createAzureBuilderClass name props =
    let typeName =
        name + "Builder" |>
        Ident.CreateLong
        
    let tupleArgs fst =
        [fst
         SynExpr.CreateIdentString("args")]
       
    let runArgs =
        SynExpr.CreateParenedTuple(tupleArgs (SynExpr.CreateLongIdent(LongIdentWithDots.CreateString ("cargs.Name"))))
        
    let argsType =
        name + "Args"
        
    let operations =
        props |>
        Array.collect (fun (prop, t) -> createOperationsFor (prop |> toPascalCase) t argsType tupleArgs) |>
        List.ofArray
    
    let field = RecordFieldName(LongIdentWithDots.CreateString("Name"), true)
    let updates = [(field, SynExpr.CreateIdentString("name") |> Some, None)]
    let recordUpdate = SynExpr.CreateRecordUpdate(SynExpr.CreateIdentString("cargs"), updates)    
    let returnTuple = SynExpr.CreateTuple(tupleArgs (recordUpdate))
    
    SynModuleDecl.CreateType(SynComponentInfoRcd.Create(typeName),
                             [
                                 implicitCtor ()
                                 //inheritType "AzureResource"
                                 
                                 createYield (createInstance argsType SynExpr.CreateUnit)
                                 createRun argsType (createInstance name runArgs)
                                 createOperation' "Name" "name" "name" argsType true returnTuple
                             ] @ operations)

let private createLet name expr =
    SynModuleDecl.CreateLet
        [ { SynBindingRcd.Let with
                Pattern = SynPatRcd.CreateLongIdent(LongIdentWithDots.CreateString name, [])
                Expr = expr } ]
                
#nowarn "25"

let private createType (provider : PulumiProvider.Root) (fqType : string, jValue : JsonValue) =
    let getComplexType (v : JsonValue) =
        provider.Types.JsonValue.Properties() |>
        Array.tryFind (fun (t, _) -> ("#/types/" + t) = v.AsString()) |>
        ignore
        "complex"
   
    let [| tProvider; category; resourceType |] = fqType.Split(":")
    let typeName = toPascalCase resourceType
    let properties = jValue.GetProperty("inputProperties").Properties()
    
    let serviceProvider =
        provider.Language.Csharp.Namespaces.JsonValue.Properties() 
        |> Array.find (fun (p, _) -> p = category)
        |> snd 
        |> (fun jv -> jv.AsString())
    
    let ns = sprintf "Pulumi.%s.%s" (toPascalCase tProvider) serviceProvider

    let nameAndType (name, jValue : JsonValue) =
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
            jValue.Properties() |>
            Array.choose (fun (p, v) -> match p with
                                        | "type" -> v.AsString() |> Some // Array type has also "items"
                                        | "$ref" -> getComplexType v |> Some
                                        (*| "description"*)
                                        | _ -> None) |>
            Array.head
        
        (tName, pType)
    
    ns, typeName, properties, nameAndType, serviceProvider

[<MyriadGenerator("k8sgenerator")>]
type K8sGenerator() =
    interface IMyriadGenerator with
        member __.Generate(namespace', _) =
            let provider = PulumiProvider.GetSample()
            
            let moduleWithType (ns, typeName, properties, nameAndType, (fullServiceProvider: string)) =
                let [|serviceProvider; version|] = fullServiceProvider.Split(".")
                let moduleName = typeName
                (serviceProvider, version), (moduleName, [
                    createAutoOpenModule (moduleName) [
                        createOpen ns
                        createOpen (sprintf "Pulumi.Kubernetes.Types.Inputs.%s" fullServiceProvider)
                        createAzureBuilderClass typeName (properties |> Array.map (nameAndType))
                        createLet (toSnakeCase typeName) (createInstance (typeName + "Builder") SynExpr.CreateUnit)             
                    ]
                ])
            
            let concatModules (moduleName: string) modules =
                createModule moduleName (modules |> List.concat)

            let typesToIgnore = [
                "ControllerRevision"
            ]

            let modules =
                provider.Resources.JsonValue.Properties()
                |> Array.map (createType provider)
                |> Array.filter (fun (_, typeName, _, _, _) -> 
                    List.contains typeName typesToIgnore |> not
                    )
                |> Array.map moduleWithType
                |> Array.groupBy (fst >> fst)
                |> Array.map (fun (serviceProvider, grouped) ->
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

            createNamespace ("Pulumi.FSharp.Kubernetes") (namespacesToOpen @ modules)