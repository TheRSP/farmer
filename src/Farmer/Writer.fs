module Farmer.Writer

open System.Text.Json
open System.IO
open System.Reflection
open System.Text.Encodings.Web

module TemplateGeneration =
    let processTemplate (template:ArmTemplate) = {|
        ``$schema`` = template.Schema
        contentVersion = "1.0.0.0"
        resources = template.Resources |> List.map(fun r -> r.JsonModel)
        parameters =
            template.Parameters
            |> List.map(fun (SecureParameter p) -> p, {| ``type`` = "securestring" |})
            |> Map.ofList
        outputs =
            template.Outputs
            |> List.map(fun (k, v) ->
                k, {| ``type`` = "string"
                      value = v |})
            |> Map.ofList
    |}

    let serialize data =
        let opts = JsonSerializerOptions()
        opts.WriteIndented <- true
        opts.IgnoreNullValues <- true
        opts.Encoder <- JavaScriptEncoder.UnsafeRelaxedJsonEscaping
        JsonSerializer.Serialize(data, opts)

let branding () =
    let version =
        Assembly
            .GetExecutingAssembly()
            .GetCustomAttribute<AssemblyInformationalVersionAttribute>()
            .InformationalVersion

    printfn "=================================================="
    printfn "Farmer %s" version
    printfn "Repeatable deployments in Azure made easy!"
    printfn "=================================================="

/// Returns a JSON string representing the supplied ARMTemplate.
let toJson =
    TemplateGeneration.processTemplate >> TemplateGeneration.serialize

/// Writes the provided JSON to a file based on the supplied template name. The postfix ".json" will automatically be added to the filename.
let toFile folder templateName json =
    let filename =
        let filename = $"{templateName}.json"
        Path.Combine(folder, filename)
    let directory = Path.GetDirectoryName filename
    if not (Directory.Exists directory) then
        Directory.CreateDirectory directory |> ignore
    File.WriteAllText(filename, json)
    filename

/// Converts the supplied ARMTemplate to JSON and then writes it out to the provided template name. The postfix ".json" will automatically be added to the filename.
let quickWrite templateName (deployment:#IDeploymentBuilder) =
    Deployment.getTemplate "farmer-resources" deployment
    |> toJson
    |> toFile "." templateName
    |> ignore