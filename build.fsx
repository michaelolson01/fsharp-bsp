// Workflow version: 1.0.0
#if FAKE // disable ionide / fake error
#r "paket: groupref Fake //"
#endif
#load ".fake/build.fsx/intellisense.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open FSharp.Json
open Fake.JavaScript
open Fake.Tools

////////////////////////////////////////////////////////////////////////////////
// INPUT
////////////////////////////////////////////////////////////////////////////////

let cli = """
usage: build [options]
options:
-t, --target <target>			Run the given target (ignored if positional argument 'target' is given)
-s, --single-target <target>	Run only the specified target.
-h, --help						Show Help
--project <project/glob>		Run targets on a specified project
--only-updated					Only choose projects that are newer than published versions
--for-docker					For use when building in docker, don't check for things like git in the environment
"""
// TODO: maybe remove --for-docker and replace with just failing if it doesn't have required things, rather than opting out here with the flag

let args = (Context.forceFakeContext ()).Arguments // Get arguments from fake build
let parsedArgs = Docopt(cli).Parse(args) // Parse them according to Docopt cli
let onlyUpdatedArg = DocoptResult.hasFlag "--only-updated" parsedArgs // try get "only-updated" from fake arguments
let forDockerArg = DocoptResult.hasFlag "--for-docker" parsedArgs // try get "for-docker" from fake arguments
let projectArg = DocoptResult.tryGetArgument "--project" parsedArgs // try get chosen project from fake arguments

////////////////////////////////////////////////////////////////////////////////
// TYPES
////////////////////////////////////////////////////////////////////////////////

type BranchType =
    | Main // "main"
    | LongTermSupport of int // lts-v*
    | Feature of string // any other name

type ProjectType =
    | FSharp
    | Node
    | Library
    | Docker

type Project = {
    InternalName: String // name given to the project for internal use, derived from the project folder
    ExternalName: String // name given to the project for external use, derived from *.fsproj or package.json
    DirectoryPath: String
    Version: SemVerInfo
    Types: Set<ProjectType>
}

////////////////////////////////////////////////////////////////////////////////
// REFERENCE
////////////////////////////////////////////////////////////////////////////////

let rootDirectory = Directory.GetCurrentDirectory()

let outputPath = rootDirectory @@ "output"

let branch = if forDockerArg then None else Some(Git.Information.getBranchName rootDirectory)

let shortShaMaybe =
    if forDockerArg then None
    else Some(Git.CommandHelper.runSimpleGitCommand rootDirectory $"rev-parse --short {Git.Information.getCurrentSHA1 rootDirectory}") // get the short sha from git

let branchTypeMaybe =
    Option.map (fun branch' ->
        let ltsMatch = Regex.Match(branch', "lts-v(\d+)")
        
        if branch' = "main" then Main
        elif ltsMatch.Success then LongTermSupport(int (ltsMatch.Groups.[1].Value))
        else Feature(branch')
    ) branch

// get variables from a .env file, if it exists
rootDirectory @@ ".env"
|> File.Exists
|> function
    | false -> printf "No .env file found."
    | true ->
        rootDirectory @@ ".env"
        |> File.ReadAllLines
        |> Seq.iter (fun line ->
            match line.Split([|"="|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
            | key :: value -> Environment.SetEnvironmentVariable(key, String.concat "=" value) // put strings back together that had multiple "="'s (e.g. connection strings)
            | _ -> ()
        )

let projects =
    !! "projects/*/CHANGELOG.md" // everything with a CHANGELOG.md is a project
    |> Seq.map (fun projectChangelog ->
        let internalName = projectChangelog |> Path.GetDirectoryName |> Path.GetFileName
        let directory = rootDirectory @@ "projects" @@ internalName // get the project root
        let version = SemVer.parse (ReleaseNotes.load (directory @@ "CHANGELOG.md")).NugetVersion
        
        let isFSharpProject = !! (directory @@ "src" @@ "*.fsproj") |> Seq.tryHead |> Option.isSome
        let isDockerOrLibrary = File.Exists (directory @@ $"Dockerfile")
        let isNodeProject = File.Exists (directory @@ $"package.json")
        
        let externalName =
            if isFSharpProject then
                !! (directory @@ $"src/*.fsproj") |> Seq.head |> System.IO.Path.GetFileNameWithoutExtension
            elif isNodeProject then
                System.IO.File.ReadAllText (directory @@ "package.json")
                |> Json.deserialize<{|name: string|}> // try to get the project name from the package.json
                |> fun json -> json.name
            else
                directory |> System.IO.Path.GetFileNameWithoutExtension
        
        let types = Set [
            if isFSharpProject then FSharp
            if isNodeProject then Node
            if isDockerOrLibrary then Docker else Library
        ]
        
        {
            InternalName = internalName
            ExternalName = externalName
            DirectoryPath = directory
            Version = version
            Types = types
        }
    )
    |> Seq.toList

let isProjectUpdated project =
    let (_, tags, _) = Git.CommandHelper.runGitCommand rootDirectory $"tag --list --sort=-version:refname \"{project.InternalName}/*\"" // ask git to get the list of tags
    tags
    |> List.choose (fun tag ->
        let text = tag |> String.split '/' |> List.item 1 // extract the tag
        if SemVer.isValid text then Some(SemVer.parse text) else None // only keep if it's a valid semver
    )
    |> function
    | [] -> true // if there are no existing tags, this project must be new
    | tag :: _ -> tag < project.Version // if there are tags, this project is updated if it is newer than the tag // FIXME: does not work for LTS

let getSelectedProjects projectTypes =
    projects
    |> fun filteredProjects ->
        if not <| List.isEmpty projectTypes // if there are given project types
        then List.filter (fun project -> Set.isSubset (Set projectTypes) project.Types) filteredProjects // get all buildable projects
        else filteredProjects
    |> fun filteredProjects ->
        match projectArg with // if there is a project specified from FAKE cli
        | Some(specificProject) -> List.filter (fun project -> project.InternalName = specificProject || project.ExternalName = specificProject) filteredProjects // filter for projects with that name
        | None -> filteredProjects
    |> fun filteredProjects ->
        if onlyUpdatedArg
        then List.filter isProjectUpdated filteredProjects
        else filteredProjects

let getFSharpProjectFilePath project = project.DirectoryPath @@ $"src/{project.ExternalName}.fsproj"

let getFSharpTestProjectFilePath project = project.DirectoryPath @@ $"test/{project.ExternalName}.Tests.fsproj"

////////////////////////////////////////////////////////////////////////////////
// TARGETS
////////////////////////////////////////////////////////////////////////////////
Target.initEnvironment ()

/// Compare all project changelogs with corresponding git tags. If any changelog has been updated, pass the test
let ensureAProjectIsUpdated _ =
    // TODO: maybe also check if there are updated files in the src directories & put a warning if there are & no changelog updates
    getSelectedProjects []
    |> List.exists (fun project -> isProjectUpdated project)
    |> fun atLeastOneVersionBump ->
        if not atLeastOneVersionBump then
            match projectArg with
            | Some(project) -> failwith $"{project} changelog has not been updated!"
            | None -> failwith "No changelogs have been updated!"

let cleanFSharpProjects _ =
    Shell.cleanDir outputPath
    
    getSelectedProjects [FSharp]
    |> List.iter (fun project ->
        [
            project.DirectoryPath @@ "src/bin"
            project.DirectoryPath @@ "src/obj"
            if getFSharpTestProjectFilePath project |> File.Exists then
                project.DirectoryPath @@ "test/bin"
                project.DirectoryPath @@ "test/obj"
        ]
        |> Shell.cleanDirs
    )

let restoreFSharpProjects _ =
    // TODO: run top-level tests (when we have a top-level test folder)
    getSelectedProjects [FSharp]
    |> Seq.iter (fun project ->
        DotNet.restore id (getFSharpProjectFilePath project) // restore the project
        let localTestProject = (getFSharpTestProjectFilePath project)
        if File.exists localTestProject then DotNet.restore id localTestProject // try to restore the test project too
    )

let buildFSharpProjects _ =
    getSelectedProjects [FSharp]
    |> Seq.iter (fun project ->
        DotNet.build (fun options ->
            {options with
                NoRestore = true
                MSBuildParams =
                    {options.MSBuildParams with
                        Targets = ["Build"]
                        Properties = [
                            ("Optimize", "True")
                            ("Version", project.Version.ToString())
                        ]
                    }
            }
        ) (getFSharpProjectFilePath project)
    )

let buildExecutableForFSharpProjects _ =
    getSelectedProjects [FSharp; Docker]
    |> List.iter (fun project ->
        let projectFilePath = getFSharpProjectFilePath project
        
        // build for no-self-contained net6.0
        DotNet.publish (fun options ->
            {options with
                NoBuild = true // this always follows the "BuildFSharpProjects" target
                NoRestore = true // this is done in the "RestoreFSharpProjects" target
                Configuration = DotNet.BuildConfiguration.Release
                OutputPath = Some(outputPath @@ "executables" @@ $"{project.ExternalName}.{project.Version}" @@ "not self-contained - net6.0") // put it here when it's done
                Framework = Some("net6.0")
                SelfContained = Some(false)
            }
        ) projectFilePath
        
        // build for self-contained net6.0
        DotNet.publish (fun options ->
            {options with
                NoBuild = true // this always follows the "BuildFSharpProjects" target
                NoRestore = true // this is done in the "RestoreFSharpProjects" target
                Configuration = DotNet.BuildConfiguration.Release
                OutputPath = Some(outputPath @@ "executables" @@ $"{project.ExternalName}.{project.Version}" @@ "self-contained - net6.0") // put it here when it's done
                Framework = Some("net6.0")
                SelfContained = Some(true)
            }
        ) projectFilePath
    )

let buildNugetFileForFSharpProjects _ =
    getSelectedProjects [FSharp; Library] // get every project with a template file
    |> Seq.iter (fun project ->
        Paket.pack (fun defaultOptions -> // package the individual project
            {defaultOptions with
                ToolType = ToolType.CreateLocalTool ()
                TemplateFile = project.DirectoryPath @@ "src" @@ "paket.template"
                OutputPath = outputPath @@ "packages"
                Version = project.Version.ToString()
                // ReleaseNotes = release.Notes |> String.toLines // TODO
            }
        )
    )

let packageFSharpProjects _ =
    buildExecutableForFSharpProjects ()
    buildNugetFileForFSharpProjects ()

let testFSharpProjects _ =
    getSelectedProjects [FSharp]
    |> List.iter (fun project ->
        if getFSharpTestProjectFilePath project |> File.Exists then
            DotNet.build (fun options ->
                {options with
                    NoLogo = true
                    Configuration = DotNet.BuildConfiguration.Debug
                }
            ) (getFSharpTestProjectFilePath project)
            
            !! $"{project.DirectoryPath}/test/bin/Debug/net*/*.Tests.dll" // glob for the test dll instead of an exact path to allow for multiple dotnet versions // FIXME: this relies on the convention of .Tests at the end of the .fsproj name, it shouldn't
            |> Set
            |> Expecto.run (fun defaultOptions ->
                {defaultOptions with
                    WorkingDirectory = rootDirectory // Use root directory for environment variables
                }
            )
    )

let lintFSharpProjects _ =
    let fsharplintPath = rootDirectory @@ "paket-files/gist.githubusercontent.com/fsharplint.json"
    getSelectedProjects [FSharp]
    |> Seq.iter (fun project ->
        DotNet.exec (fun options -> {options with RedirectOutput = true}) "fsharplint" $"lint -l {fsharplintPath} {getFSharpProjectFilePath project}"
        |> fun result -> if result.OK then () else failwith "Linting Errors"
        if getFSharpTestProjectFilePath project |> File.Exists then
            DotNet.exec (fun options -> {options with RedirectOutput = true}) "fsharplint" $"lint -l {fsharplintPath} {getFSharpTestProjectFilePath project}"
            |> fun result -> if result.OK then () else failwith "Linting Errors"
    )

let documentFSharpProjects _ =
    Shell.cleanDir ".fsdocs" // remove the docs cache if there is one
    let docsOutputPath = outputPath @@ "docs"
    DotNet.exec id "fsdocs" $"build --clean --output {docsOutputPath} --qualify --properties Configuration=Release" |> ignore

let setUpNodeProjects _ =
    getSelectedProjects [Node]
    |> List.iter (fun project ->
        Npm.install (fun o -> { o with WorkingDirectory = project.DirectoryPath })
    )

let testNodeProjects _ =
    getSelectedProjects [Node]
    |> List.iter (fun project ->
        Npm.test (fun o -> { o with WorkingDirectory = project.DirectoryPath })
    )

let lintNodeProjects _ =
    getSelectedProjects [Node]
    |> Seq.iter (fun project ->
        Npm.run "lint" (fun o -> { o with WorkingDirectory = project.DirectoryPath })
    )

let buildDockerWithArgs dockerArgs =
    let branchType = branchTypeMaybe |> Option.defaultWith (fun _ -> failwith "Cannot run this target with --for-docker tag!")
    let shortSha = shortShaMaybe |> Option.defaultWith (fun _ -> failwith "Cannot run this target with --for-docker tag!")
    let githubUsername = Environment.environVarOrFail("GITHUB_USERNAME")
    let githubPersonalAccessToken = Environment.environVarOrFail("GITHUB_PERSONAL_ACCESS_TOKEN")
    
    // ensure environment has docker installed and running
    CreateProcess.fromRawCommand "docker" ["info"]
    |> CreateProcess.ensureExitCodeWithMessage "Docker not installed or engine not running!"
    |> CreateProcess.redirectOutput // don't print docker info to console
    |> Proc.run
    |> ignore
    
    getSelectedProjects [Docker]
    |> List.iter (fun project ->
        
        let dockerTags = // calculate appropriate tags for this image depending on changelog version and branch
            match (branchType, project.Version.PreRelease) with
            | Main, None | LongTermSupport(_), None -> // for main and lts releases, publish tags for major and minor semver
                [
                    "-t"; $"aaosc/{project.ExternalName}:latest";
                    "-t"; $"aaosc/{project.ExternalName}:{project.Version.ToString()}";
                    "-t"; $"aaosc/{project.ExternalName}:{project.Version.Major}.{project.Version.Minor}";
                    "-t"; $"aaosc/{project.ExternalName}:{project.Version.Major}";
                ]
            | Feature(_), _ -> ["-t"; $"aaosc/{project.ExternalName}:{project.Version.ToString()}-{shortSha}";] // for features, publish single tag with shortSha attached
            | _, Some(_) -> ["-t"; $"aaosc/{project.ExternalName}:{project.Version.ToString()}";] // for pre-release versions, publish just one tag
        
        [
            "buildx"; "build";
            "-f"; $"{project.DirectoryPath}/Dockerfile";
            "--build-arg"; $"GITHUB_USERNAME={githubUsername}";
            "--build-arg"; $"GITHUB_PERSONAL_ACCESS_TOKEN={githubPersonalAccessToken}";
            "."
        ] @ dockerTags @ dockerArgs
        |> CreateProcess.fromRawCommand "docker"
        |> CreateProcess.addOnExited (fun _ exitCode ->
            if exitCode <> 0
            then failwith $"Error building docker image for project {project.InternalName}!"
            else ()
        )
        |> Proc.run
        |> ignore
    )

// Build docker images for this device and load them into docker
let buildDockerProjects _ =
    [
        "--load";
    ]
    |> buildDockerWithArgs
    // TODO: put this in a tar in your outputs

let packageDockerProjects _ =
    let branchType = branchTypeMaybe |> Option.defaultWith (fun _ -> failwith "Cannot run this target with --for-docker tag!")
    let shortSha = shortShaMaybe |> Option.defaultWith (fun _ -> failwith "Cannot run this target with --for-docker tag!")
    
    getSelectedProjects [Docker]
    |> List.iter (fun project ->
        let tagToSave =
            match (branchType, project.Version.PreRelease) with
                | Main, None | LongTermSupport(_), None -> $"aaosc/{project.ExternalName}:{project.Version.ToString()}" // for main and lts releases, publish tags for major and minor semver
                | Feature(_), _ -> $"aaosc/{project.ExternalName}:{project.Version.ToString()}-{shortSha}" // for features, publish single tag with shortSha attached
                | _, Some(_) -> $"aaosc/{project.ExternalName}:{project.Version.ToString()}" // for pre-release versions, publish just one tag
        
        [
            "save";
            "--output"; outputPath @@ $"{project.ExternalName}-{project.Version.ToString()}.tar"; // the tar file to write to
            tagToSave; // the tag to save (the specific version)
        ]
        |> CreateProcess.fromRawCommand "docker"
        |> CreateProcess.addOnExited (fun _ exitCode ->
            if exitCode <> 0
            then failwith $"Error saving docker image for project {project.InternalName}!"
            else ()
        )
        |> Proc.run
        |> ignore
    )

// Build docker images for amd64 and arm64 and push them to DockerHub
let publishDockerProjects _ =
    [
        "--platform"; "linux/amd64,linux/arm64";
        "--push";
    ]
    |> buildDockerWithArgs

let publishNewTagsForUpdatedProjects _ =
    match branchTypeMaybe with
    | None -> failwith "Cannot run this target with --for-docker tag!" // need git to make tags
    | Some(Feature(_)) -> failwith "Cannot run this target on feature branch!" // only tag on main and lts
    | _ -> ()
    if not onlyUpdatedArg then failwith "Cannot run this target without --only-updated tag!"
    
    getSelectedProjects []
    |> List.iter (fun project ->
        let tag = $"{project.ExternalName}/{project.Version.ToString()}"
        
        Git.Branches.tag rootDirectory tag
        Git.Branches.pushTag rootDirectory "origin" tag
    )

////////////////////////////////////////////////////////////////////////////////

// Nomenclature

// Produce = not used
// Compile = not used
// Build = used where it makes sense
// Package = produce a distributable thing of the project
// Push = not used
// Publish = put the distributable thing of the project on the internet for other people to download, therefore makes
//           the version official. This includes documentation.

// Setup

Target.create "EnsureAProjectIsUpdated" ensureAProjectIsUpdated

// F# Projects

Target.create "CleanFSharpProjects" cleanFSharpProjects

Target.create "RestoreFSharpProjects" restoreFSharpProjects
"RestoreFSharpProjects" <=? "CleanFSharpProjects" // Clean removes restored stuff, make sure restore happens after

Target.create "BuildFSharpProjects" buildFSharpProjects
"BuildFSharpProjects" <== ["CleanFSharpProjects"; "RestoreFSharpProjects"] // builds the projects as local release builds

Target.create "PackageFSharpProjects" packageFSharpProjects
"PackageFSharpProjects" <== ["BuildFSharpProjects"]

Target.create "TestFSharpProjects" testFSharpProjects
"TestFSharpProjects" <== ["CleanFSharpProjects"; "RestoreFSharpProjects"; "BuildFSharpProjects"]

Target.create "LintFSharpProjects" lintFSharpProjects
"LintFSharpProjects" <== ["CleanFSharpProjects"; "RestoreFSharpProjects"]

Target.create "DocumentFSharpProjects" documentFSharpProjects
"DocumentFSharpProjects" <== ["BuildFSharpProjects"]

Target.create "PublishFSharpLibraries" ignore // TODO: implement
"PublishFSharpLibraries" <== ["PackageFSharpProjects"]

// Node Projects

Target.create "SetUpNodeProjects" setUpNodeProjects

Target.create "TestNodeProjects" testNodeProjects
"TestNodeProjects" <== ["SetUpNodeProjects"]

Target.create "LintNodeProjects" lintNodeProjects
"LintNodeProjects" <== ["SetUpNodeProjects"]

Target.create "DocumentNodeProjects" ignore

Target.create "PackageNodeProjects" ignore

Target.create "PublishNodeProjects" ignore

// Docker Projects

Target.create "BuildDockerProjects" buildDockerProjects

Target.create "PackageDockerProjects" packageDockerProjects
"PackageDockerProjects" <== ["BuildDockerProjects"]

Target.create "PublishDockerProjects" publishDockerProjects

// All Projects

Target.create "PublishNewTagsForUpdatedProjects" publishNewTagsForUpdatedProjects
"PublishNewTagsForUpdatedProjects" <== ["EnsureAProjectIsUpdated"]

Target.create "CleanAllProjects" ignore
"CleanAllProjects" <== ["CleanFSharpProjects"]

Target.create "TestAllProjects" ignore
"TestAllProjects" <== ["TestFSharpProjects"; "TestNodeProjects"]

Target.create "LintAllProjects" ignore
"LintAllProjects" <== ["LintFSharpProjects"; "LintNodeProjects"]

Target.create "DocumentAllProjects" ignore
"DocumentAllProjects" <== ["DocumentFSharpProjects"; "DocumentNodeProjects"]

Target.create "PackageAllProjects" ignore
"PackageAllProjects" <== ["PackageFSharpProjects"; "PackageNodeProjects"; "PackageDockerProjects"]

Target.create "PublishAllProjects" ignore
"PublishAllProjects" <== ["PublishFSharpLibraries"; "PublishNodeProjects"; "PublishDockerProjects"]
"PublishNewTagsForUpdatedProjects" <=? "PublishAllProjects" // Wait to tag in case build fails

// Workflows

Target.create "PullRequestWorkflow" ignore
"PullRequestWorkflow" <== ["EnsureAProjectIsUpdated"; "LintAllProjects"; "TestAllProjects"]

Target.create "PushWorkflow" ignore
"PushWorkflow" <== ["PublishNewTagsForUpdatedProjects"; "PublishAllProjects"; "DocumentAllProjects"]

Target.create "DevelopmentWorkflow" ignore
"DevelopmentWorkflow" <== ["EnsureAProjectIsUpdated"; "LintAllProjects"; "TestAllProjects"]

// Aliases

Target.create "Clean" ignore
"Clean" <== ["CleanAllProjects"]

Target.create "Test" ignore
"Test" <== ["TestAllProjects"]

Target.create "Lint" ignore
"Lint" <== ["LintAllProjects"]

Target.create "Document" ignore
"Document" <== ["DocumentAllProjects"]

Target.create "Package" ignore
"Package" <== ["PackageAllProjects"]

Target.create "Pack" ignore
"Pack" <== ["Package"]

// Default

Target.runOrDefaultWithArguments "DevelopmentWorkflow"
