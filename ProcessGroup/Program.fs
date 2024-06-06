open System
open System.ComponentModel
open System.Diagnostics
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Core
open Vanara.PInvoke
open Spectre.Console

[<RequireQualifiedAccess>]
module List =
    let split predicate list =

        List.foldBack
            (fun x (matching, notMatching) ->
                if predicate x then
                    (x :: matching, notMatching)
                else
                    (matching, x :: notMatching)
            )
            list
            ([], [])

type ProcessSummary = {
    Pid: int
    Name: string
    ParentPid: int option
    MemoryUsage: int64
    Priority: ProcessPriorityClass option
}

type ProcessTree = {
    Pid: int
    Name: string
    MemoryUsage: int64
    CumulativeMemoryUsage: int64
    Children: ProcessTree list
}

let rec (|NestedException|_|) (e: exn) =
    match e with
    | null -> None
    | :? 'a as e -> Some e
    | e -> (|NestedException|_|) e.InnerException

let getParentPid o : int option =
    try
        let processType = typeof<Process>

        let parentProcessId =
            processType
                .GetProperty("ParentProcessId", BindingFlags.Instance ||| BindingFlags.NonPublic)
                .GetGetMethod(true)

        let parentPid = parentProcessId.Invoke(o, [||]) :?> int
        Some parentPid
    with
    | NestedException(e: Win32Exception) -> None
    | NestedException(e: InvalidOperationException) when Regex.IsMatch(e.Message, "^Cannot process request because the process \\(\\d+\\) has exited") -> None

while true do

    AnsiConsole.Clear()

    let tryGetPriorityClass (p: Process) =
        try
            Some p.PriorityClass
        with NestedException(e: Win32Exception) ->
            None

    let processes =
        Process.GetProcesses()
        |> List.ofSeq
        |> List.map (fun p -> {
            Pid = p.Id
            Name = p.ProcessName
            ParentPid = (getParentPid p)
            MemoryUsage = p.PrivateMemorySize64
            Priority = tryGetPriorityClass p
        })

    let forceNotParentProcesses =
        [
            "explorer"
            "PowerToys.PowerLauncher"
        ]
        |> List.choose (fun p -> processes |> List.tryFind (fun x -> x.Name = p))
        |> List.map (_.Pid)
        |> Set.ofList

    let allPids = processes |> List.map (_.Pid) |> Set.ofSeq

    let rec createProcessTree parentPid orphanProcesses : ProcessTree list * ProcessSummary list =
        let matchingProcesses, orphans =
            orphanProcesses
            |> List.split (fun p ->
                match p.ParentPid, parentPid with
                | Some parentPid, None ->
                    let parentProcessNotFound = not <| (allPids |> Set.contains parentPid)
                    let forceNotParentProcess = forceNotParentProcesses |> Set.contains parentPid
                    parentProcessNotFound || forceNotParentProcess
                | _ -> p.ParentPid = parentPid
            )

        (orphans, matchingProcesses)
        ||> List.mapFold (fun orphans process_ ->
            let processTrees, stillOrphans = createProcessTree (Some process_.Pid) orphans

            let cumulativeMemoryUsage =
                process_.MemoryUsage + (processTrees |> List.sumBy (_.CumulativeMemoryUsage))

            {
                Pid = process_.Pid
                Name = process_.Name
                Children = processTrees
                MemoryUsage = process_.MemoryUsage
                CumulativeMemoryUsage = cumulativeMemoryUsage
            },
            stillOrphans
        )

    let rec updateProcessTreePriority nameCondition (processTrees: ProcessTree list) priority =

        let (|IsMatch|_|) (name: string) (nameCondition: string option) =
            match nameCondition with
            | None -> Some()
            | Some n when n = name -> Some()
            | _ -> None

        for processTree in processTrees do
            match nameCondition with
            | IsMatch processTree.Name ->
                let processById = Process.GetProcessById(processTree.Pid)

                if processById.PriorityClass <> priority then
                    printfn $"Setting priority for {processTree.Name} (Pid: {processTree.Pid}) to {priority}"
                    processById.PriorityClass <- priority

                updateProcessTreePriority None processTree.Children priority
            | _ -> ()

    let processTrees, processSummaries as result = createProcessTree None processes

    let printMemoryUsage (i: int64) =
        sprintf "%s MB" ((float i / 1024. / 1024.).ToString("0.00"))

    let printMemoryUsageU (i: uint64) =
        sprintf "%s MB" ((float i / 1024. / 1024.).ToString("0.00"))

    let totalMemoryUsage = processTrees |> List.sumBy (_.CumulativeMemoryUsage)
    let mutable memoryStatusEx = Kernel32.MEMORYSTATUSEX.Default
    let globalMemoryStatusEx = Kernel32.GlobalMemoryStatusEx(&memoryStatusEx)

    let percentMemoryUsage =
        ((float totalMemoryUsage / float memoryStatusEx.ullTotalPhys) * 100.)
            .ToString("0.0")

    let rec printProcessTree depth processTree =
        let percentMemoryUsage =
            ((float processTree.CumulativeMemoryUsage / float memoryStatusEx.ullTotalPhys)
             * 100.)

        let color =
            if percentMemoryUsage > 30. then "red"
            elif percentMemoryUsage > 20. then "darkorange"
            elif percentMemoryUsage > 10. then "yellow"
            else "grey"

        let paddingLeft = (String.replicate depth "    ")
        let memoryUsage = printMemoryUsage processTree.CumulativeMemoryUsage

        // printfn $"%s{paddingLeft}%s{processTree.Name} (Pid: {processTree.Pid}) {memoryUsage}"
        AnsiConsole.MarkupLineInterpolated
            $"[bold]{paddingLeft}{processTree.Name}[/] (Pid: {processTree.Pid}) [{color}]{memoryUsage}[/] {percentMemoryUsage:f2} %%"

        processTree.Children
        |> List.sortByDescending (_.CumulativeMemoryUsage)
        |> List.iter (printProcessTree (depth + 1))

    updateProcessTreePriority (Some "rider64") processTrees ProcessPriorityClass.AboveNormal
    updateProcessTreePriority (Some "ms-teams") processTrees ProcessPriorityClass.BelowNormal
    updateProcessTreePriority (Some "olk") processTrees ProcessPriorityClass.BelowNormal // Outlook
    

    printfn "----------------------"
    printfn $"Total memory usage    : {printMemoryUsage totalMemoryUsage} ({percentMemoryUsage}%%)"
    printfn $"Total physical memory : {printMemoryUsageU memoryStatusEx.ullTotalPhys}"
    printfn $"Total virtual memory  : {printMemoryUsageU memoryStatusEx.ullTotalVirtual}"
    printfn "----------------------"

    processTrees
    |> List.sortByDescending (_.CumulativeMemoryUsage)
    |> List.iter (printProcessTree 0)

    printfn "----------------------"

    if processSummaries.Length > 0 then
        printfn "Orphan processes:"

        processSummaries
        |> List.iter (fun p -> printfn $"  Name: {p.Name}, Pid: {p.Pid}, ParentPid: {p.ParentPid}")

        printfn "----------------------"

    printfn "Press Enter to refresh"
    Console.ReadLine() |> ignore
