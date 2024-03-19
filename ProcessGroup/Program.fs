// For more information see https://aka.ms/fsharp-console-apps

open System
open System.ComponentModel
open System.Diagnostics
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Core
open Vanara.PInvoke

// open System
// open System.ComponentModel
// open System.Runtime.CompilerServices
// open System.Runtime.InteropServices
//
// [<Struct; StructLayout(LayoutKind.Explicit)>]
// type ProcessBasicInformation =
//     [<FieldOffset 0>]
//     val mutable ExitStatus: IntPtr
//
//     [<FieldOffset 1>]
//     val mutable PebBaseAddress: IntPtr
//
//     [<FieldOffset 2>]
//     val mutable AffinityMask: IntPtr
//
//     [<FieldOffset 3>]
//     val mutable BasePriority: IntPtr
//
//     [<FieldOffset 4>]
//     val mutable UniqueProcessId: IntPtr
//
//     [<FieldOffset 5>]
//     val mutable InheritedFromUniqueProcessId: IntPtr
//
// type ProcessExtensions() =
//
//     [<DllImport("ntdll.dll")>]
//     static extern Int32 NtQueryInformationProcess(
//         IntPtr processHandle,
//         Int32 processInformationClass,
//         ProcessBasicInformation& processInformation ,
//         Int32 processInformationLength,
//         Int32& returnLength
//     )
//
//     [<Extension>]
//     static member GetParentPid(p: Process) =
//         try
//             let h = p.Handle
//             let mutable parentPid = ProcessBasicInformation()
//             let mutable returnLength = 0
//             let ProcessBasicInformation = 0
//             let status = NtQueryInformationProcess(h, ProcessBasicInformation, &parentPid, Marshal.SizeOf(parentPid), &returnLength)
//
//             if status <> 0 then
//                 None
//             else
//                 Some(parentPid.InheritedFromUniqueProcessId.ToInt32())
//         with :? Win32Exception ->
//             Some -1

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
}

type ProcessTree = {
    Pid: int
    Name: string
    MemoryUsage: int64
    CumulativeMemoryUsage: int64
    Children: ProcessTree list
}

let getMemoryUsage (p: Process) = p.WorkingSet64

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

let processes =
    Process.GetProcesses()
    |> List.ofSeq
    |> List.map (fun p -> {
        Pid = p.Id
        Name = p.ProcessName
        ParentPid = (getParentPid p)
        MemoryUsage = p.PrivateMemorySize64
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

let rec blablabla parentPid orphanProcesses : ProcessTree list * ProcessSummary list =
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
        let processTrees, stillOrphans = blablabla (Some process_.Pid) orphans

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

let processTrees, processSummaries as result = blablabla None processes

let printMemoryUsage (i: int64) =
    sprintf "%s MB" ((float i / 1024. / 1024.).ToString("0.00"))

let printMemoryUsageU (i: uint64) =
    sprintf "%s MB" ((float i / 1024. / 1024.).ToString("0.00"))

let rec printProcessTree depth processTree =
    let paddingLeft = (String.replicate depth "  ")
    let memoryUsage = printMemoryUsage processTree.CumulativeMemoryUsage
    printfn $"%s{paddingLeft}%s{processTree.Name} (Pid: {processTree.Pid}) {memoryUsage}"

    processTree.Children
    |> List.sortByDescending (_.CumulativeMemoryUsage)
    |> List.iter (printProcessTree (depth + 1))

let totalMemoryUsage = processTrees |> List.sumBy (_.CumulativeMemoryUsage)
let mutable memoryStatusEx = Kernel32.MEMORYSTATUSEX.Default
let globalMemoryStatusEx = Kernel32.GlobalMemoryStatusEx(&memoryStatusEx)

let percentMemoryUsage =
    ((float totalMemoryUsage / float memoryStatusEx.ullTotalPhys) * 100.)
        .ToString("0.0")

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
