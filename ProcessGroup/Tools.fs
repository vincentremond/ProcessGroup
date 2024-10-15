module ProcessGroup.Tools

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

let rec (|NestedException|_|) (e: exn) =
    match e with
    | null -> None
    | :? 'a as e -> Some e
    | e -> (|NestedException|_|) e.InnerException

let expect expected actual =
    if expected <> actual then
        failwithf $"Expected %A{expected} but got %A{actual}"
