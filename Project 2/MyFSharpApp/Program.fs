open System

module BinaryOperations =
    let padTo8 (bin: int list) =
        let len = List.length bin
        if len < 10 then
            List.init (10 - len) (fun _ -> 0) @ bin
        elif len > 10 then
            bin |> List.skip (len - 10)
        else
            bin

    let unsignedToBin (n: int) : int list =
        let rec toBin x acc =
            if x = 0 then acc
            else toBin (x / 2) ((x % 2)::acc)
        let bin = if n = 0 then [0] else toBin n []
        padTo8 bin

    let notBinary (bin: int list) : int list =
        List.map (fun b -> if b = 0 then 1 else 0) bin

    let addBinaryLists (a: int list) (b: int list) : int list =
        let rec addHelper aRev bRev carry acc =
            match (aRev, bRev) with
            | ([], []) ->
                if carry = 1 then carry::acc else acc
            | (ha::ta, hb::tb) ->
                let sum = ha + hb + carry
                let digit = sum % 2
                let newCarry = sum / 2
                addHelper ta tb newCarry (digit::acc)
            | _ -> failwith "Lists must be of equal length"
        let aRev = List.rev a
        let bRev = List.rev b
        let sumRev = addHelper aRev bRev 0 []
        padTo8 sumRev

    let twosComplement (bin: int list) : int list =
        let inverted = notBinary bin
        addBinaryLists inverted (unsignedToBin 1)

    let signedToBin (n: int) : int list =
        if n >= 0 then
            unsignedToBin n
        else
            let posBin = unsignedToBin (-n)
            twosComplement posBin

    let binToSignedInt (bin: int list) : int =
        let powers = [512; 256; 128; 64; 32; 16; 8; 4; 2; 1]
        if List.head bin = 0 then
            List.fold2 (fun acc bit power -> acc + bit * power) 0 bin powers
        else
            let posBin = twosComplement bin
            let magnitude = List.fold2 (fun acc bit power -> acc + bit * power) 0 posBin powers
            -magnitude

    let andBinary (a: int list) (b: int list) : int list =
        List.map2 (fun x y -> if x = 1 && y = 1 then 1 else 0) a b

    let orBinary (a: int list) (b: int list) : int list =
        List.map2 (fun x y -> if x = 1 || y = 1 then 1 else 0) a b

    let xorBinary (a: int list) (b: int list) : int list =
        List.map2 (fun x y -> if x = y then 0 else 1) a b

    let subtractBinaryLists (a: int list) (b: int list) : int list =
        let bComplement = twosComplement b
        addBinaryLists a bComplement

module Operations =
    open BinaryOperations

    let addSigned (a: int) (b: int) =
        let binA = signedToBin a
        let binB = signedToBin b
        let sumBin = addBinaryLists binA binB
        let result = binToSignedInt sumBin
        (binA, binB, sumBin, result)

    let subtractSigned (a: int) (b: int) =
        let binA = signedToBin a
        let binB = signedToBin b
        let diffBin = subtractBinaryLists binA binB
        let result = binToSignedInt diffBin
        (binA, binB, diffBin, result)

open BinaryOperations
open Operations

[<EntryPoint>]
let main argv =
    
    let binToString bin =
        bin |> List.map string |> String.concat ""

    let parseBinary (s: string) : int list =
        if s |> Seq.forall (fun c -> c = '0' || c = '1') then
            s.ToCharArray()
            |> Array.map (fun c -> int c - int '0')
            |> Array.toList
            |> padTo8
        else
            invalidArg "s" "Invalid binary value. Input must consist of 0s and 1s only."

    let rec askInputType () =
        printf "Enter input type (INTEGER, BINARY, or QUIT): "
        let inputType = Console.ReadLine().Trim().ToUpper()
        match inputType with
        | "INTEGER" | "BINARY" -> inputType
        | "QUIT" -> 
            printfn "Exiting program."
            "QUIT" 
        | _ ->
            printfn "Invalid input type. Please enter INTEGER, BINARY, or QUIT."
            askInputType()

    let rec mainLoop () =
        let inputType = askInputType()
        
        if inputType = "QUIT" then
            0 // Exit the program
        else
            printf "\nEnter the operation to perform (NOT, AND, OR, XOR, ADD, SUB): "
            let op = Console.ReadLine().Trim().ToUpper()
            match op with
            
            | "NOT" ->
                if inputType = "INTEGER" then
                    printf "Enter an integer value: "
                    let input = Console.ReadLine().Trim()
                    try
                        let num = Convert.ToInt32(input)
                        let bin = signedToBin num
                        let resultBin = notBinary bin
                        printfn "Input binary: %s" (binToString bin)
                        printfn "NOT result:   %s" (binToString resultBin)
                        printfn "Result as signed int: %d" (binToSignedInt resultBin)
                    with
                    | ex -> printfn "Error: %s" ex.Message
                else
                    printf "Enter a binary value (e.g., 110001): "
                    let input = Console.ReadLine().Trim()
                    try
                        let bin = parseBinary input
                        let resultBin = notBinary bin
                        printfn "Input binary: %s" (binToString bin)
                        printfn "NOT result:   %s" (binToString resultBin)
                        printfn "Result as signed int: %d" (binToSignedInt resultBin)
                    with
                    | ex -> printfn "Error: %s" ex.Message
                mainLoop()
            | "AND" | "OR" | "XOR" ->
                printf "Enter first value: "
                let input1 = Console.ReadLine().Trim()
                printf "Enter second value: "
                let input2 = Console.ReadLine().Trim()
                try
                    let bin1, bin2 =
                        if inputType = "INTEGER" then
                            let num1 = Convert.ToInt32(input1)
                            let num2 = Convert.ToInt32(input2)
                            (signedToBin num1, signedToBin num2)
                        else
                            (parseBinary input1, parseBinary input2)
                    let resultBin =
                        match op with
                        | "AND" -> andBinary bin1 bin2
                        | "OR" -> orBinary bin1 bin2
                        | "XOR" -> xorBinary bin1 bin2
                        | _ -> failwith "Invalid operation"
                    printfn "First value binary:  %s" (binToString bin1)
                    printfn "Second value binary: %s" (binToString bin2)
                    printfn "%s result:          %s" op (binToString resultBin)
                    printfn "Result as signed int: %d" (binToSignedInt resultBin)
                with
                | ex -> printfn "Error: %s" ex.Message
                mainLoop()
            | "ADD" | "SUB" ->
                printf "Enter first value: "
                let input1 = Console.ReadLine().Trim()
                printf "Enter second value: "
                let input2 = Console.ReadLine().Trim()
                try
                    let bin1, bin2 =
                        if inputType = "INTEGER" then
                            let num1 = Convert.ToInt32(input1)
                            let num2 = Convert.ToInt32(input2)
                            (signedToBin num1, signedToBin num2)
                        else
                            (parseBinary input1, parseBinary input2)
                    let resultBin, result =
                        match op with
                        | "ADD" ->
                            let sumBin = addBinaryLists bin1 bin2
                            (sumBin, binToSignedInt sumBin)
                        | "SUB" ->
                            let diffBin = subtractBinaryLists bin1 bin2
                            (diffBin, binToSignedInt diffBin)
                        | _ -> failwith "Invalid operation"
                    printfn "First value binary:  %s" (binToString bin1)
                    printfn "Second value binary: %s" (binToString bin2)
                    printfn "%s result:          %s -> %d" op (binToString resultBin) result
                with
                    | ex -> printfn "Error: %s" ex.Message
                mainLoop()
            | _ ->
                printfn "Invalid operation. Please try again."
                mainLoop()

    mainLoop()
    0