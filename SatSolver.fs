module minesweeper.Minesweeper

open minesweeper.SatSolver
open System.Collections.Generic

type Cell =
    | Number of int
    | Bomb
    | Empty
    
exception Error of string
   
let neighbourCells i j N M =
    [|(i-1, j-1); (i, j-1); (i+1, j-1); (i-1, j); (i+1, j); (i-1, j+1); (i, j+1); (i+1, j+1) |]
    |> Array.filter (fun (x, y) -> x >= 0 && x < N && y >= 0 && y < M )
    
let formulaForNumberCell k i j N M =
//    let R, C = N-1, M-1
    let nbrs = neighbourCells i j N M
    printfn "For %i %i %A" i j nbrs 
    let rec formulaForNumberCell k n current =
        let (x, y) = nbrs.[current]
        let next = current + 1
//        let next = 
//            let tmp = 
//                match (x, y) with
//                | (_, tmp) when tmp = j+1 -> if j = 0 then (x+1, 0) else (x+1, j-1)
//                | (_, tmp) when tmp = j -> if j = R then (x+1, j-1) else (x, y+1)
//                | (_, _) -> (x, y+1)
//            if tmp = (i, j) then (if j = R then (i+1, j-1) else (i, j+1)) else tmp
        let var = x*N+y+1
        if n = 1
        then 
            if k = n 
            then PV(var)
            elif k = 0
            then NOT(PV(var))
            else  failwith "UNSAT with Error: wrong configuration (number of bombs is greater that number of empty cells)"
        elif k = n 
        then AND(PV(var), formulaForNumberCell (n-1) (n-1) next)
        elif k = 0
        then AND(NOT(PV(var)), formulaForNumberCell 0 (n-1) next)
        else AND(OR(NOT(PV(var)), formulaForNumberCell (k-1) (n-1) next) , OR(PV(var), formulaForNumberCell k (n-1) next))
    formulaForNumberCell k nbrs.Length 0
//    match (i, j) with
//    | (0, 0) -> formulaForNumberCell k 3 (i, j+1)
//    | (0, tmp) when tmp = R -> formulaForNumberCell k 3 (i, j-1)
//    | (0, _) -> formulaForNumberCell k 5 (i, j-1)
//    | (tmp, 0) when tmp = C -> formulaForNumberCell k 3 (i-1, j)
//    | (tmp1, tmp2) when tmp1 = C && tmp2 = R -> formulaForNumberCell k 3 (i-1, j-1)
//    | (tmp, _) when tmp = C -> formulaForNumberCell k 5 (i-1, j-1)
//    | (_, 0) -> formulaForNumberCell k 5 (i-1, j)
//    | (_, tmp) when tmp = R -> formulaForNumberCell k 5 (i-1, j-1)
//    | (_, _) -> formulaForNumberCell k 8 (i-1, j-1)

    
let formulaForCell c i j N M = 
    match c with
    | Number nmb -> AND(formulaForNumberCell nmb i j N M, NOT(PV(i*N+j+1)))
    | Bomb -> PV(i*N+j+1)
    | Empty -> failwith "empty without formula!!"
    
let formulaForField (fld: array<array<Cell>>): int list list = 
    let N = (fld.[0]).Length
    let M = fld.Length
    let formulas = new List<Formula>()
    for i in 0 .. M - 1 do
        for j in 0 .. N - 1 do
            match fld.[i].[j] with
            | Empty -> ()
            | _ -> formulas.Add(formulaForCell fld.[i].[j] i j N M)
    TSEYTINFromArray (formulas.ToArray()) (M*N) 
        
let DPLLForMinesweeper field = 
    let nconjs = formulaForField field
    let (sat, model) = DPLL(nconjs, Set.empty) 
    if sat = SAT
    then 
        printfn "DPLL result: SAT"
        printfn "Model: %A" model
        model |> Seq.filter (fun x -> abs(x) <= (field.Length*field.[0].Length))|> Seq.iter (fun x -> printf "%d " x)
        printfn ""
    else printfn "DPLL result: UNSAT"
    
let DPLLForMinesweeperCheckCell (fld: array<array<Cell>>) i j =
    match fld.[i].[j] with
    | Empty -> fld.[i].[j] <- Bomb
    | Bomb -> ()
    | Number _ -> failwith "this cell is not empty or a bomb"
    DPLLForMinesweeper fld
