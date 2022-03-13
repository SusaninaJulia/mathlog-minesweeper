module minesweeper.Main

open minesweeper.Minesweeper

let field1 = [|[|    Empty;     Number(1);     Empty|];
               [|    Empty;     Empty;         Empty|];
               [|     Bomb;     Empty;     Number(1)|]|]

let field2 = [|[|    Empty;     Number(1);     Empty|];
               [|    Empty;     Empty;         Empty|];
               [|     Bomb;     Empty;     Number(2)|]|]

let field3 = [|[|    Empty;     Number(2);     Empty|];
               [|    Empty;     Empty;         Empty|];
               [|     Bomb;     Empty;     Number(1)|]|]

let field4 = [|[|    Empty;     Number(2);     Empty|];
               [|    Empty;     Empty;         Empty|];
               [|     Bomb;     Number(2);     Empty|]|]
let field5 = [|[|    Empty;     Number(3);     Empty|];
               [|    Empty;     Empty;         Empty|];
               [|     Bomb;     Empty;     Number(2)|]|]

let field6 = [|[|    Empty;     Number(5);     Empty|];
               [|    Empty;     Empty;         Empty|];
               [|     Bomb;     Empty;     Number(1)|]|]

let newTest1 = [|[|    Empty;    Number(1);    Empty;    Empty |]|]
let newTest2 = [|[|    Number(2);    Empty;    Empty |]|]

[<EntryPoint>]
let main argv =
    printfn "field №1:"
    DPLLForMinesweeper field1
    printfn "field №2:"
    DPLLForMinesweeper field2
    printfn "field №3:"
    DPLLForMinesweeper field3
    printfn "field №4:"
    DPLLForMinesweeper field4
    printfn "field №5:"
    DPLLForMinesweeper field5
    printfn "field №6:"
    DPLLForMinesweeper field6
    printfn ""
    
    printfn "field №1:"
    DPLLForMinesweeperCheckCell field1 0 0
    printfn "field №2:"
    DPLLForMinesweeperCheckCell field2 0 0
    printfn "field №3:"
    DPLLForMinesweeperCheckCell field3 0 0
    printfn "field №4:"
    DPLLForMinesweeperCheckCell field4 0 0
    printfn "field №5:"
    DPLLForMinesweeperCheckCell field5 0 0
    printfn "field №6:"
    DPLLForMinesweeperCheckCell field6 0 0
    
    printfn "newTest1:"
    DPLLForMinesweeper newTest1
    
    printfn "newTest2:"
    DPLLForMinesweeper newTest2
    
    0 
