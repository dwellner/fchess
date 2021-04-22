module ConsoleUi

open Types
open Turn


let private getPositionName pos =
    let File file, Rank rank = pos
    sprintf "%c%d" "ABCDEFGH".[file-1] rank 


let getPieceName piece = 
        match piece with 
        | (White,King) -> "wK"
        | (White,Queen) -> "wQ"
        | (White,Bishop) -> "wB"
        | (White,Rook) -> "wR"
        | (White,Knight) -> "wN"
        | (White,Pawn) -> "wp"
        | (Black,King) -> "bK"
        | (Black,Queen) -> "bQ"
        | (Black,Bishop) -> "bB"
        | (Black,Rook) -> "bR"
        | (Black,Knight) -> "bN"
        | (Black,Pawn) -> "bp"


let printBoard (turn:Turn) : unit = 

    let previousMove = turn.previousMoves |> List.tryFind (fun _ -> true)
    
    let GetCellContent position =
        let content = match GetPieceOnPosition turn position with 
        | Some piece -> getPieceName piece
        | None -> "  "

        match previousMove with
        | Some (_, move,_) when move.NewPosition = position -> sprintf "*%s*" content
        | _ -> sprintf " %s " content

    for rank in  8 .. -1 .. 1 do
        printfn "|---------------------------------------|"

        for file in 1 .. 8 do
            printf "|%s" (GetCellContent (File file,Rank rank))
        printfn "|"
    printfn "|---------------------------------------|"

    
    match previousMove with
    | Some ((color,kind), move,pieceTaken) -> 
        printf "%A: " color
        printf "%A %s -> %s." kind (getPositionName move.CurrentPosition) (getPositionName move.NewPosition)
        match pieceTaken with 
        | Some (_,kind) -> printf " Takes %A." kind
        | None -> ()
        printfn ""
    | None -> ()

    printfn "Player: %A" turn.Player 
