module Turn

open Types

let private PieceOnBoardIsColor color (_,(col:Color,_)) = col = color

let PieceOnBoardIsOnPosition position (pos,_) = pos = position    


let GetPlayerPiecesOnBoard (turn:Turn) (player:Color) = 
    turn.Pieces |> List.filter (player |> PieceOnBoardIsColor)  

let GetOwnPiecesOnBoard turn = turn.Player |> GetPlayerPiecesOnBoard turn 
let GetOpponentPiecesOnBoard turn = turn.Player.Opponent |> GetPlayerPiecesOnBoard turn

let GetPieceOnPosition turn position =
    turn.Pieces 
    |> List.tryFind (PieceOnBoardIsOnPosition position) 
    |> function
    | Some (pos,piece) -> Some piece
    | None -> None
    

let private IsOccupiedByPlayer (turn:Turn) (position: Position) (player:Color) = 
    GetPlayerPiecesOnBoard turn player
    |> List.map (fun (pos: Position,_) -> pos) 
    |> List.contains position

    
let IsOccupiedBySelf turn position = turn.Player |> IsOccupiedByPlayer turn position 
let IsOccupiedByOpponent turn position = turn.Player.Opponent |> IsOccupiedByPlayer turn position

let IsUnOccupied (turn:Turn) (position: Position): bool = 
    turn.Pieces |> List.filter (PieceOnBoardIsOnPosition position) |> List.isEmpty



