module MoveRating

open Types
open Turn
open Moves

type ConsideredMove = {
    Turn: Turn
    Rating: int
    Depth: int
    OponentMoves: ConsideredMove list option
    Status: Status option
}

let rec RateMove turn move (opponentMoves:ConsideredMove list option) :int =
    let capturedPieceKind = 
        move.NewPosition 
        |> GetPieceOnPosition turn
        |> function Some (_,kind) -> Some kind | None -> None

    let hitRating = match capturedPieceKind with
    | Some Queen -> 1000
    | Some Rook -> 400
    | Some Bishop -> 300
    | Some Knight -> 200
    | Some Pawn -> 50
    | _ -> 0

    let bestOpponentRating = 
        match opponentMoves with 
        | Some moves when moves.IsEmpty -> 100000
        | Some moves -> moves |> List.map (fun move -> move.Rating) |> List.max |> float |> (*) 0.9 |> int
        | None -> 0


    hitRating - bestOpponentRating



let IsCheck (pieces:(Position*Piece) list): bool = false // TODO Check king is newPosition of any next move for same color


let rec RateConsideredMove (turn:Turn) (depth:int) (move:Move) : ConsideredMove =
    
    let _,piece = turn.Pieces |> List.find (PieceOnBoardIsOnPosition move.CurrentPosition)
    let pieceTaken = GetPieceOnPosition turn move.NewPosition

    let pieces = 
        (move.NewPosition,piece) ::
        (turn.Pieces 
            |> List.filter (not << PieceOnBoardIsOnPosition move.CurrentPosition)
            |> List.filter (not << PieceOnBoardIsOnPosition move.NewPosition))
    
    let nextTurn = 
        { 
            Pieces=pieces
            Player=turn.Player.Opponent
            IsCheck=IsCheck(pieces)
            previousMoves = (piece,move,pieceTaken)::turn.previousMoves
        }
    
    let opponentMoves = 
        if (depth  < 3) then
                nextTurn 
                |> GetAllAvailableMoves 
                |> List.map (RateConsideredMove nextTurn (depth + 1))
                |> Some
        else
            None

    let status = 
        match (opponentMoves) with
        | Some moves when nextTurn.IsCheck -> if (moves.IsEmpty) then Some CheckMate else Some Check
        | Some moves -> if (moves.IsEmpty) then Some StaleMate else Some Normal
        | None -> None

    {
        Turn=nextTurn
        Rating=RateMove turn move opponentMoves
        OponentMoves=opponentMoves
        Depth=1
        Status=status
    }

let PlayHighestRatedNextMove (turn:Turn): (Turn*Status*int) option=
    let consideredMoves = 
        turn 
        |> GetAllAvailableMoves 
        |> List.map (RateConsideredMove turn 0)

    let hasMoves = not consideredMoves.IsEmpty


    let rec getOptions list = list |> List.map (fun move -> match move.OponentMoves with Some moves -> getOptions moves | _ -> 1) |> List.sum


    
    if hasMoves then
        printfn "Considered options: %d" (getOptions consideredMoves)

        let highestRating = consideredMoves |> List.map (fun m -> m.Rating) |> List.max
        let nextMoveOptions = consideredMoves |> List.filter (fun m -> m.Rating = highestRating) |> List.toArray
        let optionCount = nextMoveOptions |> Array.length

        let rnd = System.Random()  
        let nextMove = nextMoveOptions.[rnd.Next(optionCount)]
        let status = match nextMove.Status with Some status -> status | _ -> Normal
        Some (nextMove.Turn, status, nextMove.Rating)

        
        
        
    else None
