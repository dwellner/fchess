open Types
open ConsoleUi
open MoveRating
open Game
    

// TODO: Rating logic

// IS CHECK -VALIDATION in two places and print in output!

// improve logic

// unit test all the things

// take input (later)

// replace pawns with queen

// implement castling, en passant


let rec Play (turn,status)  =

    printBoard turn
    System.Console.ReadLine() |> ignore

    let state = PlayHighestRatedNextMove turn
    match state with
    | Some (turn,status,rating) -> 
        printfn "Move Rating: %d" rating
        Play (turn,status) |> ignore
    | None -> exit 0






[<EntryPoint>]
let main argv =
    let game = NewGame
    Play (game,Normal)
    0