module Game

open Types

let NewGame =
    let whiteFirstRank = [
        ((File 1,Rank 1),(White,Rook))
        ((File 2,Rank 1),(White,Knight))
        ((File 3,Rank 1),(White,Bishop))
        ((File 4,Rank 1),(White,Queen))
        ((File 5,Rank 1),(White,King))
        ((File 6,Rank 1),(White,Bishop))
        ((File 7,Rank 1),(White,Knight))
        ((File 8,Rank 1),(White,Rook))
    ]

    let blackFirstRank = [
        ((File 1,Rank 8),(Black,Rook))
        ((File 2,Rank 8),(Black,Knight))
        ((File 3,Rank 8),(Black,Bishop))
        ((File 4,Rank 8),(Black,Queen))
        ((File 5,Rank 8),(Black,King))
        ((File 6,Rank 8),(Black,Bishop))
        ((File 7,Rank 8),(Black,Knight))
        ((File 8,Rank 8),(Black,Rook))
    ]

    let pawns = 
        [1 ..8] 
        |> List.collect (fun f -> [
            ((File f,Rank 2),(White,Pawn))
            ((File f,Rank 7),(Black,Pawn))
        ])

    {
        Pieces=[whiteFirstRank; blackFirstRank; pawns] |> List.concat
        Player=White
        IsCheck=false
        previousMoves=[]
    }
