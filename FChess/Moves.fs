module Moves

open Types
open Turn

let private IsOnBoard (File file, Rank rank) = 
    if (file >= 1 && file <=8 && rank >= 1 && rank <= 8) then true else false

let private IsInitialPawnPosition  (player:Color) (_,Rank rank): bool =
    match player with White -> rank = 2 | Black -> rank = 7

let private GetPath (pos1:Position) (pos2:Position) =
    let File file1,Rank rank1 = pos1 
    let File file2,Rank rank2 = pos2

    let ranks = [(min rank1 rank2) .. (max rank1 rank2)]
    let files = [(min file1 file2) .. (max file1 file2)]

    if (file1=file2) then 
        ranks |> List.map (fun r -> File file1,Rank r)
    elif (rank1=rank2) then
        files |> List.map (fun f -> File f,Rank rank1)
    else
        ranks 
        |> List.zip files 
        |> List.map (fun (f,r) -> File f,Rank r)


let private isPathObstructed (turn:Turn) (pos1:Position) (pos2:Position) =
    GetPath pos1 pos2
    |> List.filter (fun pos -> pos <> pos1 && pos <> pos2) 
    |> List.filter (not << IsUnOccupied turn) 
    |> List.isEmpty |> not


let private RankForward (turn:Turn) (_,Rank currentRank) steps = 
    match turn.Player with White -> currentRank+steps | Black -> currentRank-steps 
    |> Rank

let private RankBackward turn position steps = RankForward turn position (steps * -1) 

let private FileLeft (turn:Turn) (File currentFile,_) steps = 
    match turn.Player with White -> currentFile-steps | Black -> currentFile+steps 
    |> File

let private FileRight turn pos steps = FileLeft turn pos (steps * -1)

let private CreateCastlingMove currentKingPos newKingPos currentRookPos newRookPos = 
    { 
        CurrentPosition=currentKingPos; 
        NewPosition=newKingPos; 
        CastlingMove=Some {CurrentPosition=currentRookPos; NewPosition=newRookPos}
    }

let private CreateMove currentPos newPos = { CurrentPosition=currentPos; NewPosition=newPos; CastlingMove=None }

type private MoveRule = Turn->Position->Move list

let private MovePawnTwoRanksForward:MoveRule = fun turn position ->
    let currentFile, _ = position

    let oneForward = (currentFile, RankForward turn position 1)
    let twoForward = (currentFile, RankForward turn position 2)

    [
        if (
            IsUnOccupied turn oneForward &&
            IsUnOccupied turn twoForward &&
            IsInitialPawnPosition turn.Player position
        ) then yield CreateMove position twoForward
    ]
        
let private MovePawnForward:MoveRule = fun turn position ->
    let currentFile, _ = position
    let oneForward = (currentFile,RankForward turn position 1)

    [oneForward] 
    |> List.filter IsOnBoard
    |> List.filter (IsUnOccupied turn)
    |> List.map (CreateMove position)

let private HitOpponentWithPawn:MoveRule = fun turn position ->
    let hitLeft = (FileLeft turn position 1,RankForward turn position 1)
    let hitRight = (FileRight turn position 1,RankForward turn position 1)

    [hitLeft; hitRight] 
    |> List.filter IsOnBoard
    |> List.filter (IsOccupiedByOpponent turn)
    |> List.map (CreateMove position)

let private MoveAlongRankOrFile:MoveRule = fun turn position ->
    let currentFile,currentRank = position
    
    let file =  [1 .. 8] |> List.map (fun r -> (currentFile,Rank r))
    let rank =  [1 .. 8] |> List.map (fun f -> (File f,currentRank))

    [file; rank]
    |> List.concat
    |> List.filter (not << IsOccupiedBySelf turn)
    |> List.filter (not << isPathObstructed turn position)
    |> List.map (CreateMove position)

let private MoveDiagonally:MoveRule = fun turn position -> 
    let File currentFile, Rank currentRank = position
    
    [1 ..8] 
    |> List.collect (fun i -> [
        (File (currentFile - i), Rank (currentRank - i))
        (File (currentFile - i), Rank (currentRank + i))
        (File (currentFile + i), Rank (currentRank - i))
        (File (currentFile + i), Rank (currentRank + i))
    ])
    |> List.filter IsOnBoard
    |> List.filter (not << IsOccupiedBySelf turn)
    |> List.filter (not << isPathObstructed turn position)
    |> List.map (CreateMove position)

let private MoveTwoAndOneRanksOrFiles:MoveRule = fun turn position -> 
    [
        (FileLeft turn position 1,RankForward turn position 2)
        (FileRight turn position 1,RankForward turn position 2)
        (FileLeft turn position 2,RankForward turn position 1)
        (FileRight turn position 2,RankForward turn position 1)
        (FileLeft turn position 1,RankBackward turn position 2)
        (FileRight turn position 1,RankBackward turn position 2)
        (FileLeft turn position 2,RankBackward turn position 1)
        (FileRight turn position 2,RankBackward turn position 1)

    ]
    |> List.filter IsOnBoard
    |> List.filter (not << IsOccupiedBySelf turn)
    |> List.map (CreateMove position)

let private MoveOneInEachDirection:MoveRule = fun turn position -> 
    let File currentFile, Rank currentRank = position

    
    let x = 
        [(currentFile - 1) .. (currentFile + 1)]
        |> List.allPairs[ (currentRank - 1) .. (currentRank + 1)]
        |> List.map (fun (r,f)-> (File f,Rank r))
        |> List.filter IsOnBoard
        |> List.filter (not << IsOccupiedBySelf turn)
        |> List.map (CreateMove position)
    x

let private PerformCastling:MoveRule = fun turn position -> 
    let kingMoved = true // TODO: Requires state
    let leftRookMoved = false // TODO: Requires state
    let rightRookMoved = false // TODO: Requires state
    
    let leftRookPosition = match turn.Player with White -> (File 1, Rank 1) | Black -> (File 8, Rank 8)
    let rightRookPosition = match turn.Player with White -> (File 8, Rank 1) | Black -> (File 1, Rank 8)

    let createCastleMove kingPosition rookPosition = 
        let File kingFile,Rank kingRank = kingPosition
        let File rookFile,Rank rookRank = rookPosition
        
        let direction = if kingFile > rookFile then -1 else 1; 

        let newKingPos = (File (kingFile + (2 * direction)),Rank kingRank)
        let newRookPos = (File (kingFile + direction),Rank kingRank)
        CreateCastlingMove position newKingPos rookPosition newRookPos
    
    [
        if (
            not kingMoved && 
            not leftRookMoved &&
            not (isPathObstructed turn position leftRookPosition)
            ) then yield createCastleMove position leftRookPosition
        if (
            not kingMoved && 
            not rightRookMoved &&
            not (isPathObstructed turn position rightRookPosition)
            ) then yield createCastleMove position rightRookPosition
    ]

// TODO: en passant rule. historical status needed?
let private GetAvailableMovesForPiece (turn:Turn) (position, (_,kind)) : Move list = 
    let moveRules = 
        match kind with
        | Pawn -> [ MovePawnTwoRanksForward; MovePawnForward; HitOpponentWithPawn ]
        | Knight -> [MoveTwoAndOneRanksOrFiles]
        | Bishop -> [MoveDiagonally]
        | Rook -> [MoveAlongRankOrFile]
        | Queen -> [MoveAlongRankOrFile; MoveDiagonally]
        | King -> [MoveOneInEachDirection; PerformCastling]
    moveRules |> List.collect (fun rule -> rule turn position)





let GetAllAvailableMoves (turn:Turn) : Move list =
    turn 
    |> GetOwnPiecesOnBoard
    |> List.collect (GetAvailableMovesForPiece turn)

    // TODO: filter out moves that result in being checked
    