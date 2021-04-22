module Types

type Kind =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type Color =Black | White with
    member this.Opponent = match this with White -> Black | Black -> White

type Piece = Color*Kind

type Rank = Rank of int
type File = File of int

type Position = File*Rank

type CastlingMove = {
    CurrentPosition: Position; 
    NewPosition: Position
}

type Move = {
    CurrentPosition: Position
    NewPosition: Position
    CastlingMove: CastlingMove option
}

type PieceOnBoard = Position*Piece

type Turn = {
        Pieces: PieceOnBoard list
        Player: Color
        IsCheck: bool
        previousMoves: (Piece*Move*(Piece option)) list
    }

type Status = Normal | StaleMate | Check | CheckMate
    