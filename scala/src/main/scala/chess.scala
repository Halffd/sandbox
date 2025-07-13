import scala.util.{Try, Success, Failure}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.time.LocalDateTime

// Position on the board
case class Position(row: Int, col: Int) {
  def isValid: Boolean = row >= 0 && row < 8 && col >= 0 && col < 8
}

// Basic chess piece representation
sealed trait Piece {
  def color: Color
  def symbol: Char
}

sealed trait Color
case object White extends Color
case object Black extends Color

case class Pawn(color: Color) extends Piece {
  def symbol: Char = if (color == White) 'P' else 'p'
}

case class Rook(color: Color) extends Piece {
  def symbol: Char = if (color == White) 'R' else 'r'
}

case class Knight(color: Color) extends Piece {
  def symbol: Char = if (color == White) 'N' else 'n'
}

case class Bishop(color: Color) extends Piece {
  def symbol: Char = if (color == White) 'B' else 'b'
}

case class Queen(color: Color) extends Piece {
  def symbol: Char = if (color == White) 'Q' else 'q'
}

case class King(color: Color) extends Piece {
  def symbol: Char = if (color == White) 'K' else 'k'
}

// The missing ChessBoard class!
class ChessBoard {
  private var squares: Array[Array[Option[Piece]]] = Array.ofDim(8, 8)
  
  def initialize(): ChessBoard = {
    // Clear board
    for (i <- 0 until 8; j <- 0 until 8) {
      squares(i)(j) = None
    }
    
    // Place white pieces
    squares(0)(0) = Some(Rook(White))
    squares(0)(1) = Some(Knight(White))
    squares(0)(2) = Some(Bishop(White))
    squares(0)(3) = Some(Queen(White))
    squares(0)(4) = Some(King(White))
    squares(0)(5) = Some(Bishop(White))
    squares(0)(6) = Some(Knight(White))
    squares(0)(7) = Some(Rook(White))
    
    for (i <- 0 until 8) {
      squares(1)(i) = Some(Pawn(White))
    }
    
    // Place black pieces
    squares(7)(0) = Some(Rook(Black))
    squares(7)(1) = Some(Knight(Black))
    squares(7)(2) = Some(Bishop(Black))
    squares(7)(3) = Some(Queen(Black))
    squares(7)(4) = Some(King(Black))
    squares(7)(5) = Some(Bishop(Black))
    squares(7)(6) = Some(Knight(Black))
    squares(7)(7) = Some(Rook(Black))
    
    for (i <- 0 until 8) {
      squares(6)(i) = Some(Pawn(Black))
    }
    val isValid = Option[Boolean](true)
    
    this
  }
  
  def getPiece(pos: Position): Option[Piece] = {
    if (pos.isValid) squares(pos.row)(pos.col) else None
  }
  
  def move(from: Position, to: Position): ChessBoard = {
    val newBoard = new ChessBoard()
    // Copy current board state
    for (i <- 0 until 8; j <- 0 until 8) {
      newBoard.squares(i)(j) = squares(i)(j)
    }
    // Make the move
    newBoard.squares(to.row)(to.col) = squares(from.row)(from.col)
    newBoard.squares(from.row)(from.col) = None
    newBoard
  }
  
  def isValid: Boolean = true // Simplified validation
  
  def display(): Unit = {
    println("  a b c d e f g h")
    for (row <- 7 to 0 by -1) {
      print(s"${row + 1} ")
      for (col <- 0 until 8) {
        squares(row)(col) match {
          case Some(piece) => print(s"${piece.symbol} ")
          case None => print(". ")
        }
      }
      println()
    }
  }
}

// Now your flatMap hell should work!
object FlatMapChessHell {
  
  def playChessGame(): Future[String] = {
    Future.successful("Starting game")
      .flatMap(_ => initializeBoard())
      .flatMap(board => makeMove(board, "e2", "e4"))
      .flatMap(board => makeMove(board, "e7", "e5"))
      .flatMap(board => makeMove(board, "g1", "f3"))
      .flatMap(board => makeMove(board, "b8", "c6"))
      .flatMap(board => validateBoard(board))
      .flatMap(_ => checkGameState())
      .flatMap(state => 
        if (state == "continue") 
          Future.successful("Game continues")
        else 
          Future.successful("Game over")
      )
      .flatMap(result => 
        Future.successful(s"Final result: $result")
      )
  }
  
  def initializeBoard(): Future[ChessBoard] = {
    Future.successful(new ChessBoard())
      .flatMap(board => Future.successful(board.initialize()))
  }
  
  def makeMove(board: ChessBoard, from: String, to: String): Future[ChessBoard] = {
    parsePosition(from)
      .flatMap(fromPos => 
        parsePosition(to).flatMap(toPos =>
          validateMove(board, fromPos, toPos).flatMap(validMove =>
            if (validMove) Future.successful(board.move(fromPos, toPos))
            else Future.failed(new Exception("Invalid move"))
          )
        )
      )
  }
  
  def parsePosition(pos: String): Future[Position] = {
    Future.successful(pos)
      .flatMap(p => 
        if (p.length == 2) Future.successful(p)
        else Future.failed(new Exception("Invalid position length"))
      )
      .flatMap(p => Future.successful(Position(p.charAt(1).toString.toInt - 1, p.charAt(0) - 'a')))
      .flatMap(pos => 
        if (pos.isValid) Future.successful(pos)
        else Future.failed(new Exception("Position out of bounds"))
      )
  }
  
  def validateMove(board: ChessBoard, from: Position, to: Position): Future[Boolean] = {
    Future.successful(board)
      .flatMap(b => Future.successful(b.getPiece(from)))
      .flatMap {
        case Some(piece) => Future.successful(true)
        case None => Future.successful(false)
      }
      .flatMap(hasPiece => 
        if (hasPiece) checkMoveValidity(board, from, to)
        else Future.successful(false)
      )
  }
  
  def checkMoveValidity(board: ChessBoard, from: Position, to: Position): Future[Boolean] = {
    Future.successful(board.getPiece(from))
      .flatMap {
        case Some(piece) => 
          piece match {
            case _: Pawn => validatePawnMove(from, to)
            case _: Rook => validateRookMove(from, to)
            case _: Knight => validateKnightMove(from, to)
            case _ => Future.successful(true) // Simplified
          }
        case None => Future.successful(false)
      }
  }
  
  def validatePawnMove(from: Position, to: Position): Future[Boolean] = {
    Future.successful(Math.abs(from.row - to.row))
      .flatMap(rowDiff => 
        Future.successful(Math.abs(from.col - to.col))
          .flatMap(colDiff =>
            Future.successful(rowDiff == 1 && colDiff <= 1)
          )
      )
  }
  
  def validateRookMove(from: Position, to: Position): Future[Boolean] = {
    Future.successful(from.row == to.row || from.col == to.col)
  }
  
  def validateKnightMove(from: Position, to: Position): Future[Boolean] = {
    Future.successful(Math.abs(from.row - to.row))
      .flatMap(rowDiff =>
        Future.successful(Math.abs(from.col - to.col))
          .flatMap(colDiff =>
            Future.successful((rowDiff == 2 && colDiff == 1) || (rowDiff == 1 && colDiff == 2))
          )
      )
  }
  
  def validateBoard(board: ChessBoard): Future[ChessBoard] = {
    Future.successful(board)
      .flatMap(b => Future.successful(b.isValid))
      .flatMap(valid => 
        if (valid) Future.successful(board)
        else Future.failed(new Exception("Invalid board state"))
      )
  }
  
  def checkGameState(): Future[String] = {
    Future.successful("analyzing")
      .flatMap(_ => Future.successful("checking"))
      .flatMap(_ => Future.successful("continue"))
  }
}

// Usage
@main def runChess(): Unit = {
  println("🎯 FLATMAP CHESS HELL COMPILATION TEST")
  
  FlatMapChessHell.playChessGame().onComplete {
    case Success(result) => println(s"✅ Game completed: $result")
    case Failure(e) => println(s"❌ Game failed: ${e.getMessage}")
  }
  
  Thread.sleep(2000)
}