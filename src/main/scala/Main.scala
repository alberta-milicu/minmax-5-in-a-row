object Main {


  type Line = List[Player]
  type Board = List[Line]

  def profileID: Int = 410932

  def newline = "\n"

  // creates a board from a string
  def makeBoard(s: String): Board = {
    def toPos(c: Char): Player =
      c match {
        case 'X' => One
        case '0' => Two
        case _ => Empty
      }
    s.split(newline).toList.map(row => row.toList.map(toPos))
  }

  // checks if the position (x,y) on board b is free
  def isFree(x: Int, y: Int, b: Board): Boolean = b(x)(y) == Empty

  // returns the "other" player from a position, if it exists
  def complement(p: Player): Player =
    p match {
      case One => Two
      case Two => One
      case Empty => Empty
    }

  def show(b: Board): String = {
    def toChar(p: Player): Char =
      p match {
        case One => 'X'
        case Two => '0'
        case Empty => '.'
      }
    b.map(row => row.map(toChar).mkString).mkString(newline)
  }

  // Returns a list of columns from a board
  def getColumns(b: Board): Board = {
    def transpose(bd: Board): Board =
      bd.head.indices.map { i =>
        bd.map(row => row(i))
      }.toList
    transpose(b)
  }

  // returns the first diagonal as a line
  def getFstDiag(b: Board): Line = {
    def fstDiag(bd: Board): Line =
      bd.indices.toList.map(i => bd(i)(i))
    fstDiag(b)
  }

  // returns the second diagonal as a line
  def getSndDiag(b: Board): Line = {
    def sndDiag(bd: Board): Line =
      bd.indices.toList.map(i => bd(i)(bd.length - i - 1))
    sndDiag(b)
  }

  // retrieves all the diagonals above the first line
  def getAboveFstDiag(b: Board): List[Line] = {
    (1 until b.size).map(i =>
      (0 until b.size - i).map(j =>
        b(j)(i + j)
      ).toList
    ).toList
  }

  def getBelowFstDiag(b: Board): List[Line] = getAboveFstDiag(getColumns(b))

  def getAboveSndDiag(b: Board): List[Line] = getAboveFstDiag(getColumns(getColumns(b).reverse))

  def getBelowSndDiag(b: Board): List[Line] = getBelowFstDiag(getColumns(getColumns(b).reverse))

  // write a function that checks if a given player is a winner
  def winner(p: Player)(b: Board): Boolean = {
    def checkLine(line: Line): Boolean =
      line.sliding(5).exists(_.forall(_ == p))

    val rows = b.exists(checkLine)
    val cols = getColumns(b).exists(checkLine)
    val diag1 = getFstDiag(b).sliding(5).exists(_.forall(_ == p))
    val diag2 = getSndDiag(b).sliding(5).exists(_.forall(_ == p))

    rows || cols || diag1 || diag2
  }


  /*
   * Write a function which updates a position (with a player) at given indices from the board.
   * Your function need not check if the position is empty.
   */
  def update(p: Player)(ln: Int, col: Int, b: Board): Board = {
    b.updated(ln, b(ln).updated(col, p))
  }

  /*
   * generates one possible next move for player p. Hint - use "isFree" and "update"
   */
  def next(p: Player)(b: Board): List[Board] =
    (for {
      i <- 0 until b.size
      j <- 0 until b.size
      if isFree(i, j, b)
    } yield update(p)(i, j, b)).toList

  /*
   * Implement the sequences function that analyzes the board and returns a map of the form:
   * (5,a), (4,b), (3,c), (2,d)
   */
  def sequences(p: Player)(b: Board): Map[Int, Int] = {
    val lines = b ++ getColumns(b) ++ getAboveFstDiag(b) ++ getBelowFstDiag(b) ++ getAboveSndDiag(b) ++ getBelowSndDiag(b)
    val lengths = lines.flatMap(_.sliding(2).map(_.takeWhile(_ == p).length)).filter(_ >= 2)
    lengths.groupBy(identity).view.mapValues(_.length).toMap
  }


  /*
   * Implement the play function that represents the AI's decision of selecting a move.
   */


  def evaluateBoard(p: Player, b: Board): Int = {

    val currentPlayerSequences = sequences(p)(b)
    val opponentPlayerSequences = sequences(complement(p))(b)

    val currentPlayerNumSequences = currentPlayerSequences.getOrElse(5, 0)
    val currentPlayerNumPotentialSequences = currentPlayerSequences.getOrElse(4, 0) +
      currentPlayerSequences.getOrElse(3, 0) +
      currentPlayerSequences.getOrElse(2, 0)

    val opponentPlayerNumSequences = opponentPlayerSequences.getOrElse(5, 0)
    val opponentPlayerNumPotentialSequences = opponentPlayerSequences.getOrElse(4, 0) +
      opponentPlayerSequences.getOrElse(3, 0) +
      opponentPlayerSequences.getOrElse(2, 0)

    currentPlayerNumSequences * 10 +
      currentPlayerNumPotentialSequences -
      opponentPlayerNumSequences * 10 -
      opponentPlayerNumPotentialSequences
  }

  def play(p: Player, b: Board): Board = {
    def evaluateMove(move: Board, depth: Int, isMaximizingPlayer: Boolean, alpha: Int, beta: Int): Int = {
      val currentPlayer = if (isMaximizingPlayer) p else complement(p)
      val opponentPlayer = if (isMaximizingPlayer) complement(p) else p

      if (depth == 0 || winner(currentPlayer)(move)) {
        evaluateBoard(currentPlayer, move)
      } else {
        if (isMaximizingPlayer) {
          var maxEval = Int.MinValue
          var newAlpha = alpha
          val availableMoves = next(currentPlayer)(move)
          availableMoves.foreach { nextMove =>
            val eval = evaluateMove(nextMove, depth - 1, false, newAlpha, beta)
            maxEval = Math.max(maxEval, eval)
            newAlpha = Math.max(newAlpha, eval)
            if (beta <= newAlpha)
              return maxEval
          }
          maxEval
        } else {
          var minEval = Int.MaxValue
          var newBeta = beta
          val availableMoves = next(opponentPlayer)(move)
          availableMoves.foreach { nextMove =>
            val eval = evaluateMove(nextMove, depth - 1, true, alpha, newBeta)
            minEval = Math.min(minEval, eval)
            newBeta = Math.min(newBeta, eval)
            if (newBeta <= alpha)
              return minEval
          }
          minEval
        }
      }
    }

    val availableMoves = next(p)(b)
    var bestScore = Int.MinValue
    var bestMove: Board = availableMoves.head

    availableMoves.foreach { move =>
      val score = evaluateMove(move, 3, false, Int.MinValue, Int.MaxValue)
      if (score > bestScore) {
        bestScore = score
        bestMove = move
      }
    }

    bestMove
  }

  /*
   * Implement the main function for running the game loop.
   */
  def main(args: Array[String]): Unit = {
    println("Enter the number of rows for the board:")
    val rows = scala.io.StdIn.readInt()

    println("Enter the number of columns for the board:")
    val cols = scala.io.StdIn.readInt()

    val initialBoardString = "." * cols + newline
    val initialBoard = makeBoard(initialBoardString * rows)

    var currentPlayer: Player = One
    var board: Board = initialBoard

    while (!winner(currentPlayer)(board)) {
      currentPlayer match {
        case One =>
          println("Player One, enter your move (x y): ")
          val Array(row, col) = scala.io.StdIn.readLine().split(" ").map(_.toInt)
          if (isFree(row, col, board))
            board = update(One)(row, col, board)
          else
            println("Invalid move. Try again.")
        case Two =>
          println("Player Two is making a move...")
          board = play(Two, board)
      }
//      currentPlayer match {
//        case Two =>
//          println("Player Two, enter your move (x y): ")
//          val Array(row, col) = scala.io.StdIn.readLine().split(" ").map(_.toInt)
//          if (isFree(row, col, board))
//            board = update(Two)(row, col, board)
//          else
//            println("Invalid move. Try again.")
//        case One =>
//          println("Player One is making a move...")
//          board = play(One, board)
//      }

      println(show(board))
      currentPlayer = complement(currentPlayer)
    }

    currentPlayer match {
      case One => println("Player One wins!")
      case Two => println("Player Two wins!")
    }
  }
}

