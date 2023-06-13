import Main._

class GameTests extends munit.FunSuite {

    val small =
      """0.X
        |0X.
        |X..""".stripMargin.replace("\r\n","\n")

    val medium1 =
      """00000
        |0000X
        |000..
        |00.0.
        |0X..0""".stripMargin.replace("\r\n","\n")

    val aboveFstDiag1 =
      """00..
        |00.
        |0X
        |0""".stripMargin.replace("\r\n","\n")

    val aboveSndDiag1 =
      """0000
        |000
        |00
        |0""".stripMargin.replace("\r\n","\n")

    val belowSndDiag1 =
      """X..X
        |.0.
        |..
        |0""".stripMargin.replace("\r\n","\n")

    val medium2 =
      """0X0X0.
        |000.X0
        |0.0X..
        |0..0..
        |0X..0X
        |...X..""".stripMargin.replace("\r\n","\n")

  val aboveFstDiag2 =
    """X0X.X
      |0...
      |XX.
      |00
      |.""".stripMargin.replace("\r\n","\n")

  val belowFstDiag2 =
    """0....
      |0..X
      |0X.
      |0.
      |.""".stripMargin.replace("\r\n","\n")

  val aboveSndDiag2 =
    """0.0.0
      |X0.0
      |000
      |X0
      |0""".stripMargin.replace("\r\n","\n")

  val belowSndDiag2 =
    """0.0..
      |....
      |.0X
      |X.
      |.""".stripMargin.replace("\r\n","\n")

  test("Valid profile id:"+profileID){
         assert(profileID > 0)
     }

     test("isFree implementation (0p) "){
       assert(isFree(2,2,makeBoard(small)))
       assert(!isFree(0,0,makeBoard(small)))
     }

    test("Complement implementation (0p)"){
      assert(complement(One) == Two)
      assert(complement(Two) == One)
      assert(complement(Empty) == Empty)
    }

    test("Showing a small board (5p)"){
      assert(small == show(makeBoard(small)))
    }

    test("Showing a medium board (15p)"){
      assert(medium1 == show(makeBoard(medium1)))
    }

    test("Retrieving the list of columns (0p)"){
      assert(getColumns(makeBoard(medium1)) == makeBoard(medium1))
    }

    test("Retrieving the first diagonal (2p)"){
      assert(getFstDiag(makeBoard(medium1)) == List(Two,Two,Two,Two,Two))
    }

    test("Retrieving the second diagonal (2p)"){
      assert(getSndDiag(makeBoard(medium1)) == List(Two,Two,Two,Two,Two))
    }

    test("(A)Elements above fst diagonal 1 (2p)"){
      assert(show(getAboveFstDiag(makeBoard(medium1))) == aboveFstDiag1)
    }

    test("(A)Elements above fst diagonal 2 (2p)"){
      //println(show(getAboveFstDiag(makeBoard(medium2))))
      assert(show(getAboveFstDiag(makeBoard(medium2))) == aboveFstDiag2)
    }

  test("(B)Elements below fst diagonal 1 (2p)"){
        assert(show(getBelowFstDiag(makeBoard(medium1))) == aboveFstDiag1)
  }

  test("(B)Elements below fst diagonal 2 (2p)"){
    assert(show(getBelowFstDiag(makeBoard(medium2))) == belowFstDiag2)
  }

  test("(C)Elements above snd diagonal 1 (2p)"){
    //print(show(getAboveSndDiag(makeBoard(medium1))))
    assert(show(getAboveSndDiag(makeBoard(medium1))) == aboveSndDiag1)
  }

  test("(C)Elements above snd diagonal 2 (2p)"){
    assert(show(getAboveSndDiag(makeBoard(medium2))) == aboveSndDiag2)
  }

  test("(D)Elements below snd diagonal 1 (2p)"){
    //println(show(getBelowSndDiag(makeBoard(medium1))))
    assert(show(getBelowSndDiag(makeBoard(medium1))) == belowSndDiag1)
  }

  test("(D)Elements below snd diagonal 2 (2p)"){
    assert(show(getBelowSndDiag(makeBoard(medium2))) == belowSndDiag2)
  }


  test("Winner 1 (5p)"){
    assert(winner(Two)(makeBoard(medium1)))
    assert(!winner(One)(makeBoard(medium1)))
  }

  test("Winner 2 (5p)"){
    assert(winner(Two)(makeBoard(medium2)))
    assert(!winner(One)(makeBoard(medium2)))
  }

  val smallUpd1 =
    """0XX
      |0X.
      |X..""".stripMargin.replace("\r\n","\n")

  test("Update 1 (7p)"){
    assert(show(update(One)(0,1,makeBoard(small))) == smallUpd1)
  }

  val smallUpd2 =
    """0.X
      |0X.
      |X.0""".stripMargin.replace("\r\n","\n")

  test("Update 2 (8p)"){
    assert(show(update(Two)(2,2,makeBoard(small))) == smallUpd2)
  }

  val full =
    """0XX
      |0XX
      |XX0""".stripMargin.replace("\r\n","\n")

  test("Next 1 (5p)"){
    assert(next(Two)(makeBoard(full)) == Nil)
    assert(next(One)(makeBoard(full)) == Nil)
  }

  val nextTest =
    """0..
      |0.X
      |.X.""".stripMargin.replace("\r\n","\n")

  val nextTestR1 = Set("00.\n0.X\n.X.","0.0\n0.X\n.X.", "0..\n00X\n.X.", "0..\n0.X\n0X.", "0..\n0.X\n.X0")
  val nextTestR2 = Set("0X.\n0.X\n.X.","0.X\n0.X\n.X.", "0..\n0XX\n.X.", "0..\n0.X\nXX.", "0..\n0.X\n.XX")

  test("Next 2 (10p)"){
    println(next(Two)(makeBoard(nextTest)))
    assert(next(Two)(makeBoard(nextTest)).map(show).toSet == nextTestR1)
    assert(next(One)(makeBoard(nextTest)).map(show).toSet == nextTestR2)
  }
}