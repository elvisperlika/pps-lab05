package polyglot.a01a

import polyglot.a01a.Logics
import polyglot.a01a.Logics.Result
import util.Sequences.Sequence
import util.Sequences.Sequence.Cons

import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01a/sol2/ */
class LogicsImpl(private val size: Int, private val boatLength: Int) extends Logic:
  opaque type Position = (Int, Int)
  val boat: Boat = Boat(getRandomPosition, boatLength)
  val HIT_MAX = 5
  private var hitsCounter = HIT_MAX

  def hit(row: Int, col: Int): Result = {
    hitsCounter -= 1
    if hitsCounter == 0 then
      Result.LOST
    else if boat.isDestroy(row, col) then {
      Result.WON
    } else if boat.isHit(row, col) then {
      Result.HIT
    } else
      Result.MISS
  }

  private def getRandomPosition: Position =
    val randGenerator: Random = Random
    val x = randGenerator.nextInt(size - boatLength)
    val y = randGenerator.nextInt(size)
    (x, y)

  case class Boat(position: Position, boatLength: Int):
    opaque type Hit = Boolean
    private val positions: Sequence[(Position, Hit)] = getBoatPositions

    private def getBoatPositions: Sequence[(Position, Hit)] =
      var sequence: Sequence[(Position, Hit)] = Sequence.Nil()
      position._2 until position._2 + boatLength foreach { i =>
        sequence = Cons(((position._1, i), false), sequence)
      }
      sequence

    def isHit(x: Int, y: Int): Boolean =
      if positions.contains(((x, y), false)) then
        var pos = positions.find(p => p._1 == (x, y)).orElse((0, 0), false)
        pos = ((x, y), true)
        true
      else
        false

    def isDestroy(x: Int, y: Int): Boolean = {
      isHit(x, y) && positions.filter(p => p._2).size() == boatLength
    }