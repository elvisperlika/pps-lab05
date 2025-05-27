package polyglot.a05b

import scala.util.Random
import util.Sequences.Sequence
import util.Sequences.Sequence.Cons

import javax.swing.text.Position

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a05b/sol2/ */
class LogicsImpl(private val size: Int) extends Logics:

  opaque type Position = (Int, Int)
  private var points: Sequence[Position] = Sequence.Nil()
  private var offset: Int = 1
  private val initPoint: Position = randomPosition()

  private def randomPosition(): Position =
    val randGenerator: Random = Random
    val x = 1 + randGenerator.nextInt(size - 1)
    val y = 1 + randGenerator.nextInt(size - 1)
    points = Cons((x, y), points)
    (x, y)
  
  override def tick(): Unit = {
    for
      dirX <- -1 to 1
      dirY <- -1 to 1
      if dirX != 0 || dirY != 0
    do points = Cons((initPoint._1 + offset * dirX, initPoint._2 + offset * dirY), points)
    offset += 1
  }

  private def isOnBorder(t: Position) =
    t._1 <= 0 || t._1 >= size || t._2 <= 0 || t._2 >= size

  override def isOver: Boolean = {
    !points.find(t => isOnBorder(t)).isEmpty
  }

  override def hasElement(x: Int, y: Int): Boolean =
    points.contains((x, y))
