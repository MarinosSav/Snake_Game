package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.{Apple, Direction, East, Empty, GridType, North, SnakeBody, SnakeHead, South, West}
import snake.logic
import snake.logic.SnakeLogic._

import scala.collection.mutable.ArrayBuffer

/** To implement Snake, complete the ``TODOs`` below.
  *
  * If you need additional files,
  * please also put them in the ``snake`` package.
  */

class SnakeLogic(val randomGen: RandomGenerator,
                 val nrColumns: Int,
                 val nrRows: Int) {

  var gameOver = false
  var noSpace = false
  var reverseMode = false
  var directionSwitched = false
  var grid: Array[Array[Int]] = _
  var expand : Int = 0
  var dir : Direction = East()
  var appleCoordinates : (Int, Int) = _
  var snake : ArrayBuffer[(GridType, Int, Int)] = ArrayBuffer((SnakeHead(dir), 2, 0))
  initSnakeBody()
  var gameStateCounter : Int = 0
  val storeGameState : ArrayBuffer[(ArrayBuffer[(GridType, Int, Int)], Direction, (Int, Int))] =
    ArrayBuffer.empty[(ArrayBuffer[(GridType, Int, Int)], Direction, (Int, Int))]
  initGameState()

  def this() = this(new ScalaRandomGen(), DefaultRows, DefaultColumns)

  def initGameState(): Unit = {

    storeGameState.append((snake, dir, appleCoordinates))

    var tempSnake = new ArrayBuffer[(GridType, Int, Int)]
    tempSnake ++= snake
    snake = new ArrayBuffer[(GridType, Int, Int)]
    snake = tempSnake

  }

  def initSnakeBody(): Unit = {

    snake.append((SnakeBody(1), 1, 0))
    snake.append((SnakeBody(1), 0, 0))

  }

  def placeApple(): (Int, Int) = {

    grid = Array.ofDim[Int](nrColumns, nrRows)
    val nrFreeSpots = getFreeSpots()
    if (nrFreeSpots == 0) return _
    val spot: Int = randomGen.randomInt(nrFreeSpots)
    var coordinates : (Int, Int) = (0,0)
    for (y <- 0 until nrRows){
      for (x <- 0 until nrColumns){
        if (grid(x)(y) == spot) coordinates = (x, y)
      }
    }
    coordinates
  }

  def getFreeSpots(): Int ={

    var counter = 0

    for(i <- snake.indices){
      grid(snake(i)._2)(snake(i)._3) = -1
    }
    for (i <- 0 until nrRows){
      for (j <- 0 until nrColumns){
        if (grid(j)(i) != -1){
          grid(j)(i) = counter
          counter += 1
        }
      }
    }
    counter
  }

  def isGameOver: Boolean = gameOver

  def step(): Unit = {

    if (reverseMode){
      stepBack()
      return
    }
    if (!isGameOver) {
      moveBody()
      moveHead()
      checkGameOver()
      checkForWarp()
      checkForApple()
      if (expand != 0) expandSnake()
      storeMove()
    }

  }

  def storeMove(): Unit = {

    gameStateCounter += 1

    storeGameState.append((snake, dir, appleCoordinates))

    var tempSnake = new ArrayBuffer[(GridType, Int, Int)]
    tempSnake ++= snake
    snake = new ArrayBuffer[(GridType, Int, Int)]
    snake = tempSnake

  }

  def stepBack(): Unit = {

    if (gameOver) gameOver = false
    if (gameStateCounter > 0){
      storeGameState.remove(gameStateCounter)
      gameStateCounter -= 1
      snake = storeGameState(gameStateCounter)._1
      dir = storeGameState(gameStateCounter)._2
      appleCoordinates = storeGameState(gameStateCounter)._3
    }

  }

  def moveBody(): Unit = {

    for (i <- snake.length - 1 to 1 by -1) {
      snake(i) = (snake(i)._1, snake(i - 1)._2, snake(i - 1)._3)
    }

  }

  def moveHead(): Unit = {

    directionSwitched = false

    dir match {
      case North() => snake(0) = (snake(0)._1, snake(0)._2, snake(0)._3 - 1)
      case South() => snake(0) = (snake(0)._1, snake(0)._2, snake(0)._3 + 1)
      case East() => snake(0) = (snake(0)._1, snake(0)._2 + 1, snake(0)._3)
      case West() => snake(0) = (snake(0)._1, snake(0)._2 - 1, snake(0)._3)
    }

  }

  def checkGameOver(): Unit = {
    for (i <- 1 until snake.length){
      if ((snake(i)._2, snake(i)._3) == (snake(0)._2, snake(0)._3)) gameOver = true
    }
  }

  def checkForWarp(): Unit = {
    if (snake(0)._2 >= nrColumns) snake(0) = (snake(0)._1, 0, snake(0)._3)
    else if (snake(0)._3 >= nrRows) snake(0) = (snake(0)._1, snake(0)._2, 0)
    else if (snake(0)._2 < 0) snake(0) = (snake(0)._1, nrColumns - 1, snake(0)._3)
    else if (snake(0)._3 < 0) snake(0) = (snake(0)._1, snake(0)._2, nrRows - 1)
  }

  def checkForApple(): Unit = {
    if (appleCoordinates == (snake(0)._2, snake(0)._3)){
      appleCoordinates = null
      expand = 3
    }
  }

  def expandSnake(): Unit = {
    snake.append((SnakeBody(1), snake(snake.length - 1)._2 ,snake(snake.length - 1)._3))
    expand -= 1
  }

  def setReverseTime(reverse: Boolean): Unit = (reverseMode = reverse)

  def changeDir(d: Direction): Unit = {
    if (d != dir.opposite && !reverseMode && !directionSwitched){
      dir = d
      directionSwitched = true
      snake(0) = (SnakeHead(d), snake(0)._2, snake(0)._3)
    }
  }

  def getGridTypeAt(x: Int, y: Int): GridType = {
    if (appleCoordinates == null) appleCoordinates = placeApple()
    if (appleCoordinates._1 == x && appleCoordinates._2 == y) {
      return Apple()
    }
    for (i <- snake.indices){
      if (snake(i)._2 == x && snake(i)._3 == y) return snake(i)._1
    }
    Empty()
  }

}

/** SnakeLogic companion object */
object SnakeLogic {

  val DefaultColumns = 20
  val DefaultRows = 20

}