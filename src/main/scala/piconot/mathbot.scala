package piconot

import java.io.File

import picolib.maze.Maze
import picolib.semantics.Anything
import picolib.semantics.Blocked
import picolib.semantics.East
import picolib.semantics.GUIDisplay
import picolib.semantics.North
import picolib.semantics.Open
import picolib.semantics.Picobot
import picolib.semantics.Rule
import picolib.semantics.South
import picolib.semantics.State
import picolib.semantics.Surroundings
import picolib.semantics.TextDisplay
import picolib.semantics.West
import scalafx.application.JFXApp
  
trait Mathbot {
  
  // used in case a user doesn't feel like typing in Greek letters
  // purely syntactic sugar for those who don't like being idiomatic
  val n = η
  val e = ε
  val w = ω
  val s = ς
  
  trait dir extends mathbotRule
  object η extends dir
  object ε extends dir
  object ω extends dir
  object ς extends dir
  
  // typedefs to make our lives easier
  type picoDir = picolib.semantics.RelativeDescription
  type picoMoveDir = picolib.semantics.MoveDirection
  type state = picolib.semantics.State
  
  implicit def Int2MathbotRule(state:Int) = new mathbotRule(State(state.toString))
  implicit def MathbotInstr2Rule(rule:mathbotInstr) = rule.convert
    
  /* 
   * Class: mathbotRule
   * contains operators for describing the surroundings for a picobot 
   * essentially, the lhs of a picobot rule
   * takes in information about current state and what is in each direction
   * default is state 0 with anything in each direction
   * */
  class mathbotRule(val currState: state = State("0"),
		  			val dirN:picoDir = picolib.semantics.Anything, 
      				val dirE:picoDir = picolib.semantics.Anything,
      				val dirW:picoDir = picolib.semantics.Anything,
      				val dirS:picoDir = picolib.semantics.Anything
		  			) {
    
    /*
     *  Operator that states there is a wall to the rhs direction
     * ex: + ω means there is a wall to the west
     */
    def +(rhs: dir): mathbotRule = {
       rhs match {
         case `η` => new mathbotRule(currState, picolib.semantics.Blocked, dirE, dirW, dirS)
         case `ε` => new mathbotRule(currState, dirN, picolib.semantics.Blocked, dirW, dirS)
         case `ω` => new mathbotRule(currState, dirN, dirE, picolib.semantics.Blocked, dirS)
         case `ς` => new mathbotRule(currState, dirN, dirE, dirW, picolib.semantics.Blocked)
       }
    }
    
    /* 
     * Operator that states there is nothing in the rhs direction
     * ex: - ω means there is nothing to the west
     */
    def -(rhs: dir): mathbotRule = {
       rhs match {
         case `η` => new mathbotRule(currState, picolib.semantics.Open, dirE, dirW, dirS)
         case `ε` => new mathbotRule(currState, dirN, picolib.semantics.Open, dirW, dirS)
         case `ω` => new mathbotRule(currState, dirN, dirE, picolib.semantics.Open, dirS)
         case `ς` => new mathbotRule(currState, dirN, dirE, dirW, picolib.semantics.Open)
       }
    }
    
    /*
     * Operator that declares the end of the lhs of a rule, returns
     * an Instr to get the instruction part of the rule
     */
    def ->(rhs: Int): mathbotInstr = {
        new mathbotInstr(State(rhs.toString), mathbot=this)
    }   
  }
  
  /*
   * Class: mathbotInstr
   * contains operators for describing what a picobot should do in a certain state/mathbotRule
   * essentially, the rhs of a picobot rule
   * takes in a state, a direction to go in, and a mathbotRule
   * default is to go to state 0 and stay put
   */
  class mathbotInstr(nextState:state = State("0"), 
      moveDir:picoMoveDir = picolib.semantics.StayHere, 
      mathbot: mathbotRule) {
    
    /* 
     * Essentially an operator that does nothing but set the
     * new direction to rhs
     */
    def +(rhs: dir): mathbotInstr = {
      rhs match {
	      case `η` => new mathbotInstr(nextState, picolib.semantics.North, mathbot)
	      case `ε` => new mathbotInstr(nextState, picolib.semantics.East, mathbot)
	      case `ω` => new mathbotInstr(nextState, picolib.semantics.West, mathbot)
	      case `ς` => new mathbotInstr(nextState, picolib.semantics.South, mathbot)
      }
      
    }
    
    //Does the same thing as +; gives the user more freedom in the language
    def -(rhs: dir): mathbotInstr = {
      rhs match {
	      case `η` => new mathbotInstr(nextState, picolib.semantics.North, mathbot)
	      case `ε` => new mathbotInstr(nextState, picolib.semantics.East, mathbot)
	      case `ω` => new mathbotInstr(nextState, picolib.semantics.West, mathbot)
	      case `ς` => new mathbotInstr(nextState, picolib.semantics.South, mathbot)
      }
      
    }
    
    def convert(): Rule = {
      Rule(this.mathbot.currState,
	      Surroundings(this.mathbot.dirN, this.mathbot.dirE, this.mathbot.dirW, this.mathbot.dirS),
	      this.moveDir,
	      this.nextState)
    }
  }
  
  object Proof {
    def Begin (maze: String): considerable = {
  		  new considerable(maze)
	}
  }
  
  class considerable(val maze: String) {
	  def Consider(rules: picolib.semantics.Rule*): qed = {
	    val emptyMaze = Maze("resources" + File.separator + maze)
	    object EmptyBot extends Picobot(emptyMaze, rules.toList)
	    	with TextDisplay with GUIDisplay
	    	
	    new qed(EmptyBot)
	  }
}
  class qed(bot: Unit) {
    val QED = {
      bot
    }
  }
}

