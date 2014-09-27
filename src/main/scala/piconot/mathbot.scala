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




//class mathbot(val ruleList: List[ruleClass.mathbotRule]) {
//  
//}
  
trait ruleClass {
  	
  trait dir extends mathbotRule
  class n
  object n extends dir
  class e
  object e extends dir
  class w
  object w extends dir
  class s
  object s extends dir
  
  type picoDir = picolib.semantics.RelativeDescription
  type picoMoveDir = picolib.semantics.MoveDirection
  type state = picolib.semantics.State
  
  implicit def Int2MathbotRule(state:Int) = new mathbotRule(State(state.toString))
  implicit def MathbotInstr2Rule(rule:mathbotInstr) = rule.convert
    
  class mathbotRule(val currState: state = State("0"),
		  			val dirN:picoDir = picolib.semantics.Anything,
      				val dirE:picoDir = picolib.semantics.Anything,
      				val dirW:picoDir = picolib.semantics.Anything,
      				val dirS:picoDir = picolib.semantics.Anything
		  			) {
    //def this() = this(picolib.semantics.Anything, picolib.semantics.Anything, picolib.semantics.Anything, picolib.semantics.Anything)
    
    def +(rhs: dir): mathbotRule = {
       rhs match {
         case `n` => new mathbotRule(currState, picolib.semantics.Blocked, dirE, dirW, dirS)
         case `e` => new mathbotRule(currState, dirN, picolib.semantics.Blocked, dirW, dirS)
         case `w` => new mathbotRule(currState, dirN, dirE, picolib.semantics.Blocked, dirS)
         case `s` => new mathbotRule(currState, dirN, dirE, dirW, picolib.semantics.Blocked)
         //default => new mathbotRule(picolib.semantics.Blocked, picolib.semantics.Blocked, picolib.semantics.Blocked, picolib.semantics.Blocked)
       }
    }
    
    def *(rhs: dir): mathbotRule = {
       rhs match {
         case `n` => new mathbotRule(currState, picolib.semantics.Anything, dirE, dirW, dirS)
         case `e` => new mathbotRule(currState, dirN, picolib.semantics.Anything, dirW, dirS)
         case `w` => new mathbotRule(currState, dirN, dirE, picolib.semantics.Anything, dirS)
         case `s` => new mathbotRule(currState, dirN, dirE, dirW, picolib.semantics.Anything)
       }
    }
    
    def -(rhs: dir): mathbotRule = {
       rhs match {
         case `n` => new mathbotRule(currState, picolib.semantics.Open, dirE, dirW, dirS)
         case `e` => new mathbotRule(currState, dirN, picolib.semantics.Open, dirW, dirS)
         case `w` => new mathbotRule(currState, dirN, dirE, picolib.semantics.Open, dirS)
         case `s` => new mathbotRule(currState, dirN, dirE, dirW, picolib.semantics.Open)
       }
    }
    
    def ->(rhs: Int): mathbotInstr = {
        new mathbotInstr(State(rhs.toString), mathbot=this)
//        new mathbotRule (currState, dirN, dirE, dirW, dirS, instr)
    }
    
    
    
  }
  
  class mathbotInstr(nextState:state = State("0"), 
      moveDir:picoMoveDir = picolib.semantics.StayHere, 
      mathbot: mathbotRule) {
    
    def +(rhs: dir): mathbotInstr = {
      rhs match {
	      case `n` => new mathbotInstr(nextState, picolib.semantics.North, mathbot)
	      case `e` => new mathbotInstr(nextState, picolib.semantics.East, mathbot)
	      case `w` => new mathbotInstr(nextState, picolib.semantics.West, mathbot)
	      case `s` => new mathbotInstr(nextState, picolib.semantics.South, mathbot)
      }
      
    }
    
    def -(rhs: dir): mathbotInstr = {
      rhs match {
	      case `n` => new mathbotInstr(nextState, picolib.semantics.North, mathbot)
	      case `e` => new mathbotInstr(nextState, picolib.semantics.East, mathbot)
	      case `w` => new mathbotInstr(nextState, picolib.semantics.West, mathbot)
	      case `s` => new mathbotInstr(nextState, picolib.semantics.South, mathbot)
      }
      
    }
    
    def convert(): Rule = {
      Rule(this.mathbot.currState,
	      Surroundings(this.mathbot.dirN, this.mathbot.dirE, this.mathbot.dirW, this.mathbot.dirS),
	      this.moveDir,
	      this.nextState)
    }
        
    // * works weird
//    def *(rhs: dir): mathbotInstr = {
//      rhs match {
//	      case `n` => new mathbotInstr(nextState, picolib.semantics.North, mathbot)
//	      case `e` => new mathbotInstr(nextState, picolib.semantics.East, mathbot)
//	      case `w` => new mathbotInstr(nextState, picolib.semantics.West, mathbot)
//	      case `s` => new mathbotInstr(nextState, picolib.semantics.South, mathbot)
//      }
//    }
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

