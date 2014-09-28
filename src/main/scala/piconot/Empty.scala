package piconot
import scalafx.application.JFXApp

object Empty extends JFXApp with Mathbot {
  Proof. Begin ("empty.txt"). Consider (
        0 - η -> 0 - η,
		0 + η - ε - ω -> 1 + ω,
		0 + η - ε + ω -> 2 - ε,
		0 + η + ε -> 1,
		
		1 - ς -> 1 + ς,
		1 + ς - ω -> 0 + ω,
		1 + s + ω -> 2 - ε,
		
		2 - ε -> 2 + ε,
		2 + ε -> 0
  ). QED
}