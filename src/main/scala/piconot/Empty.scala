package piconot
import scalafx.application.JFXApp

object Empty extends JFXApp with ruleClass {
  Proof. Begin ("empty.txt"). Consider (
        0 - n -> 0 * n,
		0 + n - e - w -> 1 + w,
		0 + n - e + w -> 2 - e,
		0 + n + e -> 1,
		
		1 - s -> 1 + s,
		1 + s - w -> 0 + w,
		1 + s + w -> 2 - e,
		
		2 - e -> 2 + e,
		2 + e -> 0
  ). QED
}