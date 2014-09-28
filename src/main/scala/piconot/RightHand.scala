package piconot
import scalafx.application.JFXApp

object RightHand extends JFXApp with Mathbot {
  Proof. Begin ("maze.txt"). Consider (
      0 - η + ε -> 0 + η,
      0 - ε -> 1 - ε,
      0 + η + ε -> 2,
      1 - ε + ς -> 1 + ε,
      1 - ς -> 3 - ς,
      1 + ε + ς -> 0,
      2 + η - ω -> 2 - ω,
      2 - η -> 0 + η,
      2 + η + ω -> 3,
      3 + ω - ς -> 3 - ς,
      3 - ω -> 2 - ω,
      3 + ω + ς -> 1
      ). QED
}