package piconot
import scalafx.application.JFXApp

object RightHand extends JFXApp with ruleClass {
  Proof. Begin ("maze.txt"). Consider (
      0 - n + e -> 0 + n,
      0 - e -> 1 - e,
      0 + n + e -> 2,
      1 - e + s -> 1 + e,
      1 - s -> 3 - s,
      1 + e + s -> 0,
      2 + n - w -> 2 - w,
      2 - n -> 0 + n,
      2 + n + w -> 3,
      3 + w - s -> 3 - s,
      3 - w -> 2 - w,
      3 + w + s -> 1
      ). QED
}