import java.awt.Color
import scala.swing._

class UI extends MainFrame {
  val la = new Label("Hello I am label!")

  la.foreground = Color.BLUE
  title = "Bean bois"

  contents = new BoxPanel(Orientation.Vertical) {
    contents += la
    contents += Swing.VStrut(10)
    contents += Swing.Glue
    contents += Button("Press me for beans!") { pressMe() }
    contents += Swing.VStrut(5)
    contents += Button("Change text") { changeText() }
    contents += Swing.VStrut(5)
    contents += Button("Close") { closeMe() }
    border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  def pressMe() {
    val t = "Thank you for pressing me!"
    val msg = "Beans!!!"
    Dialog.showMessage(contents.head, msg, title=t)
  }

  def changeText() {
    val r = Dialog.showInput(contents.head, "New label text", initial=la.text)
    r match {
      // Some basically handles exceptions for you, in case (somehow)
      // there was nonString, nonNone input
      case Some(s) => la.text = s
      case None =>
    }
  }

  def closeMe() {
    val res = Dialog.showConfirmation(contents.head,
				      "Do you really want to quit?",
				      optionType=Dialog.Options.YesNo,
				      title=title)
    Dialog.showMessage(contents.head, "Too bad", title = "Yeet")
    if (res == Dialog.Result.No)
      sys.exit(0)
  }
}

object GuiDemoProgram {
  def main(args: Array[String]) {
    val ui = new UI
    ui.visible = true
  }
}
