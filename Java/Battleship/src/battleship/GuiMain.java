package battleship;

import gui.CBattleshipGUI;
import gui.CPlayingFieldController;

/**
 *
 * @author victorapostel
 */
public class GuiMain {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        CPlayingFieldController control = new CPlayingFieldController(10, 10, "127.0.0.1", 54321);
        CBattleshipGUI gui = new CBattleshipGUI(control);
        control.run();
    }

}
