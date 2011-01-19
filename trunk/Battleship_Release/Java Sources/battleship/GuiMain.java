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
        String host = "127.0.0.1";
        int port = 54321;
        for (String arg : args) {
            if (arg.contains("-h")) {
                String h = arg.replaceAll("-h", "");
                host = h;
            } else if (arg.contains("-p")) {
                String p = arg.replaceAll("-p", "");
                port = Integer.parseInt(p);
            }
        }
        System.out.println(host);
        System.out.println(port);
        CPlayingFieldController control = new CPlayingFieldController(10, 10, host, port);
        CBattleshipGUI gui = new CBattleshipGUI(control);
        Thread mainThread = new Thread(control);
        mainThread.start();
    }

}
