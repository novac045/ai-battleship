/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package battleship;

import communication.CCommunicationServer;

/**
 *
 * @author victorapostel
 */
public class ServerMain {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        CCommunicationServer server = new CCommunicationServer(54321);
        server.start();
    }

}
