/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package communication;
import gui.CMessageGenerator;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.LinkedList;
import java.util.List;

/**
 * Angelehnt an http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/8_4.html
 * @author victorapostel
 */
public class CCommunicationServer extends Thread {
    private ServerSocket m_server = null;
    private int m_port = 0;
    private List m_clients = new LinkedList<CClientHandler>();

    public CCommunicationServer(int port) {
        m_port = port;
    }

    @Override
    public void run() {
        try {
            m_server = new ServerSocket(m_port);
            System.out.println("Server started");

            while (true) {
                if (m_clients.size() == 2) {
                    generateStart();
                }
                Socket socket = m_server.accept();
                System.out.println("Client conntected: " + socket.toString());
                boolean initDefend = m_clients.isEmpty();
                // Verhindere, dass sich mehr als 2 Clients verbinden können
                if (m_clients.size() <= 2) {
                    CClientHandler cHandle = new CClientHandler(m_clients, socket, initDefend);
                    m_clients.add(cHandle);
                    cHandle.start();
                } else {
                    System.out.println("connection refused");
                }
            }

        } catch (IOException ex) {
            System.out.println("CCommunicationServer::run - IOException");
            System.out.println(ex.toString());
        }
    }

    private synchronized void generateStart() throws IOException {
        String msg = CMessageGenerator.getInstance().generateStartSignal();
        for (int i = 0; i < m_clients.size(); ++i) {
            ((CClientHandler)m_clients.get(i)).send(msg);
        }
    }
}
