package communication;
import common.CMessageGenerator;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.LinkedList;
import java.util.List;

/**
 * Angelehnt an http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/8_4.html
 *
 * Diese Klasse stellt einen Serverdienst bereit, an den sich die Java und Prolog
 * Clients verbinden koennen. Fuer jeden Client wird ein eigener Thread vom Typ
 * CClientHandler gestartet.
 * Der Server nimmt maximal 2 Clients auf.
 * Der erste verbundene Client wird initial immer in den defend Status gesetzt, der
 * zweite immer auf attack.
 * Sobald zwei Clients verbunden wurden, wird vom Server ein Startsignal erzeugt und
 * an alle verschickt. Sollte die Verbindung zu einem Teilnehmer abbrechen, werden
 * alle Verbindungen getrennt.
 *
 * Der Server dient zentral als Knotenpunkt und verteilt eingehende Nachrichten an
 * alle anderen verbundenen Teilnehmer.
 *
 * @author Victor Apostel
 */
public class CCommunicationServer extends Thread {
    private ServerSocket m_server = null;
    private int m_port = 0;
    private List m_clients = new LinkedList<CClientHandler>();

    /**
     * Konstruktor, dem man den Serverport angeben muss.
     * 
     * @param port
     */
    public CCommunicationServer(int port) {
        m_port = port;
    }

    /**
     * Hauptthread, in dem der Server gestartet wird.
     * Des Weiteren erfolgt hier die Erzeugung des Startsignals, die Zuweisung
     * des initialen Spielerzustands und die Annahme von Verbindungen.
     */
    @Override
    public void run() {
        try {
            // Server starten
            m_server = new ServerSocket(m_port);
            System.out.println("Server started");

            // Hauptschleife
            while (true) {
                // wenn 2 Teilnehmer verbunden sind, wird ein Startsignal an beide
                // verschickt
                if (m_clients.size() == 2) {
                    generateStart();
                }
                // Neue Teilnehmer akzeptieren
                Socket socket = m_server.accept();
                System.out.println("Client conntected: " + socket.toString());
                // Dem ersten Teilnehmer eine Nachricht mit DEFENDFIRST schicken
                boolean initDefend = m_clients.isEmpty();
                // Verhindere, dass sich mehr als 2 Clients verbinden können
                if (m_clients.size() <= 2) {
                    // Thread fuer jeden Client starten
                    CClientHandler cHandle = new CClientHandler(m_clients, socket, initDefend);
                    // Und in Clientliste eintragen
                    m_clients.add(cHandle);
                    cHandle.start();
                } else {
                    socket = null;
                    System.out.println("connection refused");
                }
            }

        } catch (IOException ex) {
            System.out.println("CCommunicationServer::run - IOException");
            System.out.println(ex.toString());
        }
    }

    /**
     * Diese Methode erzeugt fuer jeden Client ein Startsignal
     * @throws IOException
     */
    private synchronized void generateStart() throws IOException {
        // Startsignal erzeugen
        String msg = CMessageGenerator.getInstance().generateStartSignal();
        // An alle Teilnehmer verschicken
        for (int i = 0; i < m_clients.size(); ++i) {
            ((CClientHandler)m_clients.get(i)).send(msg);
        }
    }
}
