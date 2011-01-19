package communication;
import common.CMessageGenerator;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

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
    Random m_RNG = new Random();
    // Fifo Liste mit Clients, die sich verbunden haben, aber noch nicht bedient werden koennen
    private List<Socket> m_pending = Collections.synchronizedList(new LinkedList<Socket>());

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
                // Neue Teilnehmer akzeptieren
                if (m_pending.size() <= 2) {
                    Socket socket = null;
                    socket = m_server.accept();
                    m_pending.add(socket);
                    System.out.println("Client conntected: " + socket.toString());
                }
                // Dem ersten Teilnehmer eine Nachricht mit DEFENDFIRST schicken
                setUpGame(m_pending);
            }

        } catch (IOException ex) {
            System.out.println("CCommunicationServer::run - IOException");
            System.out.println(ex.toString());
        }
    }

    /**
     * Erzeugt ein Spiel, sobald sich zwei Clients verbunden haben.
     *
     * @param clients   Liste mit Clients, die auf ein Spiel warten
     */
    public synchronized void setUpGame(List<Socket> clients) {
        if (clients.size() >= 2) {
            try {
                List<CClientHandler> participants = new LinkedList<CClientHandler>();
                // Die Sockets der Clients holen und loeschen
                Socket s1 = clients.get(0);
                Socket s2 = clients.get(1);
                clients.remove(s1);
                clients.remove(s2);
                int randomNumber = m_RNG.nextInt(2);
                boolean attackFirst = (randomNumber > 0);
                // Daraus Objekte erzeugen
                CClientHandler cHandle1 = new CClientHandler(participants, s1, attackFirst);
                CClientHandler cHandle2 = new CClientHandler(participants, s2, !attackFirst);
                // Clients in die lokale temporaere Liste eintragen, die die Spielteilnehmer
                // identifiziert
                participants.add(cHandle1);
                participants.add(cHandle2);
                // Clients starten
                cHandle1.start();
                cHandle2.start();
                // und das Startsignal versenden
                String startSignal = CMessageGenerator.getInstance().generateStartSignal();
                cHandle1.send(startSignal);
                cHandle2.send(startSignal);
            } catch (IOException ex) {
                System.out.println("CCommunicationServer::setUpGame() - IOException");
                System.out.println(ex.toString());
            }
        }
    }
}
