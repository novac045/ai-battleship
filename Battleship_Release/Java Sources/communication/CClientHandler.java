package communication;

import common.CMessageGenerator;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.List;

/**
 * Die Klasse CClientHandler uebernimmt die Koordination eines verbundenen Clients.
 * Die Hauptaufgaben belaufen sich auf die Weiterleitung von eingehenden Nachrichten
 * und Erzeugung von Startzustaenden.
 *
 * @author Victor Apostel
 */
public class CClientHandler extends Thread {
    private Socket m_socket = null;
    private BufferedReader m_inStream = null;
    private BufferedWriter m_outStream = null;
    private List<CClientHandler> m_otherClients = null;

    /**
     * Der Konstruktor erzeugt die Socketstreams und sendet in Abhaengigkeit vom
     * Parameter initDefend eine DEFENDFIRST, oder ATTACKFIRST Nachricht. Die Bekanntgabe
     * erfolgt durch die gemeinsame Liste des Servers mit allen verbundenen Clients.
     *
     * @param clients   Liste mit allen verbundenen Clients
     * @param socket    Socket zum Client
     * @param initDefend    Gibt an, ob sich der Client im ATTACK oder DEFEND Status starten soll
     */
    public CClientHandler(List<CClientHandler> clients, Socket socket, boolean initDefend) {
        try {
            m_otherClients = clients;
            m_socket = socket;
            m_inStream = new BufferedReader(new InputStreamReader(m_socket.getInputStream()));
            m_outStream = new BufferedWriter(new OutputStreamWriter(m_socket.getOutputStream()));
            if (initDefend) {
                send(CMessageGenerator.getInstance().defendFirst());
            } else {
                send(CMessageGenerator.getInstance().attackFirst());
            }
            m_outStream.flush();
        } catch (IOException ex) {
            System.out.println("CClientHandler::CClientHandler() - IOException");
            System.out.println(ex.toString());
        }
        
    }

    /**
     * Hauptschleife, die eingehende Nachrichten an alle verbundenen Clients
     * weiterleitet. Sollte eine Verbindung verloren gehen, werden alle Clients
     * vom Server getrennt.
     */
    @Override
    public void run() {
        try {
            while (true) {
                String line = m_inStream.readLine();
                // Verbindung verloren
                if (line == null) {
                    throw new IOException("Connection lost");
                // Nachricht empfangen und an GUI weiterleiten
                } else {
                    notifyAllOtherClients(line);
                }
            }
        } catch (IOException ex) {
            System.out.println("CClientHandler::run() - IOException");
            System.out.println(ex.toString());
        } finally {
            disconnectAllClients();
        }
    }

    /**
     * Wenn ein Spieler die Verbindung verliert, dann werden alle
     * Verbindungen getrennt. Dadurch wird eine komplizierte
     * Verarbeitung des anfaenglichen Verteidigungsstatus vermieden
     */
    private synchronized void disconnectAllClients() {
        try {
            close();
            m_otherClients.remove(this);
            /*
            for (CClientHandler handle : m_otherClients) {
            try {
            handle.close();
            } catch (IOException ex) {
            System.out.println("CClientHandler::disconnectAllClients() - IOException");
            System.out.println(ex.toString());
            }
            }
            m_otherClients.clear();
             *
             */
            System.out.println("Disconnected myself");
        } catch (IOException ex) {
            System.out.println("CClientHandler::disconnectAllClients() - IOException");
            System.out.println(ex.toString());
        }
    }

    /**
     * Leitet eine eingehende Nachricht an alle Teilnehmer weiter, bis auf sich selbst.
     * @param msg Die Nachricht
     * @throws IOException
     */
    private synchronized void notifyAllOtherClients(String msg) throws IOException {
        for (CClientHandler handle : m_otherClients) {
            if (handle != this) {
                handle.send(msg);
            }
        }
    }

    /**
     * Mit dieser Methode wird das eigentliche Senden ausgeführt. Am Ende der
     * Nachricht wird stets eine neue Zeile angefuegt und der Zwischenspeicher
     * wird immer sofort geleert.
     *
     * @param msg
     * @throws IOException
     */
    public void send(String msg) throws IOException {
        System.out.println("" + this + ": Sending message: " + msg);
        m_outStream.write(msg + "\r");
        m_outStream.flush();
    }

    /**
     * Schliessen der Verbindung
     * @throws IOException
     */
    public synchronized void close() throws IOException {
        m_socket.close();
        m_socket = null;
    }
}
