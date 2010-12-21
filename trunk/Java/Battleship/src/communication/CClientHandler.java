/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package communication;

import gui.CMessageGenerator;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.util.List;

/**
 *
 * @author victorapostel
 */
public class CClientHandler extends Thread {
    private Socket m_socket = null;
    private BufferedReader m_inStream = null;
    private BufferedWriter m_outStream = null;
    private List<CClientHandler> m_otherClients = null;

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
            // Wenn ein Spieler die Verbindung verliert, dann werden alle
            // Verbindungen getrennt. Dadurch wird eine komplizierte
            // Verarbeitung des anfänglichen Verteidigungsstatus vermieden
            for (CClientHandler handle : m_otherClients) {
                try {
                    handle.close();
                } catch (IOException ex) {
                    System.out.println("CClientHandler::run()::finally - IOException");
                    System.out.println(ex.toString());
                }
            }
            m_otherClients.clear();
            System.out.println("Disconnected all clients");
        }
    }

    private synchronized void notifyAllOtherClients(String msg) throws IOException {
        System.out.print("Forwarding message: " + msg);
        for (CClientHandler handle : m_otherClients) {
            if (handle != this) {
                System.out.print(" _");
                handle.send(msg);
            }
        }
        System.out.println("");
    }

    public void send(String msg) throws IOException {
        m_outStream.write(msg + "\r");
        m_outStream.flush();
    }

    public void close() throws IOException {
        m_socket.close();
    }
}
