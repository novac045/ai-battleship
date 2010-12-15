/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gui;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author victorapostel
 */
public class CPlayingFieldController extends Thread {
    public enum state {UNKNOWN, WATER, HIT, DESTROYED};
    private state[] m_enemyState = null;
    private state[] m_ownState = null;
    private int m_width = 0;
    private int m_height = 0;
    private BufferedReader m_inStream = null;
    private BufferedWriter m_outStream = null;
    private Socket m_socket = null;
    private int m_port = 54321;
    private String m_host = "127.0.0.1";
    private boolean m_updateAvailable = true;
    private boolean m_itsMyTurn = true;

    public CPlayingFieldController(int width, int height, String host, int port) {
        m_port = port;
        m_host = host;
        m_width = width;
        m_height = height;
        m_enemyState = new state[m_width * m_height];
        m_ownState = new state[m_width * m_height];

        for (int i = 0; i < m_width * m_height; ++i) {
            m_enemyState[i] = state.UNKNOWN;
            m_ownState[i] = state.WATER;
        }

        initShips();
    }

    private void initShips() {
        // TODO init own ships
    }

    public state[] getEnemyStateVec() {
        return m_enemyState;
    }

    public state[] getOwnStateVec() {
        return m_ownState;
    }

    private synchronized void handleStartingCode() throws IOException, CPlayingFieldControllerException {
        Pattern p = Pattern.compile("(\\d)");
        String whoIsDefending = m_inStream.readLine();
        Matcher m = p.matcher(whoIsDefending);
        boolean found = m.find();
        String code = m.group();
        System.out.println("Received initial defending state: " + whoIsDefending);
        if (found) {
            int iCode = Integer.parseInt(code);
            if (iCode == 3) {
                m_itsMyTurn = false;
            } else if (iCode == 4) {
                m_itsMyTurn = true;
            } else {
                throw new CPlayingFieldControllerException("Unknown opcode: " + whoIsDefending);
            }
        }
    }

    @Override
    public void run() {
        try {
            m_socket = new Socket(m_host, m_port);
            m_inStream = new BufferedReader(new InputStreamReader(m_socket.getInputStream()));
            m_outStream = new BufferedWriter(new OutputStreamWriter(m_socket.getOutputStream()));

            handleStartingCode();

            while(true) {
                try {
                    defend();
                } catch (IOException ex) {
                    System.out.println("CPlayingFieldController::connect::run - IOException");
                    System.out.println(ex.toString());
                } catch (InterruptedException ex) {
                    System.out.println("CPlayingFieldController::connect::run - InterruptedException");
                    System.out.println(ex.toString());
                }
            }
        } catch (UnknownHostException ex) {
            System.out.println("CPlayingFieldController::connect - UnknownHostException");
            System.out.println(ex.toString());
        } catch (IOException ex) {
            System.out.println("CPlayingFieldController::connect - IOException");
            System.out.println(ex.toString());
        } catch (CPlayingFieldControllerException ex) {
            System.out.println("CPlayingFieldController::connect - CPlayingFieldControllerException");
            System.out.println(ex.toString());
        }
      
    }

    private synchronized void handleIncomingMessage(String message) {
        // Messages:
        // - Attack Response XY State
        // - Attack XY

        // State:
        // - Miss
        // - Hit
        // - Destroyed
        // - Last Ship destroyed
    }

    private synchronized void defend() throws InterruptedException, IOException {
        while(m_itsMyTurn) {
            System.out.println("CPlayingFieldController::defend - waiting for remote turn");
            wait();
        }

        String msg = m_inStream.readLine();
        System.out.println("Defending: " + msg);
        handleIncomingMessage(msg);
        String response = CMessageGenerator.getInstance().respondAttack(5, 6, state.HIT);
        System.out.println("Defence response: " + response);
        try {
            send(response);
        } catch (IOException ex) {
            System.out.println("CPlayingFieldController::defend - IOException");
            System.out.println(ex.toString());
        }
        m_itsMyTurn = true;
        m_updateAvailable = true;
        notifyAll();
    }

    public synchronized void attack(int x, int y) throws InterruptedException {
        while (!m_itsMyTurn) {
            System.out.println("CPlayingFieldController::attack - waiting for my turn");
            wait();
        }
        m_itsMyTurn = false;
        String msg = CMessageGenerator.getInstance().attack(x, y);
        try {
            send(msg);
            String response = m_inStream.readLine();
            System.out.println("Attack response: " + response);
        } catch (IOException ex) {
            System.out.println("CPlayingFieldController::attack - IOException");
            System.out.println(ex.toString());
        }
        notifyAll();
    }

    private void send(String msg) throws IOException {
        if (m_outStream != null) {
            System.out.print("CPlayingFieldController::send: ");
            System.out.println(msg);
            m_outStream.write(msg + "\r");
            m_outStream.flush();
        }
    }

    public int getWidth() {
        return m_width;
    }

    public int getHeight() {
        return m_height;
    }

    public synchronized List<state[]> getUpdatedFields() throws InterruptedException {
        List<state[]> states = new LinkedList<state[]>();
        while (!m_updateAvailable) {
            System.out.println("CPlayingFieldController::getUpdatedFields - waiting for status update");
            wait();
        }
        states.add(m_enemyState);
        states.add(m_ownState);
        m_updateAvailable = false;
        return states;
    }
}
