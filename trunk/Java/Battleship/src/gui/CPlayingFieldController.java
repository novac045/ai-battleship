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
    // Statusbeschreibung fuer Felder:
    // - ERROR:             Ein Fehler ist aufgetreten
    // - UNKNOWN:           Der Feldstatus ist unbekannt.
    //                      Nur fuer die Repraesentation des gegnerischen Spielfelds gedacht
    // - WATER:             Das Feld beinhaltet Wasser
    // - HIT:               Ein Schiff wurde getroffen, aber noch nicht versenkt
    // - DESTROYED:         Ein Schiff wurde versenkt
    // - LASTSHIPDESTROYED: Signal, dass das letzte Schiff versenkt wurde
    // - SHIP:              Auf dem Feld befindet sich ein Schiff.
    //                      Nur fuer die Repraesentation des eigenen Spielfelds gedacht
    // - MISSED:            Der Gegner startete einen Angriff auf das Feld, aber hat nichts getroffen.
    //                      Nur fuer die Repraesentation des eigenen Spielfelds gedacht
    public enum FieldState {ERROR, UNKNOWN, WATER, HIT, DESTROYED, LASTSHIPDESTROYED, SHIP, MISSED};
    // Statusbeschreibung fuer eingehende Nachrichten
    // - UNKNOWN:           Eine Nachricht, die keinem der Status zugeordnet werden konnte
    // - ATTACK:            Angriff auf uebergebene Koordinate
    // - ATTACKRESPONSE:    Antwort auf Angriff
    // - ATTACKFIRST:       Der Spieler, der diese Nachricht empfaengt, faengt an
    // - DEFENDFIRST:       Der Spieler, der diese Nachricht empfaengt, wartet auf den Gegenspieler
    // - STARTGAME:         Startsignal, zur Synchronisation beider Clients
    public enum CommandState {UNKNOWN, ATTACK, ATTACKRESPONSE, ATTACKFIRST, DEFENDFIRST, STARTGAME};
    
    private FieldState[] m_enemyState = null;
    private FieldState[] m_ownState = null;
    private int m_width = 0;
    private int m_height = 0;
    private BufferedReader m_inStream = null;
    private BufferedWriter m_outStream = null;
    private Socket m_socket = null;
    private int m_port = 54321;
    private String m_host = "127.0.0.1";
    private boolean m_updateAvailable = true;
    private boolean m_itsMyTurn = true;
    private boolean m_gameInProgress = false;
    private boolean m_won = false;

    public CPlayingFieldController(int width, int height, String host, int port) {
        m_port = port;
        m_host = host;
        m_width = width;
        m_height = height;
        m_enemyState = new FieldState[m_width * m_height];
        m_ownState = new FieldState[m_width * m_height];

        for (int i = 0; i < m_width * m_height; ++i) {
            m_enemyState[i] = FieldState.UNKNOWN;
            m_ownState[i] = FieldState.WATER;
        }

        initShips();
    }

    private void initShips() {
        // TODO eigene Schiffe platzieren
    }

    public FieldState[] getEnemyStateVec() {
        return m_enemyState;
    }

    public FieldState[] getOwnStateVec() {
        return m_ownState;
    }

    @Override
    public void run() {
        try {
            m_socket = new Socket(m_host, m_port);
            m_inStream = new BufferedReader(new InputStreamReader(m_socket.getInputStream()));
            m_outStream = new BufferedWriter(new OutputStreamWriter(m_socket.getOutputStream()));

            // Wer beginnt?
            String whoIsDefending = m_inStream.readLine();
            handleIncomingMessage(whoIsDefending);
            // Startsignal empfangen
            String startSignal = m_inStream.readLine();
            handleIncomingMessage(startSignal);

            while(m_gameInProgress) {
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

    private synchronized String handleOp1(String paramList) throws CPlayingFieldControllerException {
        System.out.println("Coordinates: " + paramList);
        String[] xy = paramList.split(",");
        int x = Integer.parseInt(xy[0]);
        int y = Integer.parseInt(xy[1]);
        if (y >= m_height || x >= m_width || x < 0 || y < 0) {
            throw new CPlayingFieldControllerException("Coordinates (" + x + "/" + y +") out of range");
        }
        // Ausgabestate ermitteln und eigenen Feldstatus aktualisieren
        FieldState s = FieldState.UNKNOWN;
        FieldState ownFieldState = m_ownState[y * m_width + x];
        switch(ownFieldState) {
            case WATER:
                s = FieldState.WATER;
                m_ownState[y * m_width + x] = FieldState.MISSED;
                break;
            case SHIP:
                // TODO pruefen, ob das Schiff gaenzlich versenkt wurde
                boolean destroyed = false;
                if (destroyed) {
                    // TODO pruefen, ob alle eigenen Schiffe versenkt wurden
                    boolean allShipsDestroyed = false;
                    if (allShipsDestroyed) {
                        s = FieldState.DESTROYED;
                    } else {
                        s = FieldState.LASTSHIPDESTROYED;
                        m_gameInProgress = false;
                        m_won = false;
                    }
                    // TODO ganzes Schiff als zerstoert markieren
                    m_ownState[y * m_width + x] = FieldState.DESTROYED;
                } else {
                    s = FieldState.HIT;
                    m_ownState[y * m_width + x] = FieldState.HIT;
                }
                break;
            // Darf nicht passieren.
            // Wenn der Feldstatus MISSED, HIT, oder DESTROYED ist, wurde das
            // Feld bereits einmal angegriffen.
            default:
                throw new CPlayingFieldControllerException("Illegal field state: " + ownFieldState + " at (" + x + "/" + y +")");
        }
        String out = CMessageGenerator.getInstance().respondAttack(x, y, s);
        return out;
    }

    private synchronized String handleOp2(String paramList) throws CPlayingFieldControllerException {
        System.out.println("Coordinates: " + paramList);
        String[] xy = paramList.split(",");
        int x = Integer.parseInt(xy[0]);
        int y = Integer.parseInt(xy[1]);
        if (y >= m_height || x >= m_width || x < 0 || y < 0) {
            throw new CPlayingFieldControllerException("Coordinates (" + x + "/" + y +") out of range");
        }
        int stateInt = Integer.parseInt(xy[2]);
        // stateInt in FieldState umkonvertieren
        FieldState s = CMessageGenerator.getInstance().parseState(stateInt);
        switch (s) {
            case HIT:
                m_enemyState[y * m_width + x] = FieldState.HIT;
                break;
            case WATER:
                m_enemyState[y * m_width + x] = FieldState.WATER;
                break;
            case LASTSHIPDESTROYED:
                // TODO s == LASTSHIPDESTROYED -> Spiel als gewonnen deklarieren
                m_gameInProgress = false;
                m_won = true;
            case DESTROYED:
                // TODO s == DESTORYED -> ganzes Schiff markieren
                m_enemyState[y * m_width + x] = FieldState.DESTROYED;
                break;
        }
        
        return "";
    }

    private synchronized String handleIncomingMessage(String message) throws CPlayingFieldControllerException {
        // Messages:
        // - Attack Response XY State
        // - Attack XY

        // State:
        // - Miss
        // - Hit
        // - Destroyed
        // - Last Ship destroyed
        String out = "";
        System.out.println("Incoming message: " + message);
        // Nachrichten kommen in den beiden moeglichen Formaten an:
        //   Im Falle eines Prolog Clients: OPCODE,[PARAM1,PARAM2,PARAM3]
        //                                  OPCODE,[PARAM1,PARAM2]
        //                                  OPCODE,[]
        //   Im Falle eines Java Clients:   (OPCODE,[PARAM1,PARAM2,PARAM3])
        //                                  (OPCODE,[PARAM1,PARAM2])
        //                                  (OPCODE,[])
        Pattern p = Pattern.compile("^\\(?(\\d+),\\[(.*)\\]\\)?");
        Matcher m = p.matcher(message);
        boolean found = m.find();
        String code = m.group(1);
        System.out.println("Opcode: " + code);
        if (found) {
            int iCode = Integer.parseInt(code);
            CommandState s = CMessageGenerator.getInstance().parseCommand(iCode);
            switch(s) {
                // Ich werde angegriffen
                case ATTACK:
                    out = handleOp1(m.group(2));
                    break;
                // Reaktion des Gegners auf meinen Angriff
                case ATTACKRESPONSE:
                    out = handleOp2(m.group(2));
                    break;
                // Ich verteidige zuerst
                case DEFENDFIRST:
                    m_itsMyTurn = false;
                    break;
                // Ich greife zuerst an
                case ATTACKFIRST:
                    m_itsMyTurn = true;
                    break;
                // Startsignal empfangen
                case STARTGAME:
                    m_gameInProgress = true;
                    break;
                // Unbekannte Nachricht
                default:
                    throw new CPlayingFieldControllerException("Unknown message: " + message);
            }
        }
        return out;
    }

    private synchronized void defend() throws InterruptedException, IOException {
        while(m_itsMyTurn) {
            System.out.println("CPlayingFieldController::defend - waiting for remote turn");
            wait();
        }

        try {
            String msg = m_inStream.readLine();
            System.out.println("Defending: " + msg);
            String response = handleIncomingMessage(msg);
            System.out.println("Defence response: " + response);
            send(response);
        } catch (IOException ex) {
            System.out.println("CPlayingFieldController::defend - IOException");
            System.out.println(ex.toString());
        } catch (CPlayingFieldControllerException ex) {
            System.out.println("CPlayingFieldController::defend - CPlayingFieldControllerException");
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
            handleIncomingMessage(response);
        } catch (IOException ex) {
            System.out.println("CPlayingFieldController::attack - IOException");
            System.out.println(ex.toString());
        } catch (CPlayingFieldControllerException ex) {
            System.out.println("CPlayingFieldController::attack - CPlayingFieldControllerException");
            System.out.println(ex.toString());
        }
        m_updateAvailable = true;
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

    public synchronized List<FieldState[]> getUpdatedFields() throws InterruptedException {
        List<FieldState[]> states = new LinkedList<FieldState[]>();
        while (!m_updateAvailable) {
            System.out.println("CPlayingFieldController::getUpdatedFields - waiting for status update");
            wait();
        }
        states.add(m_enemyState);
        states.add(m_ownState);
        m_updateAvailable = false;
        return states;
    }

    public synchronized boolean isItMyTurn() {
        return m_itsMyTurn;
    }
}