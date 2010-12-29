package gui;

import common.CMessageGenerator;
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
 * Die Klasse CPlayingFieldController beinhaltet die gesamte Spiellogik und den
 * aktuellen Status eines laufenden Spiels.
 * Der Spielstatus wird mittels eines Enumerationarrays vom Typ FieldState abgebildet.
 * Der Spielstatus hingegen mit der Enumeration GameState.
 * Die Klasse CPlayingFieldController beinhaltet des Weiteren Objekte zur Kommunikation
 * ueber Sockets mit dem CCommunicationServer.
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
    public enum GameState {INITIALIZATION, RUNNING, WON, LOST};

    // Abbildung des aktuellen Spielstatus
    private FieldState[] m_enemyState = null;
    private FieldState[] m_ownState = null;
    // Spielfeldgroesse
    private int m_width = 0;
    private int m_height = 0;
    // Streams zur Kommunikation mit dem Server
    private BufferedReader m_inStream = null;
    private BufferedWriter m_outStream = null;
    private Socket m_socket = null;
    // Verbindungsdaten
    private int m_port = 54321;
    private String m_host = "127.0.0.1";
    // Variablen zur Signalisierung von aktualisierten Daten, die von der GUI
    // abgeholt werden koennen
    private boolean m_updateAvailable = true;
    private boolean m_itsMyTurn = true;
    // Aktueller Spielstatus
    private GameState m_gameState = GameState.INITIALIZATION;

    /**
     * Konstruktor, der den Aufbau der Spielfelder uebernimmt und die eigenen
     * Schiffe platziert
     * 
     * @param width Spielfeldbreite
     * @param height Spielfeldhoehe
     * @param host  Adresse zum CCommunicationServer
     * @param port  Port des CCommunicationServers
     */
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

    /**
     * Schiffe mithilfe der CShipPlacementController Klasse plazieren
     */
    private void initShips() {
        CShipPlacementController shipPlacer = new CShipPlacementController(m_width, m_height);
        m_ownState = shipPlacer.getNewBoard();
    }

    /**
     * Get Methode zum Abrufen des aktuellen Spielstatus
     * @return
     */
    public synchronized GameState getGameState() {
        return m_gameState;
    }

    /**
     * Get Methode zum Abrufen des gegnerischen Spielfelds
     * @return
     */
    public FieldState[] getEnemyStateVec() {
        return m_enemyState;
    }

    /**
     * get Methode zum Abrufen des eigenen Spielfelds
     * @return
     */
    public FieldState[] getOwnStateVec() {
        return m_ownState;
    }

    /**
     * Hauptthread, der die Nachrichten des Servers entgegennimmt.
     * Eingangs wird die Verbindung zum Server hergestellt.
     * Anschliessend werden zwei Nachrichten empfangen.
     * Die erste gibt an, welcher Client anfaengt. Die zweite stellt
     * das Startsignal dar.
     *
     * In der Hauptschleife wird stets die Methode defend aufgerufen.
     * Sollte sich der Client im Zustand attack befinden, blockiert diese Methode
     * und der Thread wird schlafen gelegt.
     */
    @Override
    public void run() {
        try {
            // Verbindung herstellen
            m_socket = new Socket(m_host, m_port);
            m_inStream = new BufferedReader(new InputStreamReader(m_socket.getInputStream()));
            m_outStream = new BufferedWriter(new OutputStreamWriter(m_socket.getOutputStream()));

            // Wer beginnt?
            String whoIsDefending = m_inStream.readLine();
            handleIncomingMessage(whoIsDefending);
            // Startsignal empfangen
            String startSignal = m_inStream.readLine();
            handleIncomingMessage(startSignal);

            while(m_gameState == GameState.RUNNING) {
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

    /**
     * Prueft, ob noch Felder mit der Enumeration SHIP uebrig geblieben sind.
     * Ist dies nicht der Fall, wird false zurueckgegeben, andernfalls true
     * @return true, wenn noch Felder mit dem Status SHIP existieren
     */
    private synchronized boolean areAnyShipsLefts() {
        for (int i = 0; i < m_ownState.length; ++i) {
            if (m_ownState[i] == FieldState.SHIP) {
                return true;
            }
        }
        return false;
    }

    /**
     * Wurde ein Schiff getroffen, muss geprueft werden, ob das ganze Schiff versenkt wurde.
     * Dies erfolgt mit Hilfe dieser rekursiven Methode. Ausgehend von einer Schiffskoordinate,
     * die mit den Parametern x/y angegeben wird, werden die Nachbarfelder untersucht.
     * Ein Schiff wird als versenkt deklariert, wenn kein benachbartes und zusammenhaengendes
     * Feld mit dem Status SHIP gefunden wird.
     * Die Untersuchung findet in einer 4er Nachbarschaft statt, da diagonale Schiffe nicht erlaubt sind.
     * Um eine rekursive Endlosschleife zu vermeiden, werden ebenfalls die alten Koordinaten
     * uebermittelt. Die Schleife endet ebenfalls, wenn das untersuchte Feld nicht vom Typ HIT ist.
     *
     *
     * @param x Aktuelle x Koordinate, die untersucht werden soll
     * @param y Aktuelle y Koordinate, die untersucht werden soll
     * @param board Zu untersuchendes Spielfeld
     * @param oldX  Vorangegangene x Koordiante
     * @param oldY  Vorangegangene y Koordiante
     * @return  true, wenn kein Feld mit dem Status SHIP gefunden wurde
     */
    private synchronized boolean isShipCompletlyDestroyed(int x, int y, FieldState[] board, int oldX, int oldY) {
        boolean north = true;
        boolean south = true;
        boolean west = true;
        boolean east = true;
        //System.out.println("  X: " + x + " Y: " + y);
        if (x < 0 || x >= m_width || y < 0 || y >= m_height) {
            return true;
        }
        if (board[y * m_width + x] == FieldState.SHIP) {
            return false;
        } else if (board[y * m_width + x] == FieldState.HIT) {
            if (x + 1 != oldX) {
                east = isShipCompletlyDestroyed(x + 1, y, board, x, y);
            }
            if (x - 1 != oldX) {
                west = isShipCompletlyDestroyed(x - 1, y, board, x, y);
            }
            if (y - 1 != oldY) {
                north = isShipCompletlyDestroyed(x, y - 1, board, x, y);
            }
            if (y + 1 != oldY) {
                south = isShipCompletlyDestroyed(x, y + 1, board, x, y);
            }
            return north && south && west && east;
        } else {
            return true;
        }
    }

    /**
     * Sollte festgestellt werden, dass ein Schiff gaenzlich zerstoert wurde, so
     * muss dies im Spielfeld auch gespeichert werden. Mit Hilfe dieser rekursiven
     * Methode werden alle benachbarten Felder, die den Status HIT aufweisen mit
     * dem neuen Status DESTROYED versehen.
     *
     * @param x Aktuelle x Koordinate
     * @param y Aktuelle y Koordinate
     * @param board Zu aktualisierendes Spielfeld
     */
    private synchronized void markWholeShipAsDestroyed(int x, int y, FieldState[] board) {
        // Pruefe, ob wir uns noch im Spielfeld befinden
        if (x < 0 || x >= m_width || y < 0 || y >= m_height) {
            return;
        }
        // Pruefe, ob es sich bei dem Feld um ein beschaedigtes Schiff handelt
        if (board[y * m_width + x] != FieldState.HIT) {
            return;
        }
        board[y * m_width + x] = FieldState.DESTROYED;
        // Rufe Methode rekursiv in seiner 4er Nachbarschaft auf
        markWholeShipAsDestroyed(x+1, y, board);
        markWholeShipAsDestroyed(x-1, y, board);
        markWholeShipAsDestroyed(x, y+1, board);
        markWholeShipAsDestroyed(x, y-1, board);
    }

    /**
     * Wurde eine Nachricht empfangen, die angibt, dass das eigene Spielfeld angegriffen
     * wird, so wird diese Methode aufgerufen. Sie ueberprueft die uebergebene Koordinate
     * und gibt an, ob sich auf dem eigenen Spielfeld an dieser Position ein Schiff befand.
     * Das Ergebnis wird in Form eines Strings zurueckgegeben.
     *
     * @param paramList Kodierte Parameterliste in der Form X,Y
     * @return  Kodierter String mit dem Ergebnis des Angriffs.
     * @throws CPlayingFieldControllerException
     */
    private synchronized String handleOp1(String paramList) throws CPlayingFieldControllerException {
        // Parameterliste dekodieren
        System.out.println("Parameterlist: " + paramList);
        String[] xy = paramList.split(",");
        //System.out.println("Arraylength: " + xy.length);
        int x = Integer.parseInt(xy[0]);
        int y = Integer.parseInt(xy[1]);
        // Pruefen, ob die Koordinaten sich innerhalb des Spielfelds befinden
        if (y >= m_height || x >= m_width || x < 0 || y < 0) {
            throw new CPlayingFieldControllerException("Coordinates (" + x + "/" + y +") out of range");
        }
        //System.out.println("X: " + x + " Y:" + y);
        // Ausgabestate ermitteln und eigenen Feldstatus aktualisieren
        FieldState s = FieldState.UNKNOWN;
        FieldState ownFieldState = m_ownState[y * m_width + x];
        switch(ownFieldState) {
            // Der Gegner hat Wasser getroffen.
            case WATER:
                s = FieldState.WATER;
                m_ownState[y * m_width + x] = FieldState.MISSED;
                break;
            // Der Gegner hat ein Schiff getroffen
            case SHIP:
                m_ownState[y * m_width + x] = FieldState.HIT;
                // pruefen, ob das Schiff gaenzlich versenkt wurde
                System.out.println("Pruefe, ob das ganze Schiff versenkt wurde");
                boolean destroyed = isShipCompletlyDestroyed(x, y, m_ownState, x, y);
                if (destroyed) {
                    // pruefen, ob alle eigenen Schiffe versenkt wurden
                    if (areAnyShipsLefts()) {
                        s = FieldState.DESTROYED;
                    } else {
                        s = FieldState.LASTSHIPDESTROYED;
                        m_gameState = GameState.LOST;
                    }
                    // ganzes Schiff als zerstoert markieren
                    markWholeShipAsDestroyed(x, y, m_ownState);
                } else {
                    s = FieldState.HIT;
                }
                break;
            // Darf nicht passieren.
            // Wenn der Feldstatus MISSED, HIT, oder DESTROYED ist, wurde das
            // Feld bereits einmal angegriffen.
            default:
                throw new CPlayingFieldControllerException("Illegal field state: " + ownFieldState + " at (" + x + "/" + y +")");
        }
        // Aus dem Status eine Nachricht erzeugen
        String out = CMessageGenerator.getInstance().respondAttack(x, y, s);
        return out;
    }

    /**
     * Wenn ein eigener Angriff erfolgt ist, muss die Antwort des Gegenspielers ausgewertet
     * werden. Dies erfolgt mit Hilfe dieser Methode. Neben den Nachrichten "getroffen" und
     * "Wasser" koennen ebenfalls die Meldungen "versenkt" und "letztes Schiff versenkt"
     * ankommen.
     * In diesen beiden letzten Faellen werden alle Felder des Schiffes als zerstoert markiert
     * bzw. zusaetlich noch der Spielzustand auf gewonnen geaendert.
     * 
     * @param paramList
     * @return Leerer String
     * @throws CPlayingFieldControllerException
     */
    private synchronized String handleOp2(String paramList) throws CPlayingFieldControllerException {
        // Nachicht dekodieren
        System.out.println("Parameterlist: " + paramList);
        String[] xy = paramList.split(",");
        int x = Integer.parseInt(xy[0]);
        int y = Integer.parseInt(xy[1]);
        // Pruefen, ob die Koordinate sich im Spielfeld befindet.
        if (y >= m_height || x >= m_width || x < 0 || y < 0) {
            throw new CPlayingFieldControllerException("Coordinates (" + x + "/" + y +") out of range");
        }
        // Status decodieren
        //System.out.println("X: " + x + " Y:" + y);
        int stateInt = Integer.parseInt(xy[2]);
        // stateInt in FieldState umkonvertieren
        FieldState s = CMessageGenerator.getInstance().parseState(stateInt);
        switch (s) {
            // Ein gegnerisches Schiff wurde getroffen
            case HIT:
                m_enemyState[y * m_width + x] = FieldState.HIT;
                break;
            // Der Schuss ging ins Wasser
            case WATER:
                m_enemyState[y * m_width + x] = FieldState.WATER;
                break;
            // Das letzte gegnerische Schiff wurde versenkt
            // Den Spielstatus auf gewonnen setzen und zusaetlich die
            // Aktionen von DESTROYED ausfuehren
            case LASTSHIPDESTROYED:
                // Spiel als gewonnen deklarieren
                m_gameState = GameState.WON;
            // Ein Schiff wurde zerstoert
            // Markiere alle Schiffsfelder mit DESTROYED
            case DESTROYED:
                m_enemyState[y * m_width + x] = FieldState.HIT;
                // ganzes Schiff als zerstoert markieren
                markWholeShipAsDestroyed(x, y, m_enemyState);
                break;
            // Unbekannter Status wurde uebermittelt
            default:
                throw new CPlayingFieldControllerException("Unknown state received in Opcode 2: " + s);
        }
        
        return "";
    }

    /**
     * Diese Methode analysiert die eingehende Nachricht anhand des Opcodes, der
     * durch die erste Ziffer in der Nachricht repraesentiert wird. Diese
     * Decodierung findet mittels eines regulaeren Ausdrucks statt.
     *
     * @param message
     * @return
     * @throws CPlayingFieldControllerException
     */
    private synchronized String handleIncomingMessage(String message) throws CPlayingFieldControllerException {
        if (message == null) {
            throw new CPlayingFieldControllerException("Empty message string");
        }
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

        // Nachricht mittels regulaeren Ausdrucks decodieren
        Pattern p = Pattern.compile("^\\(?(\\d+),\\[(.*)\\]\\)?");
        Matcher m = p.matcher(message);
        boolean found = m.find();
        // Opcode extrahieren
        String code = m.group(1);
        System.out.println("Opcode: " + code);
        
        if (found) {
            // Opcode in Integer und anschliessend in Enumeration umwandeln
            int iCode = Integer.parseInt(code);
            CommandState s = CMessageGenerator.getInstance().parseCommand(iCode);
            // Opcode einer Aktion zuweisen
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
                    m_gameState = GameState.RUNNING;
                    break;
                // Unbekannte Nachricht
                default:
                    throw new CPlayingFieldControllerException("Unknown message: " + message);
            }
        }
        return out;
    }

    /**
     * Auf den Angriff eines Gegners reagieren.
     * Der Gegner uebermittelt eine Nachricht mit dem Opcode 1 und den Koordinaten.
     * Diese Nachricht wird hier empfangen und eine Antwort wird generiert.
     * Anschliessend werden alle GUI Threads benachrichtigt, dass die Statusfelder
     * aktualisiert worden sind.
     *
     * @throws InterruptedException
     * @throws IOException
     */
    private synchronized void defend() throws InterruptedException, IOException {
        // Verhindere das Schlafenlegen, wenn das Spiel beendet ist, damit die GUI
        // nicht einfriert.
        while(m_itsMyTurn && m_gameState == GameState.RUNNING) {
            System.out.println("CPlayingFieldController::defend - waiting for remote turn");
            wait();
        }
        if (m_gameState != GameState.RUNNING) {
            notifyAll();
            return;
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
        m_itsMyTurn = (m_gameState == GameState.RUNNING);
        m_updateAvailable = true;
        notifyAll();
    }

    /**
     * Diese Methode wird aufgerufen, wenn der Spieler auf der GUI einen Button
     * drueckt und somit einen Angriff ausfuehrt. Sollte der Spieler nicht am
     * Zug sein, wird eine Interaktion verhindert, indem das Flag itsMyTurn geprueft
     * wird. Die Nachricht wird in dieser Methode gemaess Konvention kodiert und uebermittelt.
     * Anschliessend wird auf die Reaktion des Gegners gewartet und entsprechend verwertet.
     *
     * @param x Angriffskoordinaten
     * @param y
     * @throws InterruptedException
     */
    public synchronized void attack(int x, int y) throws InterruptedException {
        // Verhindere das Schlafenlegen, wenn das Spiel beendet ist, damit die GUI
        // nicht einfriert.
        while (!m_itsMyTurn && m_gameState == GameState.RUNNING) {
            System.out.println("CPlayingFieldController::attack - waiting for my turn");
            wait();
        }
        if (m_gameState != GameState.RUNNING) {
            notifyAll();
            return;
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

    /**
     * Uebertraegt die Nachricht an den Server
     * 
     * @param msg   Zu uebertragende Nachricht
     * @throws IOException
     */
    private synchronized void send(String msg) throws IOException {
        if (m_outStream != null) {
            System.out.print("CPlayingFieldController::send: ");
            System.out.println(msg);
            m_outStream.write(msg + "\r");
            m_outStream.flush();
        }
    }

    /**
     * Get Methode zum Abrufen der Spielfeldbreite
     * @return
     */
    public int getWidth() {
        return m_width;
    }

    /**
     * Get Methode zum Abrufen der Spielfeldhoehe
     * @return
     */
    public int getHeight() {
        return m_height;
    }

    /**
     * Methode die beide Spielfelder in Listenform an eine Consumer zurueckgibt.
     * Das erste Feld der Liste ist immer das gegnerische Feld, das zweite immer
     * das eigene
     * @return Liste mit dem gegnerischen und dem eigenen Spielfeldern
     * @throws InterruptedException
     */
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
