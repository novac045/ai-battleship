package gui;

import gui.CPlayingFieldController.FieldState;
import java.util.Random;

/**
 * Diese Hilfsklasse dient der Platzierung von Schiffen auf dem Spielfeld.
 * Es gelten dabei die folgenden Regeln:
 * - Schiffe duerfen sich nicht beruehren. D.h. es muss im Umfeld der 8er
 * Nachbarschaft ein Feld frei bleiben.
 * - Die Platzierung erfolgt horizontal oder vertikal. Diagonal ist nicht erlaubt
 * - Eine Platzierung am Rand ist zulaessig.
 * - Die Platzierung erfolgt zufaellig
 *
 * @author Victor Apostel
 */
public class CShipPlacementController {
    // Die interne Repraesentation des Spielfelds erfolgt durch die
    // Enumeration BoardState.
    // Free == Ein Schiff kann hier platziert werden
    // SHIP == Ein Schiff belegt dieses Feld
    // BUSY == In Unmittelbarer Naehe befindet sich ein anderes Schiff. Hier
    //          darf kein weiteres Schiff platziert werden.
    private enum BoardState {FREE, BUSY, SHIP};
    private enum Direction {NORTH, SOUTH, WEST, EAST};
    Random m_RNG = new Random();
    private int m_width;
    private int m_height;
    private BoardState m_board[] = null;
    // Anzahl und Groesse der zu platzierenden Schiffe
    private int m_numBattleship = 1;
    private int m_BattleshipSize = 5;
    private int m_numCruiser = 1;
    private int m_CruiserSize = 4;
    private int m_numDestroyer = 2;
    private int m_DestroyerSize = 3;
    private int m_numSubmarines = 1;
    private int m_SubmarineSize = 2;

    /**
     * Der Konstruktor initialisiert anhand der uebergebenen Spielfelddimensionen
     * das Spielfeld und platziert die Schiffe.
     *
     * @param width
     * @param height
     */
    public CShipPlacementController(int width, int height) {
        m_width = width;
        m_height = height;
        resetBoard();
        initShips();
    }

    /**
     * Setzt das interne Spielfeld auf den Ursprungszustand (Alles auf FREE)
     * zurueck
     */
    private void resetBoard() {
        m_board = new BoardState[m_width * m_height];

        for (int i = 0; i < m_width * m_height; ++i) {
            m_board[i] = BoardState.FREE;
        }

    }

    /**
     * Hilfsmethode die in den Grenzen von low bis exklusiv high eine ganzzahlige
     * Zufallszahl erzeugt.
     * 
     * @param low   Untergrenze
     * @param high  exklusive Obergrenze
     * @return  Zufallszahl
     */
    private int getRandomNumber(int low, int high) {
        int randomNumber = m_RNG.nextInt(high - low);
        randomNumber = randomNumber + low;
        return randomNumber;
    }

    /**
     * Erzeugt eine zufaellige Richtung
     * @return  Zufallsrichtung
     * @throws CPlayingFieldControllerException
     */
    private Direction getRandomDirection() throws CPlayingFieldControllerException {
        int dirCode = getRandomNumber(0, 4);
        Direction dir = Direction.NORTH;
        switch (dirCode) {
            case 0:
                dir = Direction.NORTH;
                break;
            case 1:
                dir = Direction.SOUTH;
                break;
            case 2:
                dir = Direction.EAST;
                break;
            case 3:
                dir = Direction.WEST;
                break;
            default:
                throw new CPlayingFieldControllerException("Unknown direction code: " + dirCode);
        }
        return dir;
    }

    /**
     * Ein Feld wird von Free oder Busy auf Busy gesetzt.
     * Sollte es den Zustand SHIP aufweisen, wird nichts unternommen
     * @param x Koordinaten, die auf Busy gesetzt werden sollen
     * @param y
     */
    private void markBusy(int x, int y) {
        if (x < 0 || y < 0 || x >= m_width || y >= m_height) {
            return;
        }
        int pos = y * m_width + x;
        if (m_board[pos] != BoardState.SHIP) {
            m_board[pos] = BoardState.BUSY;
        }
    }

    /**
     * Nachdem ein Schiff platziert wurde, muss seine Nachbarschaft auf
     * BUSY gesetzt werden. Diese Methode geht das gesamte Spielfeld durch
     * und sobald ein Schiff gefunden wurde, wird in seiner 8er Nachbarschaft
     * alles auf BUSY gesetzt
     */
    private void surroundWithBusy() {
        for (int y = 0; y < m_height; ++y) {
            for (int x = 0; x < m_width; ++x) {
                int pos = y * m_width + x;
                if (m_board[pos] == BoardState.SHIP) {
                    markBusy(x-1, y);
                    markBusy(x+1, y);
                    markBusy(x, y-1);
                    markBusy(x, y+1);
                    markBusy(x-1, y-1);
                    markBusy(x-1, y+1);
                    markBusy(x+1, y-1);
                    markBusy(x+1, y+1);
                }
            }
        }
    }

    /**
     * Diese Methode versucht an den uebergebenen Koordinaten in angegebener
     * Richtung ein Schiff zu platzieren. Die Pruefung erfolgt dabei rekursiv,
     * wobei nach jedem Aufruf die Schiffsgroesse um 1 reduziert wird.
     * Ein Schiff ist dann platzierbar, wenn innerhalb seiner Richtung sich kein
     * Feld mit dem Status SHIP oder BUSY befindet.
     * Sollte diese Bedingung erfuellt sein, werden die Felder mit SHIP belegt.
     * 
     * @param x Ausgangskoordinate fuer das Schiff
     * @param y
     * @param size  Schiffsgroesse
     * @param dir   Richtung, in der das Schiff platziertw erden soll
     * @return  Gibt an, ob ein Schiff platziert werden konnte.
     */
    private boolean tryToPlace (int x, int y, int size, Direction dir) {
        boolean success = true;
        // Liegen die Koordinaten noch im Spielfeld?
        if (x < 0 || y < 0 || x >= m_width || y >= m_height) {
            return false;
        }
        int pos = y * m_width + x;
        // Ist die Koordinate noch frei?
        if (m_board[pos] != BoardState.FREE) {
            return false;
        }

        // Rufe das naechste benachbarte Feld in angegebener Richtung auf
        if (size - 1 > 0) {
            int nextX = x;
            int nextY = y;
            switch(dir) {
                case NORTH:
                    nextY = y - 1;
                    break;
                case SOUTH:
                    nextY = y + 1;
                    break;
                case EAST:
                    nextX = x + 1;
                    break;
                case WEST:
                    nextX = x - 1;
                    break;
                default:
                    return false;
            }
            success = tryToPlace(nextX, nextY, size - 1, dir);
        }

        // Wenn alles frei ist, belege das Feld mit einem Schiff
        if (success) {
            m_board[pos] = BoardState.SHIP;
        }
        
        return success;
    }

    /**
     * Diese Methode versucht ein Schiff an einer zufaelligen Position in
     * zufaelliger Richtung zu platzieren. Es werden dafuer 100 Versuche pro
     * Schiffstyp und Schiff vorgesehen.
     * Sollten diese nicht reichen, wird die Methode mit einer Exception beendet.
     * Nach jedem erfolgreich platzierten Schiff, wird seine Umgebung mit BUSY
     * umrandet.
     *
     * @param numShips  Anzahl der zu platzierenden Schiffe
     * @param shipSize  Groesse des zu platzierenden Schiffstyps
     * @throws CPlayingFieldControllerException
     */
    private void placeShips(int numShips, int shipSize) throws CPlayingFieldControllerException {
        int trysLeft = 100;
        for (int i = 0; i < numShips;) {
            // Zufallskoordinaten generieren
            int xSuggestion = getRandomNumber(0, m_width);
            int ySuggestion = getRandomNumber(0, m_height);
            Direction dir = getRandomDirection();
            //System.out.print("X: " + xSuggestion + " Y: " + ySuggestion + " Dir: " + dir + " Size: " + shipSize);
            // Versuche das Schiff zu platzieren
            boolean success = tryToPlace(xSuggestion, ySuggestion, shipSize, dir);
            // Schiff wurde erfolgreich platziert
            if (success) {
                surroundWithBusy();
                i = i + 1;
                trysLeft = 100;
            // Versuch fehlgeschlagen
            } else {
                if (trysLeft == 0) {
                    throw new CPlayingFieldControllerException("Out of trys");
                }
                trysLeft = trysLeft - 1;
            }
            //System.out.println(" Success: " + success);
        }
    }

    /**
     * Es wird in maximal 15 Versuchen probiert, alle Schiffe aller Typen auf dem
     * Spielfeld zu platzieren. Sollten diese Versuche nicht ausreichen, wird eine
     * Exception geworfen, die signalisiert, dass das Spielfeld nicht erfolgtreich
     * initialisiert werden konnte.
     */
    private void initShips() {
        int trysLeft = 15;
        while (trysLeft >= 0) {
            try {
                placeShips(m_numBattleship, m_BattleshipSize);
                placeShips(m_numCruiser, m_CruiserSize);
                placeShips(m_numDestroyer, m_DestroyerSize);
                placeShips(m_numSubmarines, m_SubmarineSize);
                System.out.println("Board initialized");
                break;
            } catch (CPlayingFieldControllerException ex) {
                trysLeft = trysLeft - 1;
                resetBoard();
                System.out.println("CShipPlacementController::initShips - CPlayingFieldControllerException");
                System.out.println(ex.toString());
            }
        }
        if (trysLeft < 0) {
            System.out.println("Board not initialized");
        }
    }

    /**
     * Diese Methode wandelt die interne Repraesentation des Spielfelds in die vom
     * CPlayingFieldController um und gibt diese zurueck.
     * @return  Umgewandeltes Spielfeld.
     */
    public FieldState[] getNewBoard() {
        FieldState[] boardOutput = new FieldState[m_board.length];
        for (int i = 0; i < m_width * m_height; ++i) {
            if (m_board[i] == BoardState.FREE || m_board[i] == BoardState.BUSY) {
                boardOutput[i] = FieldState.WATER;
            } else {
                boardOutput[i] = FieldState.SHIP;
            }
        }
        return boardOutput;
    }
}
