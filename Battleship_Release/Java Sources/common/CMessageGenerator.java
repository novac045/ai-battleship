package common;

import gui.CPlayingFieldControllerException;

/**
 * Diese Klasse bildet die Konvention zur Kommunikation zwischen Teilnehmern ab.
 * Der Aufbau erfolgt stets nach dem gleichen Muster:
 * MSG := (OPCODE,[PARAMS]).
 * OPCODE := Ziffer
 * PARAMS := leer oder Ziffer oder Ziffer,Ziffer oder Ziffer,Ziffer,Ziffer
 * 
 * Da diese Klasse lediglich eine Hilfe zur Erzeugung von Nachrichten darstellen
 * soll, wurde sie gemaess dem Singleton Design Pattern entworfen.
 *
 * @author Victor Apostel
 */
public class CMessageGenerator {
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
    private static CMessageGenerator m_me = null;

    /**
     * Methode zum Zugriff auf die Singletonklasse
     * @return Objekt vom Typ CMessageGenerator
     */
    public static CMessageGenerator getInstance() {
        if (m_me == null) {
            m_me = new CMessageGenerator();
        }
        return m_me;
    }

    /**
     * Leerer Standardkonstruktor
     */
    private CMessageGenerator() {
    }

    /**
     * Erzeuge eine Nachricht zum Angriff auf ein Feld
     * @param x Feldkoordinaten
     * @param y
     * @return  Kodierte Nachricht (1,[X,Y]).
     */
    public String attack(int x, int y) {
        return "(" + parseCommand(CommandState.ATTACK) + ",[" + x + "," + y + "]).";
    }

    /**
     * Reagiere auf den Angriff auf ein Feld
     * @param x Koordinaten des Felds
     * @param y
     * @param s Feldstatus
     * @return Kodierte Nachricht (2,[X,Y,Status]).
     * @throws CPlayingFieldControllerException
     */
    public String respondAttack(int x, int y, FieldState s) throws CPlayingFieldControllerException {
        int stateCode = -1;
        // Nur WATER, HIT und DESTORYED sind als Antwort erlaubt
        if (s == FieldState.WATER || s == FieldState.HIT || s == FieldState.DESTROYED || s == FieldState.LASTSHIPDESTROYED) {
            stateCode = parseState(s);
        } else {
            throw new CPlayingFieldControllerException("Illegal output state: " + s);
        }
        
        return "(" + parseCommand(CommandState.ATTACKRESPONSE) + ",[" + x + "," + y + "," + stateCode + "]).";
    }

    /**
     * Nachricht, dass der Spieler zuerst in den DEFENCE Status gehen soll
     * @return Kodierte Nachricht (3,[]).
     */
    public String defendFirst() {
        return "(" + parseCommand(CommandState.DEFENDFIRST) + ",[]).";
    }

    /**
     * Nachricht, dass der Spieler zuerst in den ATTACK Status gehen soll
     * @return Kodierte Nachricht (4,[]).
     */
    public String attackFirst() {
        return "(" + parseCommand(CommandState.ATTACKFIRST) + ",[]).";
    }

    /**
     * Startsignal, zur Synchronisation beider Clients
     * @return Kodierte Nachricht (5,[]).
     */
    public String generateStartSignal() {
        return "(" + parseCommand(CommandState.STARTGAME) + ",[]).";
    }

    /**
     * Hilfsmethode, die einen int code in einen Feldstatus umwandelt
     * @param code
     * @return Feldstatus
     */
    public FieldState parseState(int code) {
        FieldState out = FieldState.UNKNOWN;
        switch (code) {
            default:
                out = FieldState.ERROR;
                break;
            case 0:
                out = FieldState.UNKNOWN;
                break;
            case 1:
                out = FieldState.WATER;
                break;
            case 2:
                out = FieldState.HIT;
                break;
            case 3:
                out = FieldState.DESTROYED;
                break;
            case 4:
                out = FieldState.LASTSHIPDESTROYED;
                break;
            case 5:
                out = FieldState.MISSED;
                break;
            case 6:
                out = FieldState.SHIP;
                break;
        }
        return out;
    }

    /**
     * Hilfsmethode, die einen Feldstatus in einen int code umwandeln soll
     * @param s
     * @return int code
     */
    public int parseState(FieldState s) {
        int out = -1;
        switch (s) {
            default:
                out = -1;
                break;
            case UNKNOWN:
                out = 0;
                break;
            case WATER:
                out = 1;
                break;
            case HIT:
                out = 2;
                break;
            case DESTROYED:
                out = 3;
                break;
            case LASTSHIPDESTROYED:
                out = 4;
                break;
            case MISSED:
                out = 5;
                break;
            case SHIP:
                out = 6;
                break;
        }
        return out;
    }

    /**
     * Hilfsmethode, die den OPCODE in eine Kommandoenumeration umwandelt
     * @param code
     * @return Kommando
     */
    public CommandState parseCommand(int code) {
        CommandState out = CommandState.UNKNOWN;
        switch (code) {
            default:
                out = CommandState.UNKNOWN;
                break;
            case 1:
                out = CommandState.ATTACK;
                break;
            case 2:
                out = CommandState.ATTACKRESPONSE;
                break;
            case 3:
                out = CommandState.DEFENDFIRST;
                break;
            case 4:
                out = CommandState.ATTACKFIRST;
                break;
            case 5:
                out = CommandState.STARTGAME;
                break;
        }
        return out;
    }

    /**
     * Hilfsmethode, die eine Kommandoenumeration in einen OPCODE umwandelt
     * @param s
     * @return OPCODE
     */
    public int parseCommand(CommandState s) {
        int out = 0;
        switch (s) {
            default:
                out = 0;
                break;
            case ATTACK:
                out = 1;
                break;
            case ATTACKRESPONSE:
                out = 2;
                break;
            case DEFENDFIRST:
                out = 3;
                break;
            case ATTACKFIRST:
                out = 4;
                break;
            case STARTGAME:
                out = 5;
                break;
        }
        return out;
    }
}
