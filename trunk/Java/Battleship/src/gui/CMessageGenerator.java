package gui;

import gui.CPlayingFieldController.CommandState;
import gui.CPlayingFieldController.FieldState;

/**
 *
 * @author Victor Apostel
 */
public class CMessageGenerator {
    private static CMessageGenerator m_me = null;

    public static CMessageGenerator getInstance() {
        if (m_me == null) {
            m_me = new CMessageGenerator();
        }
        return m_me;
    }

    private CMessageGenerator() {
    }

    public String attack(int x, int y) {
        return "(" + parseCommand(CommandState.ATTACK) + ",[" + x + "," + y + "]).";
    }

    public String respondAttack(int x, int y, FieldState s) throws CPlayingFieldControllerException {
        int stateCode = -1;
        // Nur WATER, HIT und DESTORYED sind als Antwort erlaubt
        if (s == FieldState.WATER || s == FieldState.HIT || s == FieldState.DESTROYED) {
            stateCode = parseState(s);
        } else {
            throw new CPlayingFieldControllerException("Illegal output state: " + s);
        }
        
        return "(" + parseCommand(CommandState.ATTACKRESPONSE) + ",[" + x + "," + y + "," + stateCode + "]).";
    }

    public String defendFirst() {
        return "(" + parseCommand(CommandState.DEFENDFIRST) + ",[]).";
    }

    public String attackFirst() {
        return "(" + parseCommand(CommandState.ATTACKFIRST) + ",[]).";
    }

    public String generateStartSignal() {
        return "(" + parseCommand(CommandState.STARTGAME) + ",[]).";
    }

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
