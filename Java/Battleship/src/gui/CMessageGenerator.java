/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gui;

import gui.CPlayingFieldController.state;

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
        return "(1,[" + x + "," + y + "]).";
    }

    public String respondAttack(int x, int y, state s) {
        String stateCode = "";
        switch (s) {
            default: stateCode = "0";
                break;
            case UNKNOWN: stateCode = "0";
                break;
            case WATER: stateCode = "1";
                break;
            case HIT: stateCode = "2";
                break;
            case DESTROYED: stateCode = "3";
                break;
        }
        return "(2,[" + x + "," + y + "," + stateCode + "]).";
    }

    public String defendFirst() {
        return "(3,[]).";
    }

    public String attackFirst() {
        return "(4,[]).";
    }
}
