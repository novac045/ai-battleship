package gui;

/**
 * Eigene Exceptionklasse zum Signalisieren von Fehlern im Spielverlauf
 *
 * @author Victor Apostel
 */
public class CPlayingFieldControllerException extends Exception {
    public CPlayingFieldControllerException(String s) {
        super(s);
    }

}
