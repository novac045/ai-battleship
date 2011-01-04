package gui;

import common.CMessageGenerator.FieldState;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;

/**
 * Diese Klasse ist Bestandteil der GUI und stellt eines der beiden vorgesehenen
 * Spielfelder dar. Das Spielfeld ist quadratisch aufgebaut und die einzelnen
 * Felder sind durch die Verwendung von Buttons interaktiv bedienbar. Des Weiteren
 * wird das Spielfeld mit Labeln in der ersten Zeile und Spalte versehen, die
 * Koordinaten widerspiegeln.
 *
 * @author Victor Apostel
 */
public class CPlayingFieldPanel {
    private int m_width;
    private int m_height;
    private JButton[] m_button = null;
    private JPanel m_panel = new JPanel();
    private GridLayout m_layout = null;

    /**
     * Der Konstruktor initialisiert ein Spielfeld, indem das Design auf "flach"
     * gestellt wird und die GUI Elemente im GridLayout platziert werden.
     * Gleichzeitig werden den Buttons ActionCommands zugewiesen, die dem Angriff
     * dienen.
     *
     * @param width
     * @param height
     * @param al
     */
    public CPlayingFieldPanel(int width, int height, ActionListener al) {
        m_width = width;
        m_height = height;
        m_button = new JButton[m_width * m_height];
        m_layout = new GridLayout(m_height + 1, m_width + 1);
        m_panel.setLayout(m_layout);

        // Element oben links bleibt leer
        m_panel.add(new JLabel("", null, JLabel.CENTER));
        // 1. Zeile mit 1..width nummerieren
        for (int i = 0; i < m_width; ++i) {
            m_panel.add(new JLabel(Integer.toString(i + 1), null, JLabel.CENTER));
        }

        // Design auf "flach" stellen
        Border line = new LineBorder(Color.BLACK);
        Border margin = new EmptyBorder(5, 15, 5, 15);
        Border compound = new CompoundBorder(line, margin);

        // 1. Buchstabe für die Spalten
        char row = 'A';
        // Hauptfeld initialisieren

        int buttonSize = 50;
        for (int i = 0; i < m_width * m_height; ++i) {
            m_button[i] = new JButton();
            m_button[i].setSize(buttonSize, buttonSize);
            m_button[i].setMinimumSize(new Dimension(buttonSize ,buttonSize));
            m_button[i].setMaximumSize(new Dimension(buttonSize ,buttonSize));
            m_button[i].setPreferredSize(new Dimension(buttonSize ,buttonSize));
            m_button[i].addActionListener(al);
            m_button[i].setBorder(compound);
            int y = i / m_width;
            int x = i - y * m_width;
            String command = "" + x + "," + y;
            m_button[i].setActionCommand(command);
           
            // Debug Info
            m_button[i].setText(command);

            // Der Anfang einer jeden neuen Zeile wird mit einem neuen
            // Buchstaben versehen
            if (i % m_width == 0) {
                // Buchstaben zeichnen
                m_panel.add(new JLabel(String.valueOf(row), null, JLabel.CENTER));
                // Naechsten Buchstaben auswaehlen
                row++;
            }
            m_panel.add(m_button[i]);
        }
    }

    /**
     * Setzt den Status und damit sein Design eines spezifischen Buttons
     *
     * @param x Buttonkoordinate
     * @param y
     * @param s Neuer Status
     * @throws CPlayingFieldControllerException
     */
    public void setState(int x, int y, FieldState s) throws CPlayingFieldControllerException {
        applyButtonDesign(x, y, s);
    }

    /**
     * Setzt den Status und damit das Design des gesamten Spielfelds
     *
     * @param stateArray    Array mit den neuen Buttonzustaenden
     * @throws CPlayingFieldControllerException
     */
    public void setState(FieldState[] stateArray) throws CPlayingFieldControllerException {
        if (stateArray.length == m_width * m_height) {
            int i = 0;
            for (FieldState s : stateArray) {
                applyButtonDesign(i, s);
                ++i;
            }
        }
    }

    /**
     * Setzt das Design eines spezifischen Buttons. Die Angabe des spezifischen
     * Buttons erfolgt durch die Angabe in der Arrayoisition.
     *
     * @param pos   Arrayposition
     * @param s     Neuer Zustand
     * @throws CPlayingFieldControllerException
     */
    private void applyButtonDesign(int pos, FieldState s) throws CPlayingFieldControllerException {
        int y = pos / m_width;
        int x = pos - y * m_width;
        applyButtonDesign(x, y, s);
    }

    /**
     * Setzt den Status bzw. das Design eines spezifischen Buttons, der durch
     * seine x/y Koordinaten angegeben wird.
     * Unbekannte Felder werden in hellem grau dargestellt.
     * Wasser -> blau
     * Schiff -> grau
     * Treffer -> rot
     * Wasser (eigenes Feld) -> hellblau
     * Zerstoert -> schwarz
     *
     * @param x Koordinate des Buttons
     * @param y
     * @param s Neuer Buttonstatus
     * @throws CPlayingFieldControllerException
     */
    private void applyButtonDesign(int x, int y, FieldState s) throws CPlayingFieldControllerException {
        if (y >= m_height || x >= m_width || x < 0 || y < 0) {
            throw new CPlayingFieldControllerException("Coordinates ("+ x + "/" + y +") out of range");
        }
        int pos = m_width * y + x;
        if (pos < m_button.length) {
            String content = m_button[pos].getText();
            Color c = Color.LIGHT_GRAY;
            switch(s) {
                default:
                    throw new CPlayingFieldControllerException("Unknown state");
                case UNKNOWN:
                    m_button[pos].setEnabled(true);
                    break;
                case WATER:
                    c = Color.BLUE;
                    content = "~";
                    m_button[pos].setEnabled(false);
                    break;
                case SHIP:
                    c = Color.GRAY;
                    content = "S";
                    m_button[pos].setEnabled(false);
                    break;
                case MISSED:
                    c = Color.CYAN;
                    content = "~";
                    m_button[pos].setEnabled(false);
                    break;
                case HIT:
                    c = Color.RED;
                    content = "x";
                    m_button[pos].setEnabled(false);
                    break;
                case DESTROYED:
                    c = Color.BLACK;
                    content = "#";
                    m_button[pos].setEnabled(false);
                    break;
            }
            m_button[pos].setText(content);
            m_button[pos].setBackground(c);
        }
    }

    /**
     * Aktiviert einen spezifischen Button
     * @param x
     * @param y
     */
    public void enable(int x, int y) {
        if (y * m_width + x < m_button.length) {
            m_button[y * m_width + x].setEnabled(true);
        }
    }

    /**
     * Deaktiviert das gesamte Spielfeld und aktualisiert die Zustaende
     * @param states Spielfeldstatus
     * @throws CPlayingFieldControllerException
     */
    public void disable(FieldState[] states) throws CPlayingFieldControllerException {
        setState(states);
        for (int i = 0; i < m_button.length; ++i) {
            m_button[i].setEnabled(false);
        }
    }

    /**
     * Gibt das Panel zwecks Anzeige zurueck
     * @return
     */
    public JPanel getPanel() {
        return m_panel;
    }
}
