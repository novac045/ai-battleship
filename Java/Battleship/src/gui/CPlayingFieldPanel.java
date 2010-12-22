/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gui;

import java.awt.Color;
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
 *
 * @author victorapostel
 */
public class CPlayingFieldPanel {
    private int m_width;
    private int m_height;
    private JButton[] m_button = null;
    private JPanel m_panel = new JPanel();
    private GridLayout m_layout = null;

    public CPlayingFieldPanel(int width, int height, ActionListener al) {
        m_width = width;
        m_height = height;
        m_button = new JButton[m_width * m_height];
        m_layout = new GridLayout(m_height + 1, m_width + 1);
        m_panel.setLayout(m_layout);

        // Element oben links bleibt leer
        m_panel.add(new JLabel(""));
        // 1. Zeile mit 1..width nummerieren
        for (int i = 0; i < m_width; ++i) {
            m_panel.add(new JLabel(Integer.toString(i + 1)));
        }

        Border line = new LineBorder(Color.BLACK);
        Border margin = new EmptyBorder(5, 15, 5, 15);
        Border compound = new CompoundBorder(line, margin);

        // 1. Buchstabe für die Spalten
        char row = 'A';
        // Hauptfeld initialisieren
        for (int i = 0; i < m_width * m_height; ++i) {
            m_button[i] = new JButton();
            m_button[i].setSize(10, 10);
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
                m_panel.add(new JLabel(String.valueOf(row)));
                // Naechsten Buchstaben auswaehlen
                row++;
            }
            m_panel.add(m_button[i]);
        }
    }

    public void setState(int x, int y, CPlayingFieldController.FieldState s) throws CPlayingFieldControllerException {
        applyButtonDesign(x, y, s);
    }

    public void setState(CPlayingFieldController.FieldState[] stateArray) throws CPlayingFieldControllerException {
        if (stateArray.length == m_width * m_height) {
            int i = 0;
            for (CPlayingFieldController.FieldState s : stateArray) {
                applyButtonDesign(i, s);
                ++i;
            }
        }
    }

    protected void applyButtonDesign(int pos, CPlayingFieldController.FieldState s) throws CPlayingFieldControllerException {
        int y = pos / m_width;
        int x = pos - y * m_width;
        applyButtonDesign(x, y, s);
    }

    protected void applyButtonDesign(int x, int y, CPlayingFieldController.FieldState s) throws CPlayingFieldControllerException {
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

    public void enable(int x, int y) {
        if (y * m_width + x < m_button.length) {
            m_button[y * m_width + x].setEnabled(true);
        }
    }

    public void disable(CPlayingFieldController.FieldState[] states) throws CPlayingFieldControllerException {
        setState(states);
        for (int i = 0; i < m_button.length; ++i) {
            m_button[i].setEnabled(false);
        }
    }

    public JPanel getPanel() {
        return m_panel;
    }
}
