package gui;

import common.CMessageGenerator.FieldState;
import gui.CPlayingFieldController.GameState;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * Die Klasse CBattleshipGUI ist die Hauptanzeige und gleichzeitig die
 * Interaktionsflaeche des Benutzers. Durch die Betaetigung einer Schaltflaeche
 * wird der ActionListener aktiviert und eine entsprechende Nachricht an
 * den Controller vom Typ CPlayingFieldController uebermittelt, der die eigentliche
 * Spiellogik ausuebt. Des Weiteren laeft ein separater Thread, der Aenderungen
 * der Controllerzustaende ueberwacht und visualisiert.
 *
 * Die GUI ist zum einen aus einem Hauptpanel aufgebaut, das die Ueberschriften und
 * den Aktionsstatus anzeigt. Des Weiteren beinhaltet diese Klasse auch zwei Komponenten
 * vom Typ CPlayingFieldPanel, die das eigentliche Spielfeld abbilden.
 *
 * @author Victor Apostel
 */
public class CBattleshipGUI extends JFrame implements ActionListener {
    // Die beiden Spielfelder
    protected CPlayingFieldPanel m_enemy = null;
    protected CPlayingFieldPanel m_own = null;
    // Layoutmanager fuer die Spielfelder.
    // Diese sollen nebeneinander dargestellt werden
    private GridLayout m_playingFieldLayout = new GridLayout(0,2);
    // Das Hauptlayout beinhaltet in der Mitte die Spielfelder.
    // Oben die Ueberschriften und unten den Spielstatus
    private BorderLayout m_windowLayout = new BorderLayout();
    // Panel zur Darstellung aller Inhalte
    private JPanel m_panel = new JPanel();
    // Verweis zum Controller mit der Spiellogik
    private CPlayingFieldController m_control = null;
    // Thread zur Ueberwachung des Controllers
    private Thread m_controllerListener = null;
    // Label, das den aktuellen Spielstatus anzeigt
    private JLabel m_statusMsg = new JLabel("");

    /**
     * Konstruktor zum Aufbau der GUI. Hier werden die GUI Elemente
     * platziert und der Thread zur Ueberwachung des Controllers gestartet.
     * @param control Verweis zum Controllerobjekt
     */
    public CBattleshipGUI(CPlayingFieldController control) {
        setLayout(m_windowLayout);
        m_control = control;
        int width = m_control.getWidth();
        int height = m_control.getHeight();
        m_enemy = new CPlayingFieldPanel(width, height, this);
        // Das eigene Panel soll nicht auf das Druecken eines
        // Buttons reagieren, deshalb ist der ActionListener
        // auf null gesetzt
        m_own = new CPlayingFieldPanel(width, height, null);
        m_panel.setLayout(m_playingFieldLayout);

        m_panel.add(m_enemy.getPanel());
        m_panel.add(m_own.getPanel());

        m_controllerListener = new Thread() {
            @Override
            public void run() {
                try {
                    while (true) {
                        List<FieldState[]> states = m_control.getUpdatedFields();
                        boolean isItMyTurn = m_control.isItMyTurn();
                        GameState gameState = m_control.getGameState();
                        System.out.println("CBattleshipGUI::CBattleshipGUI::Thread - status update received");
                        setEnemyPlayingField(states.get(0));
                        if (!isItMyTurn) {
                            m_statusMsg.setText("Gegner ist am Zug");
                            m_enemy.disable(m_control.getEnemyStateVec());
                        } else {
                            m_statusMsg.setText("Sie sind am Zug");
                        }
                        setOwnPlayingField(states.get(1));
                        // Wenn das Spiel beendet ist, wird der Kommunikationsthread mit dem Controller beendet
                        if (gameState == GameState.WON) {
                            m_statusMsg.setText("Sie haben das Spiel gewonnen!");
                            this.interrupt();
                        } else if (gameState == GameState.LOST) {
                            m_statusMsg.setText("Sie haben das Spiel leider verloren!");
                            this.interrupt();
                        } else if (gameState == GameState.INITIALIZATION) {
                            m_statusMsg.setText("Auf das Startsignal warten");
                        }
                    }
                } catch (InterruptedException ex) {
                    System.out.println("CBattleshipGUI::CBattleshipGUI::Thread - InterruptedException");
                    System.out.println(ex.toString());
                } catch (CPlayingFieldControllerException ex) {
                    System.out.println("CBattleshipGUI::CBattleshipGUI::Thread - CPlayingFieldControllerException");
                    System.out.println(ex.toString());
                }
            }
        };

        // Spielfelder zentriert platzieren
        getContentPane().add(m_panel, BorderLayout.CENTER);
        // Statusfeld am unteren Rand platzieren
        getContentPane().add(m_statusMsg, BorderLayout.SOUTH);
        // Ueberschrifen der Spielfelder generieren
        GridLayout captions = new GridLayout(0,2);
        JPanel captionPanel = new JPanel();
        captionPanel.setLayout(captions);
        captionPanel.add(new JLabel("Gegnerisches Feld", null, JLabel.CENTER));
        captionPanel.add(new JLabel("Eigenes Feld", null, JLabel.CENTER));
        getContentPane().add(captionPanel, BorderLayout.NORTH);

        // Programm beendet sich beim Schliessen der GUI
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        // Fenster automatisch justieren, sodass alle Elemente
        // sichtbar sind
        pack();
        m_controllerListener.start();
        setVisible(true);
    }

    /**
     * Das gesamte Spielfeld des Gegners wird in der Darstellung anhand des FieldState
     * Arrays angepasst. Wenn die Arraylaenge mit der Spielfeldgroesse nicht korreliert,
     * wird eine CPlayingFieldControllerException Exception geworfen.
     * @param stateVec  Array mit dem neuen Status des gegnerischen Spielfelds
     * @throws CPlayingFieldControllerException
     */
    public void setEnemyPlayingField(FieldState[] stateVec) throws CPlayingFieldControllerException {
        m_enemy.setState(stateVec);
    }

    /**
     * Gleiches Verhalten, wie setEnemyPlayingField, jedoch fuer das eigene Spielfeld
     * @param stateVec  Array mit dem neuen Status des eigenen Spielfelds
     * @throws CPlayingFieldControllerException
     */
    public void setOwnPlayingField(FieldState[] stateVec) throws CPlayingFieldControllerException {
        m_own.setState(stateVec);
    }

    /**
     * Implementierter ActionListener, der die hinterlegten Kommandos hinter den
     * Spielfeldbuttons entgegennimmt, analysiert und an den Controller uebermittelt.
     * @param ae    ActionEvent des Buttons
     */
    public void actionPerformed(ActionEvent ae) {
        try {
            // Kommando extrahieren und in int umwandeln
            String command = ae.getActionCommand();
            String[] xy = command.split(",");
            final int x = Integer.parseInt(xy[0]);
            final int y = Integer.parseInt(xy[1]);
            // Spielfeld aktualisieren und deaktivieren, da der Spielstatus
            // von attack auf defend wechselt.
            m_enemy.disable(m_control.getEnemyStateVec());
            // Angriffskommando an den Controller uebermitteln
            m_control.attack(x, y);
        } catch (InterruptedException ex) {
            System.out.println("CBattleshipGUI::actionPerformed - InterruptedException");
            System.out.println(ex.toString());
        } catch (CPlayingFieldControllerException ex) {
            System.out.println("CBattleshipGUI::actionPerformed - CPlayingFieldControllerException");
            System.out.println(ex.toString());
        }
    }

}
