/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gui;

import gui.CPlayingFieldController.FieldState;
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
 *
 * @author victorapostel
 */
public class CBattleshipGUI extends JFrame implements ActionListener {
    protected CPlayingFieldPanel m_enemy = null;
    protected CPlayingFieldPanel m_own = null;
    private GridLayout m_playingFieldLayout = new GridLayout(0,2);
    private BorderLayout m_windowLayout = new BorderLayout();
    private JPanel m_panel = new JPanel();
    private CPlayingFieldController m_control = null;
    private Thread m_controllerListener = null;
    private JLabel m_statusMsg = new JLabel("DUMMYLABEL");

    public CBattleshipGUI(CPlayingFieldController control) {
        setLayout(m_windowLayout);
        m_control = control;
        int width = m_control.getWidth();
        int height = m_control.getHeight();
        m_enemy = new CPlayingFieldPanel(width, height, this);
        // Das eigene Panel soll nicht auf das Drücken eines
        // Buttons reagieren, deshalb ist der ActionListener
        // auf null gesetzt
        m_own = new CPlayingFieldPanel(width, height, null);
        m_panel.setLayout(m_playingFieldLayout);

        //m_panel.add(new JLabel("Computer"));
        //m_panel.add(new JLabel("Eigenes Feld"));
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

        getContentPane().add(m_panel, BorderLayout.CENTER);
        getContentPane().add(m_statusMsg, BorderLayout.SOUTH);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        // Fenster automatisch justieren, sodass alle Elemente
        // sichtbar sind
        pack();
        m_controllerListener.start();
        setVisible(true);
    }

    public void setEnemyPlayingField(CPlayingFieldController.FieldState[] stateVec) throws CPlayingFieldControllerException {
        m_enemy.setState(stateVec);
    }
    
    public void setOwnPlayingField(CPlayingFieldController.FieldState[] stateVec) throws CPlayingFieldControllerException {
        m_own.setState(stateVec);
    }

    public void actionPerformed(ActionEvent ae) {
        try {
            String command = ae.getActionCommand();
            String[] xy = command.split(",");
            final int x = Integer.parseInt(xy[0]);
            final int y = Integer.parseInt(xy[1]);
            m_enemy.disable(m_control.getEnemyStateVec());
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
