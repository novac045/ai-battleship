/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package gui;

import gui.CPlayingFieldController.state;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 *
 * @author victorapostel
 */
public class CBattleshipGUI extends JFrame implements ActionListener {
    protected CPlayingFieldPanel m_enemy = null;
    protected CPlayingFieldPanel m_own = null;
    private GridLayout m_layout = new GridLayout(0,2);
    private JPanel m_panel = new JPanel();
    private CPlayingFieldController m_control = null;
    private Thread m_controllerListener = null;

    public CBattleshipGUI(CPlayingFieldController control) {
        m_control = control;
        int width = m_control.getWidth();
        int height = m_control.getHeight();
        m_enemy = new CPlayingFieldPanel(width, height, this);
        // Das eigene Panel soll nicht auf das Drücken eines
        // Buttons reagieren, deshalb ist der ActionListener
        // auf null gesetzt
        m_own = new CPlayingFieldPanel(width, height, null);
        m_panel.setLayout(m_layout);

        //m_panel.add(new JLabel("Computer"));
        //m_panel.add(new JLabel("Eigenes Feld"));
        m_panel.add(m_enemy.getPanel());
        m_panel.add(m_own.getPanel());

        m_controllerListener = new Thread() {
            @Override
            public void run() {
                try {
                    while (true) {
                        List<state[]> states = m_control.getUpdatedFields();
                        boolean isItMyTurn = m_control.isItMyTurn();
                        System.out.println("CBattleshipGUI::CBattleshipGUI::Thread - status update received");
                        if (isItMyTurn) {
                            setEnemyPlayingField(states.get(0));
                        } else{
                            m_enemy.disable(m_control.getEnemyStateVec());
                        }
                        setOwnPlayingField(states.get(1));
                    }
                } catch (InterruptedException ex) {
                    System.out.println("CBattleshipGUI::CBattleshipGUI::Thread - InterruptedException");
                    System.out.println(ex.toString());
                }
            }
        };

        getContentPane().add(m_panel);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        // Fenster automatisch justieren, sodass alle Elemente
        // sichtbar sind
        pack();
        m_controllerListener.start();
        setVisible(true);
    }

    public void setEnemyPlayingField(CPlayingFieldController.state[] stateVec) {
        m_enemy.setState(stateVec);
    }
    
    public void setOwnPlayingField(CPlayingFieldController.state[] stateVec) {
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
        }
    }

}
