package gui;

import gui.CPlayingFieldController.FieldState;

/**
 *
 * @author Victor Apostel
 */
public class CShipPlacementController {
    private enum BoardState {FREE, BUSY, SHIP};
    private int m_width;
    private int m_height;
    private FieldState m_boardOutput[] = null;
    private BoardState m_board[] = null;
    private int m_numBattleship = 1;
    private int m_BattleshipSize = 5;
    private int m_numCruiser = 2;
    private int m_CruiserSize = 4;
    private int m_numDestroyer = 3;
    private int m_DestroyerSize = 3;
    private int m_numSubmarines = 4;
    private int m_SubmarineSize = 2;

    public CShipPlacementController(int width, int height) {
        m_width = width;
        m_height = height;
        resetBoard();
        initShips();
    }

    private void resetBoard() {
        m_boardOutput = new FieldState[m_width * m_height];
        m_board = new BoardState[m_width * m_height];

        for (int i = 0; i < m_width * m_height; ++i) {
            m_boardOutput[i] = FieldState.WATER;
            m_board[i] = BoardState.FREE;
        }

    }

    private boolean hitsOtherShip (int x, int y, int size, boolean horizontal) {
        return false;
    }

    private void placeBattleships() {
        
    }

    private void initShips() {

    }

    public FieldState[] getNewBoard() {
        for (int i = 0; i < m_width * m_height; ++i) {
            if (m_board[i] == BoardState.FREE || m_board[i] == BoardState.BUSY) {
                m_boardOutput[i] = FieldState.WATER;
            } else {
                m_boardOutput[i] = FieldState.SHIP;
            }
        }
        return m_boardOutput;
    }
}
