package gui;

import gui.CPlayingFieldController.FieldState;
import java.util.Random;

/**
 *
 * @author Victor Apostel
 */
public class CShipPlacementController {
    private enum BoardState {FREE, BUSY, SHIP};
    private enum Direction {NORTH, SOUTH, WEST, EAST};
    Random m_RNG = new Random();
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

    private int getRandomNumber(int low, int high) {
        int randomNumber = m_RNG.nextInt(high - low);
        randomNumber = randomNumber + low;
        return randomNumber;
    }

    private Direction getRandomDirection() throws CPlayingFieldControllerException {
        int dirCode = getRandomNumber(0, 4);
        Direction dir = Direction.NORTH;
        switch (dirCode) {
            case 0:
                dir = Direction.NORTH;
                break;
            case 1:
                dir = Direction.SOUTH;
                break;
            case 2:
                dir = Direction.EAST;
                break;
            case 3:
                dir = Direction.WEST;
                break;
            default:
                throw new CPlayingFieldControllerException("Unknown direction code: " + dirCode);
        }
        return dir;
    }

    private void markBusy(int x, int y) {
        if (x < 0 || y < 0 || x >= m_width || y >= m_height) {
            return;
        }
        int pos = y * m_width + x;
        if (m_board[pos] != BoardState.SHIP) {
            m_board[pos] = BoardState.BUSY;
        }
    }

    private void surroundWithBusy() {
        for (int y = 0; y < m_height; ++y) {
            for (int x = 0; x < m_width; ++x) {
                int pos = y * m_width + x;
                if (m_board[pos] == BoardState.SHIP) {
                    markBusy(x-1, y);
                    markBusy(x+1, y);
                    markBusy(x, y-1);
                    markBusy(x, y+1);
                    markBusy(x-1, y-1);
                    markBusy(x-1, y+1);
                    markBusy(x+1, y-1);
                    markBusy(x+1, y+1);
                }
            }
        }
    }

    private boolean tryToPlace (int x, int y, int size, Direction dir) {
        boolean success = true;
        if (x < 0 || y < 0 || x >= m_width || y >= m_height) {
            return false;
        }
        int pos = y * m_width + x;
        if (m_board[pos] != BoardState.FREE) {
            return false;
        }

        if (size - 1 > 0) {
            int nextX = x;
            int nextY = y;
            switch(dir) {
                case NORTH:
                    nextY = y - 1;
                    break;
                case SOUTH:
                    nextY = y + 1;
                    break;
                case EAST:
                    nextX = x + 1;
                    break;
                case WEST:
                    nextX = x - 1;
                    break;
                default:
                    return false;
            }
            success = tryToPlace(nextX, nextY, size - 1, dir);
        }

        if (success) {
            m_board[pos] = BoardState.SHIP;
        }
        
        return success;
    }

    private void placeShips(int numShips, int shipSize) throws CPlayingFieldControllerException {
        int trysLeft = 100;
        for (int i = 0; i < numShips;) {
            int xSuggestion = getRandomNumber(0, m_width);
            int ySuggestion = getRandomNumber(0, m_height);
            Direction dir = getRandomDirection();
            //System.out.print("X: " + xSuggestion + " Y: " + ySuggestion + " Dir: " + dir + " Size: " + shipSize);
            boolean success = tryToPlace(xSuggestion, ySuggestion, shipSize, dir);
            if (success) {
                surroundWithBusy();
                i = i + 1;
                trysLeft = 100;
            } else {
                if (trysLeft == 0) {
                    throw new CPlayingFieldControllerException("Out of trys");
                }
                trysLeft = trysLeft - 1;
            }
            //System.out.println(" Success: " + success);
        }
    }

    private void initShips() {
        int trysLeft = 15;
        while (trysLeft >= 0) {
            try {
                placeShips(m_numBattleship, m_BattleshipSize);
                placeShips(m_numCruiser, m_CruiserSize);
                placeShips(m_numDestroyer, m_DestroyerSize);
                placeShips(m_numSubmarines, m_SubmarineSize);
                System.out.println("Board initialized");
                break;
            } catch (CPlayingFieldControllerException ex) {
                trysLeft = trysLeft - 1;
                resetBoard();
                System.out.println("CShipPlacementController::initShips - CPlayingFieldControllerException");
                System.out.println(ex.toString());
            }
        }
        if (trysLeft < 0) {
            System.out.println("Board not initialized");
        }
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
