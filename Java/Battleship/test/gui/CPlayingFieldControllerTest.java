package gui;

import common.CMessageGenerator.FieldState;
import communication.CCommunicationServer;
import gui.CPlayingFieldController.GameState;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author victorapostel
 */
public class CPlayingFieldControllerTest {
    private CCommunicationServer m_server = new CCommunicationServer(54321);

    public CPlayingFieldControllerTest() {
    }

    @BeforeClass
    public static void setUpClass() throws Exception {
    }

    @AfterClass
    public static void tearDownClass() throws Exception {
    }

    @Before
    public void setUp() {
        m_server.start();
    }

    @After
    public void tearDown() {
        m_server.interrupt();
    }

    /**
     * Test of getGameState method, of class CPlayingFieldController.
     *
     * Achtung, der Test kann auf Grund des nicht vorhersehbaren Schedulings evtl. fehlschlagen!
     */
    @Test
    public void testGameStateAndTurn() throws InterruptedException {
        System.out.println("GameState and Turn");
        CPlayingFieldController instance = new CPlayingFieldController(10, 10, "127.0.0.1", 54321);
        GameState expResult = GameState.INITIALIZATION;
        GameState result = instance.getGameState();
        assertEquals(expResult, result);

        Thread t = new Thread(instance);
        t.start();
        expResult = GameState.INITIALIZATION;
        result = instance.getGameState();
        assertEquals(expResult, result);

        CPlayingFieldController instance2 = new CPlayingFieldController(10, 10, "127.0.0.1", 54321);
        Thread t2 = new Thread(instance2);
        t2.start();
        
        // Sicherstellen, dass das Defencesignal verarbeitet wird
        Thread.sleep(1000);
        boolean attackFirst = false;
        boolean turn = instance.isItMyTurn();
        assertEquals(attackFirst, turn);

        // Sicherstellen, dass das Attacksignal verarbeitet wird
        Thread.sleep(500);
        attackFirst = true;
        turn = instance2.isItMyTurn();
        assertEquals(attackFirst, turn);
        
        // Sicherstellen, dass das Startsignal verarbeitet wird
        Thread.sleep(500);
        expResult = GameState.RUNNING;
        result = instance.getGameState();
        assertEquals(expResult, result);

        // Test den wechselnden Attackstatus
        instance2.attack(0, 0);
        attackFirst = false;
        Thread.sleep(500);
        assertEquals(attackFirst, instance2.isItMyTurn());
        assertEquals(!attackFirst, instance.isItMyTurn());
        // Gegnerisches Feld darf an 0/0 nicht UNKNOWN sein.
        List<FieldState[]> updatedFields = instance2.getUpdatedFields();
        FieldState notExpectedFieldState = FieldState.UNKNOWN;
        assertNotSame(notExpectedFieldState, updatedFields.get(0)[0]);
        // Umgekehrt muss beim Gegner MISSED bzw. HIT als Feldstatus eingetragen werden
        updatedFields = instance.getUpdatedFields();
        if (updatedFields.get(1)[0] != FieldState.MISSED && updatedFields.get(1)[0] != FieldState.HIT) {
            fail("Feldstatus nicht MISSED oder HIT");
        }
        
        instance.attack(0, 0);
        attackFirst = true;
        Thread.sleep(500);
        assertEquals(attackFirst, instance2.isItMyTurn());
        assertEquals(!attackFirst, instance.isItMyTurn());

        instance2.attack(1, 0);
        attackFirst = false;
        Thread.sleep(500);
        assertEquals(attackFirst, instance2.isItMyTurn());
        assertEquals(!attackFirst, instance.isItMyTurn());
        
        t.interrupt();
        t2.interrupt();
    }

    /**
     * Test of getWidth method, of class CPlayingFieldController.
     */
    @Test
    public void testGetWidth() {
        System.out.println("getWidth");
        CPlayingFieldController instance = new CPlayingFieldController(1, 1, "127.0.0.1", 54321);
        int expResult = 1;
        int result = instance.getWidth();
        assertEquals(expResult, result);

        instance = new CPlayingFieldController(1999, 1, "127.0.0.1", 54321);
        expResult = 1999;
        result = instance.getWidth();
        assertEquals(expResult, result);
    }

    /**
     * Test of getHeight method, of class CPlayingFieldController.
     */
    @Test
    public void testGetHeight() {
        System.out.println("getHeight");
        CPlayingFieldController instance = new CPlayingFieldController(1, 1, "127.0.0.1", 54321);
        int expResult = 1;
        int result = instance.getWidth();
        assertEquals(expResult, result);

        instance = new CPlayingFieldController(1, 1999, "127.0.0.1", 54321);
        expResult = 1999;
        result = instance.getHeight();
        assertEquals(expResult, result);
    }
}