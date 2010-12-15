import java.awt.* ; 
import java.awt.event.* ; 
import java.io.* ; 
import java.net.* ;
import javax.swing.* ; 
import javax.swing.border.* ; 

public class TicTacToe extends JFrame 
                       implements ActionListener {
   JButton b11,b21,b31,
           b12,b22,b32,
           b13,b23,b33 ;
   boolean myturn ; 
   BufferedReader br ; 
   BufferedWriter bw ;
   Thread connection ; 
   Process prologProcess ; 
   String prolog ; 
   String ttt ; 

   /**
     *  Create a tic tac toe game, 
     *  prolog is the prolog command (e.g. "/opt/local/bin/swipl").
     *  ttt is the locator for ttt.pl (e.g. "/javalib/TicTacToe/ttt.pl").
     */
   public TicTacToe(String prolog, String ttt) { 
      this.prolog = prolog ; 
      this.ttt = ttt ; 
      b11 = new JButton("") ; 
      b21 = new JButton("") ; 
      b31 = new JButton("") ; 
      b12 = new JButton("") ; 
      b22 = new JButton("") ; 
      b32 = new JButton("") ; 
      b13 = new JButton("") ; 
      b23 = new JButton("") ; 
      b33 = new JButton("") ; 
      b11.setActionCommand("(1,1).") ; // prolog reads pair term
      b21.setActionCommand("(2,1).") ; 
      b31.setActionCommand("(3,1).") ; 
      b12.setActionCommand("(1,2).") ; 
      b22.setActionCommand("(2,2).") ; 
      b32.setActionCommand("(3,2).") ; 
      b13.setActionCommand("(1,3).") ; 
      b23.setActionCommand("(2,3).") ; 
      b33.setActionCommand("(3,3).") ; 
      Font f = new Font("monospaced",Font.PLAIN,64) ;
      b11.setFont(f) ; 
      b21.setFont(f) ; 
      b31.setFont(f) ; 
      b12.setFont(f) ; 
      b22.setFont(f) ; 
      b32.setFont(f) ; 
      b13.setFont(f) ; 
      b23.setFont(f) ; 
      b33.setFont(f) ; 
      b11.addActionListener(this) ; 
      b21.addActionListener(this) ; 
      b31.addActionListener(this) ; 
      b12.addActionListener(this) ; 
      b22.addActionListener(this) ; 
      b32.addActionListener(this) ; 
      b13.addActionListener(this) ; 
      b23.addActionListener(this) ; 
      b33.addActionListener(this) ; 
      JPanel panel = new JPanel() ; 
      panel.setLayout(new GridLayout(3,3)) ; 
      panel.add(b11) ; 
      panel.add(b21) ; 
      panel.add(b31) ; 
      panel.add(b12) ; 
      panel.add(b22) ; 
      panel.add(b32) ; 
      panel.add(b13) ; 
      panel.add(b23) ; 
      panel.add(b33) ; 
      //this.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE) ; 
      this.setTitle("Tic Tac Toe") ; 
      Border panelborder = BorderFactory.createLoweredBevelBorder() ; 
      panel.setBorder(panelborder) ; 
      this.getContentPane().add(panel) ; 
      this.setSize(300,300) ;
      this.setLocation(900,300) ; 
      this.myturn = true ; 

      Connector connector = new Connector(54321) ; 
      connector.start() ; 

      Socket sock ;
      try {
         sock = new Socket("127.0.0.1",54321) ;
         br = new BufferedReader(new InputStreamReader(sock.getInputStream())) ; 
         bw = new BufferedWriter(new OutputStreamWriter(sock.getOutputStream())) ; 
      } catch(Exception x) { System.out.println(x) ; }

      connection = new Thread() {
         public void run() { 
            while(true) {
               try{
                  String s = br.readLine() ; 
                  computer_move(s) ; 
               } catch(Exception xx) { System.out.println(xx) ; }
            }  
         }
      } ;
      connection.start() ;

      Thread shows = new Thread() { 
         public void run() { 
            setVisible(true) ;
         }
      } ;
      EventQueue.invokeLater(shows);

      // Start the prolog player

      try { 
         prologProcess = 
           Runtime.getRuntime().exec(prolog + " -f " + ttt) ; 
      } catch(Exception xx) {System.out.println(xx) ; }

      // On closing, kill the prolog process first and then exit
      this.addWindowListener(new WindowAdapter() { 
         public void windowClosing(WindowEvent w) { 
            if (prologProcess != null) prologProcess.destroy() ;
            System.exit(0) ; 
         }
      }) ; 

   } 

//       /opt/local/bin/swipl   /javalib/TicTacToe/ttt.pl
   public static void main(String[] args) { 
      String prolog = "/opt/local/bin/swipl" ;
      String ttt = "/javalib/TicTacToe/ttt.pl" ;
      boolean noargs = true ; 
      try { 
         prolog = args[0] ;
         ttt = args[1] ;
         noargs = false ; 
      } 
      catch (Exception xx) {
         System.out.println("usage: java TicTactoe  <where prolog>  <where ttt>") ; 
      }
      if (noargs) { 
         Object[] message = new Object[4] ; 
         message[0] = new Label("  prolog command") ;
         message[1] = new JTextField(prolog) ; 
         message[2] = new Label("  where ttt.pl ") ;
         message[3] = new JTextField(ttt) ; 
         try { 
            int I = JOptionPane.showConfirmDialog(null,message,"Where are Prolog and ttt.pl? ",JOptionPane.OK_CANCEL_OPTION) ;  
            if (I == 2 | I == 1) System.exit(0) ;
            System.out.println(I) ; 
            new TicTacToe(((JTextField)message[1]).getText().trim(),((JTextField)message[3]).getText().trim()) ; 
         } catch(Exception yy) {} 
      }
      else
         new TicTacToe(prolog,ttt) ; 
   }




   void computer_move(String s) { // " x ## y '
      String[] c = s.split(",") ; 
      int x = Integer.parseInt(c[0].trim()), 
          y = Integer.parseInt(c[1].trim()) ; 
      //System.out.println(x+","+y) ; 
      if (x == 1) {
         if (y == 1) b11.setText("O") ; 
         else if (y == 2) b12.setText("O") ; 
         else if (y == 3) b13.setText("O") ; 
      }
      else if (x == 2) {
         if (y == 1) b21.setText("O") ;
         else if (y == 2) b22.setText("O") ; 
         else if (y == 3) b23.setText("O") ; 
      }
      else if (x == 3) { 
         if (y == 1) b31.setText("O") ;
         else if (y == 2) b32.setText("O") ; 
         else if (y == 3) b33.setText("O") ; 
      }
      if (winner()) connection.stop() ;
      else  myturn = true ;
   }

   /**
     * Java player
     */
   public void actionPerformed(ActionEvent act) {
      if (!myturn) return ; // otherwise 
      String s = ((JButton)act.getSource()).getText() ; 
      if (!s.equals("")) return  ; 
      ((JButton)(act.getSource())).setText("X") ; 
      try { 
         bw.write(act.getActionCommand() + "\n") ; 
         bw.flush() ;  
      } catch(Exception xx) { System.out.println(xx) ; } 
      myturn = false ; 
      if (winner()) connection.stop() ;
   }

   /**
     *  Do we have a winner?
     */
   boolean winner() { 
      return  line(b11,b21,b31) ||
         line(b12,b22,b32) ||
         line(b13,b23,b33) ||
         line(b11,b12,b13) ||
         line(b21,b22,b23) ||
         line(b31,b32,b33) ||
         line(b11,b22,b33) ||
         line(b13,b22,b31)  ;
   }

   /**
     *  Are three buttons marked with same player? 
     *  If, so color the line and return true.
     */
   boolean line(JButton b, JButton c, JButton d) {        
      if (!b.getText().equals("") &&b.getText().equals(c.getText()) &&
                c.getText().equals(d.getText()))  {
         if (b.getText().equals("O")) { 
            b.setBackground(Color.red) ;
            c.setBackground(Color.red) ;
            d.setBackground(Color.red) ; 
         } 
         else { 
            b.setBackground(Color.green) ;
            c.setBackground(Color.green) ;
            d.setBackground(Color.green) ; 
         }
         return true ;  
      } else return false;
   }

}
  
/*
If Java player closes GUI, then Prolog process is terminated.
Java process monitors "win" status of both players, signals a win,
and closes the connector and prolog player.
Prolog justs plays given position.
Write all of this up; it is interesting.
*/

