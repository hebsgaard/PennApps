
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.List;
import java.awt.event.ActionEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.FileWriter;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.UnknownHostException;
import java.util.IllegalFormatException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ActionMap;
import javax.swing.BoxLayout;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.text.AbstractDocument;
import javax.swing.text.DefaultEditorKit;

public class DistributedTextEditor extends JFrame {

    private JTextArea area1 = new JTextArea(10,120);
    private JTextArea area2 = new JTextArea(5,120);     
    private final JTextField ipaddress = new JTextField("IP address here");     
    private final JTextField portNumber = new JTextField("Port number here");     
    private final JTextField nodeNavn = new JTextField("NodeNavn");     
    
    private EventReplayer er;
    private Thread ert; 

    private JFileChooser dialog = 
    		new JFileChooser(System.getProperty("user.dir"));

    private String currentFile = "Untitled";
    private boolean changed = false;
    private boolean connected = false;
    private LamportCounterObj lamportCounterObj=new LamportCounterObj();

    private NodeTalk nodeTalk;
    
    private DocumentEventCapturer dec=new DocumentEventCapturer(this, lamportCounterObj);
    private DistributedTextEditor myDTE=this;
    
    public DistributedTextEditor() {
    	ipaddress.addMouseListener(new MouseAdapter() {
    		@Override
    		public void mouseClicked(MouseEvent e) {
    			ipaddress.setText("");
    		}
		});
    	
    	portNumber.addMouseListener(new MouseAdapter() {
    		@Override
    		public void mouseClicked(MouseEvent e) {
    			portNumber.setText("");
    		}
		});
    	
    	nodeNavn.addMouseListener(new MouseAdapter() {
    		@Override
    		public void mouseClicked(MouseEvent e) {
    			nodeNavn.setText("");
    		}
		});
    	area1.setFont(new Font("Monospaced",Font.PLAIN,12));
    	
    	area2.setFont(new Font("Monospaced",Font.PLAIN,12));
    	((AbstractDocument)area1.getDocument()).setDocumentFilter(dec);
    	area2.setEditable(false);
    	area1.setEditable(false);
    	
    	Container content = getContentPane();
    	content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));
    	
    	JScrollPane scroll1 = 
    			new JScrollPane(area1, 
    					JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
    					JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
    	content.add(scroll1,BorderLayout.CENTER);

    	JScrollPane scroll2 = 
    			new JScrollPane(area2, 
    					JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
    					JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		content.add(scroll2,BorderLayout.CENTER);	
		
		content.add(ipaddress,BorderLayout.CENTER);	
		content.add(portNumber,BorderLayout.CENTER);	
		content.add(nodeNavn,BorderLayout.CENTER);
		
	JMenuBar JMB = new JMenuBar();
	setJMenuBar(JMB);
	JMenu file = new JMenu("File");
	JMenu edit = new JMenu("Edit");
	JMB.add(file); 
	JMB.add(edit);
	
	file.add(Listen);
	file.add(Connect);
	file.add(Disconnect);
	file.addSeparator();
	file.add(Save);
	file.add(SaveAs);
	file.add(Quit);
		
	edit.add(Copy);
	edit.add(Paste);
	edit.getItem(0).setText("Copy");
	edit.getItem(1).setText("Paste");

	Save.setEnabled(false);
	SaveAs.setEnabled(false);
		
	setDefaultCloseOperation(EXIT_ON_CLOSE);
	pack();
	area1.addKeyListener(k1);
	setTitle("Disconnected");
	setVisible(true);
	Connect.setEnabled(false);
	Disconnect.setEnabled(false);
    }

    private KeyListener k1 = new KeyAdapter() {
	    public void keyPressed(KeyEvent e) {
		changed = true;
		Save.setEnabled(true);
		SaveAs.setEnabled(true);
	    }
	};

    Action Listen = new AbstractAction("Listen") {
	    public void actionPerformed(ActionEvent e) {
	    	saveOld();
	    	area1.setText("");
	    	area1.setEditable(true);
	    	InetAddress localhost;
	    	String localhostAddress = "";
			try {
				localhost = InetAddress.getLocalHost();
				localhostAddress = localhost.getHostAddress();
			} catch (UnknownHostException e1) {
				e1.printStackTrace();
			}
			int port = 0;
			try {
				port = Integer.parseInt(portNumber.getText());
				if ((port > 65535) || (port < 1025)){
					throw new UnsupportedOperationException("Write a number between 1026 and 65535");
				}
				nodeTalk=new NodeTalk(Integer.parseInt(nodeNavn.getText()),port, lamportCounterObj, myDTE);
				dec.setNodeTalk(nodeTalk);
				nodeTalk.setDEC(dec);
				er = new EventReplayer(dec, area1,nodeTalk);
				ert = new Thread(er);
				ert.start();
			} catch (Exception e1) {
				portNumber.setText(e1.toString());
			}
	    	setTitle("I'm listening on " + localhostAddress + ":" + portNumber.getText());
	    	changed = false;
	    	Save.setEnabled(false);
	    	SaveAs.setEnabled(false);
	    	Listen.setEnabled(false);
	    	Connect.setEnabled(true);
	    }
	};

    Action Connect = new AbstractAction("Connect") {
	    public void actionPerformed(ActionEvent e) {
	    	saveOld();
	    	area1.setText("");
	    	area1.setEditable(true);
			int port = 0;
			try {
				port = Integer.parseInt(portNumber.getText());
				if ((port > 65535) || (port < 1025)){
					throw new UnsupportedOperationException("Write a number between 1026 and 65535");
				}
		    	nodeTalk.join(new InetSocketAddress(InetAddress.getByName(ipaddress.getText()),port));
			} catch (Exception e1) {
				portNumber.setText(e1.toString());
			}

	    	setTitle("Connecting to " + ipaddress.getText() + ":" + portNumber.getText() + "...");
	    	changed = false;
	    	Save.setEnabled(false);
	    	SaveAs.setEnabled(false);
	    	Listen.setEnabled(false);
	    	Disconnect.setEnabled(true);
	    }
	};

    Action Disconnect = new AbstractAction("Disconnect") {
	    public void actionPerformed(ActionEvent e) {	
	    	Disconnect.setEnabled(false);
	    	nodeTalk.leave();
	    }
	};

    Action Save = new AbstractAction("Save") {
	    public void actionPerformed(ActionEvent e) {
		if(!currentFile.equals("Untitled"))
		    saveFile(currentFile);
		else
		    saveFileAs();
	    }
	};

    Action SaveAs = new AbstractAction("Save as...") {
	    public void actionPerformed(ActionEvent e) {
	    	saveFileAs();
	    }
	};

    Action Quit = new AbstractAction("Quit") {
	    public void actionPerformed(ActionEvent e) {
	    	saveOld();
	    	System.exit(0);
	    }
	};
	
    ActionMap m = area1.getActionMap();

    Action Copy = m.get(DefaultEditorKit.copyAction);
    Action Paste = m.get(DefaultEditorKit.pasteAction);

    private void saveFileAs() {
	if(dialog.showSaveDialog(null)==JFileChooser.APPROVE_OPTION)
	    saveFile(dialog.getSelectedFile().getAbsolutePath());
    }
    
    private void saveOld() {
    	if(changed) {
	    if(JOptionPane.showConfirmDialog(this, "Would you like to save "+ currentFile +" ?","Save",JOptionPane.YES_NO_OPTION)== JOptionPane.YES_OPTION)
		saveFile(currentFile);
    	}
    }
    
    private void saveFile(String fileName) {
	try {
	    FileWriter w = new FileWriter(fileName);
	    area1.write(w);
	    w.close();
	    currentFile = fileName;
	    changed = false;
	    Save.setEnabled(false);
	}
	catch(IOException e) {
	}
    }
    
    public void writeToArea2(String s){
    	area2.insert(s+"\n",0);
    }
    public static void main(String[] arg) {
    	new DistributedTextEditor();
    }
    
    public JTextArea getArea1() {
    	return area1;
    }
    
    public EventReplayer getEr() {
    	return er;
    }

	public void setArea1Editable(final boolean b) {
		EventQueue.invokeLater(new Runnable() {
			
			@Override
			public void run() {
				area1.setEditable(b);
			}
		});
	}
	
	public void setArea1Text(final String s) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				area1.setEditable(false);
				dec.disableSend();
				try {
					area1.setText("");
					area1.setText(s);
				} catch (Exception e) {
					System.err.println(e);
					/*
					 * We catch all axceptions, as an uncaught exception
					 * would make the EDT unwind, which is now healthy.
					 */
				}
				dec.enableSend();
				if (dec.mayWeSwitch) {
					area1.setEditable(true);
				}
			}
		});
	}
}