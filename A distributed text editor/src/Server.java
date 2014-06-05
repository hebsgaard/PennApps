import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketImpl;
import java.util.concurrent.LinkedBlockingQueue;



public class Server extends Thread {
	private int portNumber;
	private ServerSocket serverSocket;
	private Socket rocketSocket;
	private NodeTalk nodeTalk;
	private DocumentEventCapturer dec;
	protected LamportCounterObj lamportCounterObj;
	ServerIndivid actualServer=null;
	ServerIndivid nextServer=null;

	public Server(int portNumber, NodeTalk nodetalk,LamportCounterObj lamportCounterObj){
		this.nodeTalk=nodetalk;
		this.portNumber=portNumber;
		this.lamportCounterObj=lamportCounterObj;

	}
	public void setDEC(DocumentEventCapturer dec){
		this.dec=dec;
	}
	
	
	protected void registerOnPort() {
		try {
			serverSocket = new ServerSocket(portNumber);
		} catch (IOException e) {
			serverSocket = null;
			System.err.println("Cannot open server socket on port number" + portNumber);
			System.err.println(e);
			System.exit(-1);
		}
	}

	public void deregisterOnPort() {
		if (serverSocket != null) {
			try {
				serverSocket.close();
				serverSocket = null;
			} catch (IOException e) {
				System.err.println(e);
			}
		}
	}

	/**
	 * 
	 * Waits for the next client to connect on port number portNumber or takes
	 * the next one in line in case a client is already trying to connect.
	 * Returns the socket of the connection, null if there were any failures.
	 */
	protected Socket waitForConnectionFromClient() {
		Socket res = null;
		try {
			res = serverSocket.accept();
		} catch (IOException e) {
			System.out.println("Exception i server waitforconnectionfromclient "+e);
		}
		return res;
	}
	
	public void closeOurSocket() {
		try {
			rocketSocket.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public ServerIndivid getNextServer(){
		return nextServer;
	}
	public void resetNextServer(){
		nextServer=null;
	}
	public void run() {
		registerOnPort();
		boolean wasInterrupted=false;
		while (!wasInterrupted) {
			final Socket socket = waitForConnectionFromClient();
			rocketSocket = socket;
			if (actualServer==null){
				actualServer=new ServerIndivid(socket,dec, lamportCounterObj, nodeTalk);
			} else{
				nextServer=new ServerIndivid(socket, dec, lamportCounterObj, nodeTalk);
				
			}
			nodeTalk.setActualServer(actualServer);
		}
		deregisterOnPort();
		System.out.println("server Goodbye world!");
	}

}
