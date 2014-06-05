import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.concurrent.LinkedBlockingQueue;



public class Client extends Thread {
	private int portNumber;
	private Socket rocketSocket;
	private String serverName;
	private NodeTalk nodeTalk;
	private DocumentEventCapturer dec;
	public LinkedBlockingQueue<NetworkMessage> networkMessageClientReceiveQueue = new LinkedBlockingQueue<NetworkMessage>();
	public LinkedBlockingQueue<NetworkMessage> networkMessageClientSentQueue = new LinkedBlockingQueue<NetworkMessage>();


	public Client(InetSocketAddress n, NodeTalk nodeTalk, DocumentEventCapturer dec){
		this.serverName=n.getHostString();
		this.portNumber=n.getPort();
		this.nodeTalk=nodeTalk;
		this.dec=dec;
	}

	protected Socket connectToServer(String serverName) {
		Socket res = null;
		try {
			res = new Socket(serverName, portNumber);
		} catch (IOException e) {
			System.out.println(nodeTalk.getMyName()+" "+e.toString());
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

	public void run() {
		final Socket socket = connectToServer(serverName);
		
		rocketSocket = socket;
		nodeTalk.myClientName=(InetSocketAddress)socket.getLocalSocketAddress();
		nodeTalk.clientReadyListener();
		dec.enableSend();
		Thread writeToServer = new Thread() {
			public void run() {
				// For sending text to the server
				try {
					ObjectOutputStream oos = new ObjectOutputStream(socket.getOutputStream());
					boolean wasInterrupted = false;
					while (!wasInterrupted) {
						NetworkMessage nwm=networkMessageClientSentQueue.take();
						if (nwm instanceof StopMessage){
							wasInterrupted=true;
						}else{
							oos.writeObject(nwm);
						}
					}
					socket.close();
				} catch (Exception e) {
					System.err.println(nodeTalk.getMyName()+"client send "+e);
				}
			}
		};
		writeToServer.start();

		if (socket != null) {
			try {
				// For receiving text from the server
				ObjectInputStream ois = new ObjectInputStream(
						socket.getInputStream());
				boolean wasInterrupted = false;
				while (!wasInterrupted) {
					try {
						Object recObject= ois.readObject();
						if (recObject instanceof MyTextEvent){
							dec.insertTextObject((MyTextEvent) recObject);
						} else{
							if (recObject instanceof NetworkMessage) {
								nodeTalk.processMessage((NetworkMessage) recObject);
							}
						}
					} catch (Exception e) {
						wasInterrupted = true;
						System.out.println(nodeTalk.getMyName()+"klient send "+e);
						socket.close();
					}
				}
			} catch (Exception e) {
				System.out.println(nodeTalk.getMyName()+" We ignore this");
			}
		}
		System.out.println(nodeTalk.getMyName()+" client Goodbye world!");
	}

}
