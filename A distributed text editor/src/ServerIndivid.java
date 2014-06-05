import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.util.concurrent.LinkedBlockingQueue;

public class ServerIndivid {
	private Socket serverSocket;
	protected LinkedBlockingQueue<NetworkMessage> networkMessageServerSentQueue = new LinkedBlockingQueue<NetworkMessage>();
	protected LinkedBlockingQueue<NetworkMessage> networkMessageServerReceiveQueue = new LinkedBlockingQueue<NetworkMessage>();
	private DocumentEventCapturer dec;
	private NodeTalk nodeTalk;
	private LamportCounterObj lamportCounterObj;

	public ServerIndivid(Socket serverSocket, DocumentEventCapturer dec,
		LamportCounterObj lamportCounterObj, NodeTalk nodeTalk) {
		this.serverSocket = serverSocket;
		this.dec = dec;
		this.nodeTalk = nodeTalk;
		this.lamportCounterObj = lamportCounterObj;
		startServerIndivid();
	}

	public void startServerIndivid() {
		// Resets the server and clear the textfields and enable the
		// disconnect-buttom
		Thread writeToClient = new Thread() {
			public void run() {
				try {
					ObjectOutputStream oos = new ObjectOutputStream(
							serverSocket.getOutputStream());
					boolean wasInterrupted = false;
					while (!wasInterrupted) {
						NetworkMessage nwm = networkMessageServerSentQueue
								.take();
						if (nwm instanceof StopMessage) {
							wasInterrupted = true;
						} else {

							oos.writeObject(nwm);

						}
						// System.out.println("server sender "+nwm);
					}
					serverSocket.close();
				} catch (Exception e) {
					System.out
							.println(nodeTalk.getMyName() + "server send" + e);
				}
			}
		};
		writeToClient.start();

		if (serverSocket != null) {
			Thread receiveFromClient = new Thread() {
				public void run() {

					try {
						// For receiving text from the client
						ObjectInputStream ois = new ObjectInputStream(
								serverSocket.getInputStream());
						boolean wasInterrupted = false;
						while (!wasInterrupted) {
							try {
								Object recObject = ois.readObject();
								// System.out.println("server modtager"+recObject);
								if (recObject instanceof MyTextEvent) {
									MyTextEvent mte = (MyTextEvent) recObject;
									lamportCounterObj.setToMaxPlusOne(mte
											.getLamportCounter());
									if (mte.getName() != nodeTalk.getMyName()) {
										nodeTalk.client.networkMessageClientSentQueue
												.add(mte);
										dec.insertTextObject(mte);

									}
								} else {
									if (recObject instanceof NetworkMessage) {
										nodeTalk.processMessage((NetworkMessage) recObject);
									}
								}

							} catch (Exception e1) {
								wasInterrupted = true;
								serverSocket.close();
							}
						}
					} catch (Exception e1) {
						System.out.println(nodeTalk.getMyName()
								+ "server modt " + e1);
					}
					networkMessageServerSentQueue.add(new StopMessage(
							nodeTalk.BROADCAST, nodeTalk.BROADCAST));
				}
			};
			receiveFromClient.start();

		}
	}
}
