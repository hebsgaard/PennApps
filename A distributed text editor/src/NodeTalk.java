import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;


public class NodeTalk {
//	private Node node;
	private int navn;
	public Server serverListener;
	public Client client;
	protected InetSocketAddress myServerName;
	protected InetSocketAddress myClientName;
	private InetSocketAddress suc;
	private InetSocketAddress pre;
	private JoinGroupMessage JGM;
	private boolean groupJoined=false;
	public InetSocketAddress BROADCAST;
	private DocumentEventCapturer dec;
	private LamportCounterObj lamportCounterObj;
	private DistributedTextEditor dte;
	private int numberOfNodes=2;
	private boolean nONUpdate=false;
	private ServerIndivid actualServer;


	public NodeTalk(int nr,int listeningportnr, LamportCounterObj l, DistributedTextEditor dte){
		try {
			myServerName=new InetSocketAddress(InetAddress.getLocalHost().getHostAddress(),listeningportnr);
			dte.writeToArea2("My ip-address "+myServerName.toString());
			BROADCAST=new InetSocketAddress(InetAddress.getByName("255.255.255.255"),65535);
		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
		this.lamportCounterObj=l;
		navn=nr;
//		node=new Node(nr);
		serverListener=new Server(listeningportnr,this, lamportCounterObj);
		Thread serverThread=new Thread(serverListener);
		serverThread.start();
		this.dte=dte;
	}
	
	public int getNumberOfNodes(){
		return numberOfNodes;
	}
	
	public void incNumberOfNodes(){
		numberOfNodes++;
	}
	
	public void setDEC(DocumentEventCapturer dec){
		this.dec=dec;
		serverListener.setDEC(dec);
	}
	
	public synchronized void processMessage(final NetworkMessage networkMessage) {
		if (networkMessage instanceof ChangeSuccessorMessage) {
			ChangeSuccessorMessage changeSuccessorMessage = (ChangeSuccessorMessage) networkMessage;
			suc = changeSuccessorMessage.getNewSuccessor();
			numberOfNodes = changeSuccessorMessage.getNumberOfNodes();
			nONUpdate = true;
			Client newClient = new Client(suc, this, dec);
			Thread newClientThread = new Thread(newClient);
			newClientThread.start();
			client.closeOurSocket();
			client = newClient;
			Thread timer = new Thread() {
				public void run() {
					try {
						dte.writeToArea2("Wait for 2 seconds");
						sleep(2000);
						client.networkMessageClientSentQueue.add(new ContinueMessage(myClientName, BROADCAST, myClientName, dte.getArea1().getText()));
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
			};
			timer.start();
		}
		if (networkMessage instanceof JoinGroupMessage){
			if (client != null) {
				dec.setMayWeSwitch(false);
				dte.setArea1Editable(false);
				client.networkMessageClientSentQueue.add(new PauseMessage(myClientName, BROADCAST, myClientName));
				Thread timer = new Thread() {
					public void run() {
						try {
							dte.writeToArea2("Working for 3 seconds with broadcast");
							sleep(3000);
							incNumberOfNodes();
							actualServer.networkMessageServerSentQueue.add(new ChangeSuccessorMessage(myClientName, pre, ((JoinGroupMessage) networkMessage).myServerName,numberOfNodes));
							pre=networkMessage.sender;
						} catch (InterruptedException e) {
							e.printStackTrace();
						}
					}
				};
				timer.start();
			} else{
				client=new Client((((JoinGroupMessage) networkMessage).myServerName), this, dec);
				Thread clientThread=new Thread(client);
				clientThread.start();
				pre=((JoinGroupMessage) networkMessage).sender;
			}
		}
		if (networkMessage instanceof PauseMessage) {
			PauseMessage pm = (PauseMessage) networkMessage;
			InetSocketAddress os = pm.getOprSender();
			if (!pm.getOprSender().equals(myClientName)) {
				dte.writeToArea2("Wait for it...");
				dec.setMayWeSwitch(false);
				dte.setArea1Editable(false);
				client.networkMessageClientSentQueue.add(new PauseMessage(myClientName, BROADCAST, os));
			}
		}
		if (networkMessage instanceof ContinueMessage) {
			ContinueMessage cm = (ContinueMessage) networkMessage;
			InetSocketAddress os = cm.getOprSender();
			String atft = cm.getAllTheFuckingText();
			if (!cm.getOprSender().equals(myClientName)) {
				client.networkMessageClientSentQueue.add(new ContinueMessage(myClientName, BROADCAST, os, atft));
			}
			dte.getEr().resetTabel();
			dec.setMayWeSwitch(true);
			dte.setArea1Text(atft);
			dte.writeToArea2("You can now type in the editor again");
		}
		if (networkMessage instanceof UpdateNumberOfNodesMessage){
			UpdateNumberOfNodesMessage unon=(UpdateNumberOfNodesMessage)networkMessage;
			if (!unon.getOprSender().toString().equals(myClientName.toString())){
				numberOfNodes=unon.getNumberOfNodes();
				if (serverListener.getNextServer()!=null){
					actualServer=serverListener.getNextServer();
					serverListener.resetNextServer();
				}
				actualServer.networkMessageServerSentQueue.add(new UpdateNumberOfNodesMessage(myClientName,BROADCAST,unon.getOprSender(),unon.getNumberOfNodes()));
			} else {
				numberOfNodes = unon.getNumberOfNodes();
			}
		}
		if (networkMessage instanceof RundturMessage){
			if (!((RundturMessage)networkMessage).oprAfsender.toString().equals(myClientName.toString())){
				client.networkMessageClientSentQueue.add(new RundturMessage(myClientName,suc,((RundturMessage)networkMessage).oprAfsender));
			}
		}
	}
	public int getMyName()  {
		return navn;
	    }

	public void join(InetSocketAddress n){
		suc=n;
		JGM=new JoinGroupMessage(myClientName,suc,myServerName);
		client=new Client(n,this, dec);
		Thread clientThread=new Thread(client);
		clientThread.start();
		dte.writeToArea2("We are connecting...");
	}
	
	public void leave(){
		JGM=null;
		dec.setMayWeSwitch(false);
		dte.setArea1Editable(false);
		client.networkMessageClientSentQueue.add(new PauseMessage(myClientName, BROADCAST, myClientName));
		Thread timer = new Thread() {
			public void run() {
				try {
					dte.writeToArea2("Leaving...");
					sleep(3000);
					actualServer.networkMessageServerSentQueue.add(new ChangeSuccessorMessage(myServerName, pre, suc, (numberOfNodes-1)));
					dte.writeToArea2("Changing successors - please wait 3 seconds");
					sleep(2000);
					actualServer.networkMessageServerSentQueue.add(new StopMessage(BROADCAST,BROADCAST));
					sleep(1000);
					dte.getEr().resetTabel();
					dec.resetDEC();
					lamportCounterObj.resetLamportCounter();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		};
		timer.start();
		client.closeOurSocket();
		groupJoined=false;
	}
	
	public void clientReadyListener(){
		if (!groupJoined && (JGM!=null)){
			JGM.setsender(myClientName);
			client.networkMessageClientSentQueue.add(JGM);
		}
		groupJoined =true;
		if (nONUpdate){
			actualServer.networkMessageServerSentQueue.add(new UpdateNumberOfNodesMessage(myClientName,BROADCAST,myClientName,numberOfNodes));
			nONUpdate=false;
		}
	}
	
	public void rundtur(){
		client.networkMessageClientSentQueue.add(new RundturMessage(myClientName,suc,myClientName));
	}

	public void setActualServer(ServerIndivid actualServer) {
		this.actualServer=actualServer;
	}
}
