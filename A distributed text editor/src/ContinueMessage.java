import java.net.InetSocketAddress;


public class ContinueMessage extends NetworkMessage {

	private InetSocketAddress oprSender;
	private String allTheFuckingText;

	public ContinueMessage(InetSocketAddress sender, InetSocketAddress receiver, InetSocketAddress oprSender, String allTheFuckingText) {
		super(sender, receiver);
		this.oprSender = oprSender;
		this.allTheFuckingText = allTheFuckingText;
	}

	public String getAllTheFuckingText() {
		return allTheFuckingText;
	}

	public void setAllTheFuckingText(String allTheFuckingText) {
		this.allTheFuckingText = allTheFuckingText;
	}

	public InetSocketAddress getOprSender() {
		return oprSender;
	}

	public void setOprSender(InetSocketAddress oprSender) {
		this.oprSender = oprSender;
	}

}
