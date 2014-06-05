import java.net.InetSocketAddress;


public class PauseMessage extends NetworkMessage {

	private InetSocketAddress oprSender;

	public PauseMessage(InetSocketAddress sender, InetSocketAddress receiver, InetSocketAddress oprSender) {
		super(sender, receiver);
		this.oprSender=oprSender;
	}
	
	public InetSocketAddress getOprSender() {
		return oprSender;
	}

}
