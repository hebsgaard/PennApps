import java.net.InetSocketAddress;


public class StopMessage extends NetworkMessage{

	public StopMessage(InetSocketAddress sender, InetSocketAddress receiver) {
		super(sender, receiver);
	}

}
