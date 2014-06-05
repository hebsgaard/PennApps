import java.io.Serializable;
import java.net.InetSocketAddress;


public class NetworkMessage implements Serializable{
	public InetSocketAddress receiver;
	public InetSocketAddress sender;
	
	public NetworkMessage(InetSocketAddress sender,InetSocketAddress receiver){
		this.receiver=receiver;
		this.sender=sender;
	}
	
	public InetSocketAddress getSender(){
		return sender;
	}
}
