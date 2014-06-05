import java.io.Serializable;
import java.net.InetSocketAddress;


public class UpdateNumberOfNodesMessage extends NetworkMessage{
	private int numberOfNodes;
	private InetSocketAddress oprSender;

	public UpdateNumberOfNodesMessage(InetSocketAddress sender,
			InetSocketAddress receiver,InetSocketAddress oprSender,int numberOfNodes) {
		super(sender, receiver);
		this.numberOfNodes=numberOfNodes;
		this.oprSender=oprSender;
	}
	
	public int getNumberOfNodes(){
		return numberOfNodes;
	}
	
	public InetSocketAddress getOprSender(){
		return oprSender;
	}

}
