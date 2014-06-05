import java.io.Serializable;
import java.net.InetSocketAddress;


public class ChangeSuccessorMessage extends NetworkMessage implements Serializable {
	private InetSocketAddress newSuccessor;
	private int numberOfNodes;
	
	public ChangeSuccessorMessage(InetSocketAddress sender,InetSocketAddress receiver,InetSocketAddress newSuccessor,int numberOfNodes){
		super(sender,receiver);
		this.newSuccessor=newSuccessor;
		this.numberOfNodes=numberOfNodes;
	}
	
	public int getNumberOfNodes(){
		return numberOfNodes;
	}
	
	public String toString(){
		return "ChangeSuccessorMessage "+sender+" "+receiver+" "+newSuccessor+" "+numberOfNodes;
	}
	
	public InetSocketAddress getNewSuccessor() {
		return newSuccessor;
	}
}
