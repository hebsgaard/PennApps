import java.net.InetSocketAddress;


public class RundturMessage extends NetworkMessage {

	public InetSocketAddress oprAfsender;

	public RundturMessage(InetSocketAddress sender, InetSocketAddress receiver,InetSocketAddress oprAfsender) {
		super(sender, receiver);
		this.oprAfsender=oprAfsender;
		// TODO Auto-generated constructor stub
	}
	
	public String toString(){
		return "RundturMessage "+sender+" "+receiver+" "+oprAfsender;
	}
	

}
