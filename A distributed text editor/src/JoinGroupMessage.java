import java.io.Serializable;
import java.net.InetSocketAddress;


public class JoinGroupMessage extends NetworkMessage implements Serializable{
	public InetSocketAddress myServerName;

	public JoinGroupMessage(InetSocketAddress sender,InetSocketAddress receiver,InetSocketAddress myServerName){
		super(sender,receiver);
		this.myServerName=myServerName;
	}

	public String toString(){
		return "JoinGroupMessage myservername: "+myServerName+" sender: "+sender+" reciever: "+receiver;
	}
	
	public void setsender(InetSocketAddress s){
		sender=s;
	}
}
