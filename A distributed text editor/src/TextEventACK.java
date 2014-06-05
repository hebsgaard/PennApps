import java.io.Serializable;
import java.net.InetSocketAddress;


public class TextEventACK extends MyTextEvent implements Serializable{
	private int orgName;
	TextEventACK(InetSocketAddress sender, InetSocketAddress receiver, int navn, int offset, int r, int orgName) {
		super(sender, receiver, navn, offset, r, 0,0);
		this.orgName=orgName;
	}
	
	public int getOrgName(){
		return orgName;
	}
	
	public String toString(){
		return " TextEventACK sender: "+sender+" receiver: "+receiver+" navn: "+getName()+" oprnavn: "+orgName+" lamportnr: "+getLamportCounter();
	}

}
