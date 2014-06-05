
import java.io.Serializable;
import java.net.InetSocketAddress;
import java.security.acl.LastOwnerException;


public class TextRemoveEvent extends MyTextEvent implements Serializable{

	private int length;
	
	public TextRemoveEvent(InetSocketAddress sender, InetSocketAddress receiver,int navn,int offset, int length, int r, int lastName,int lastLamportCount) {
		super(sender,receiver, navn, offset, r,lastName,lastLamportCount);
		this.length = length;
	}
	
	public int getLength() { 
		return length; 
	}
	
	public String toString(){
		return " TEXTREMOVEEVENTMESSAGE sender: "+sender+" receiver: "+receiver+"  navn: "+getName()+" offset: "+getOffset()+" længde: "+getLength()+" lamportnr : "+getLamportCounter()+" lastname: "+getLastName()+" lastlamportcount: "+getLastLamportCount();
	}
}