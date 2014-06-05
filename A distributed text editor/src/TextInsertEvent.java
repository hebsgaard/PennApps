
import java.io.Serializable;
import java.net.InetSocketAddress;

/**
 * 
 * @author Jesper Buus Nielsen
 *
 */
public class TextInsertEvent extends MyTextEvent implements Serializable{

	private String text;
	
	public TextInsertEvent(InetSocketAddress sender, InetSocketAddress receiver, int navn,int offset, String text, int r,int lastName,int lastLamportCount) {
		super(sender,receiver,navn,offset, r,lastName,lastLamportCount);
		this.text = text;
	}
	
	public String getText() { 
		return text; 
	}
	
	public String toString(){
		return "\nTEXTINSERTEVENTMESSAGE sender: "+sender+" receiver: "+receiver+" navn: "+getName()+" offset: "+getOffset()+" lamportnr: "+getLamportCounter()+ "text : "+getText()+" lastname: "+getLastName()+" lastlamportcount: "+getLastLamportCount();
	}
}
