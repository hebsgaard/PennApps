
import java.io.Serializable;
import java.net.InetSocketAddress;

/**
 * 
 * @author Jesper Buus Nielsen
 *
 */
public class MyTextEvent extends NetworkMessage implements Serializable{
	private int lamportCounter;
	private int offset;
	private int navn;
	private int lastName;
	private int lastLamportCount;
	
	MyTextEvent(InetSocketAddress sender,InetSocketAddress receiver,int navn,int offset, int r, int lastName, int lastLamportCount) {
		super(sender,receiver);
		this.offset = offset;
		lamportCounter = r;
		this.navn=navn;
		this.lastName=lastName;
		this.lastLamportCount=lastLamportCount;
	}
	
	int getOffset() { 
		return offset; 
	}
	
	public int getLamportCounter() {
		return lamportCounter;
	}

	public void setOffset(int i) {
		offset=i;
	}
	
	public int getName(){
		return navn;
	}

	public int getLastName() {
		return lastName;
	}


	public int getLastLamportCount() {
		return lastLamportCount;
	}


}