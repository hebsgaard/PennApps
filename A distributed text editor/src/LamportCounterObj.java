
public class LamportCounterObj {
	private int lamportCounter=0;
	
	public int getLamportCounter(){
		return lamportCounter;
	}

	public void setToMaxPlusOne(int remoteLamportCounter) {
		lamportCounter=Math.max(lamportCounter, remoteLamportCounter)+1;
	}
	
	public void inc(){
		lamportCounter++;
	}
	
	public void resetLamportCounter(){
		lamportCounter=0;
	}
	
}
