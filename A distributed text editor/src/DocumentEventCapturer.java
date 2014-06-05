
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DocumentFilter;


/**
 * 
 * This class captures and remembers the text events of the given document on
 * which it is put as a filter. Normally a filter is used to put restrictions on
 * what can be written in a buffer. In out case we just use it to see all the
 * events and make a copy.
 * 
 * @author Jesper Buus Nielsen
 * 
 */
public class DocumentEventCapturer extends DocumentFilter {

	/*
	 * We are using a blocking queue for two reasons: 1) They are thread safe,
	 * i.e., we can have two threads add and take elements at the same time
	 * without any race conditions, so we do not have to do explicit
	 * synchronization. 2) It gives us a member take() which is blocking, i.e.,
	 * if the queue is empty, then take() will wait until new elements arrive,
	 * which is what we want, as we then don't need to keep asking until there
	 * are new elements.
	 */
	protected LinkedBlockingQueue<MyTextEvent> eventHistory = new LinkedBlockingQueue<MyTextEvent>();

	private boolean send = false;
    private DistributedTextEditor dte;
    private int r;
    private LamportCounterObj lamportCounterObj;
    private int lastName;
    private int lastLamportCount;
    
    public void resetDEC(){
    	lastName=0;
    	lastLamportCount=0;
    	send=false;
    	mayWeSwitch=true;
    }
    
    public boolean mayWeSwitch = true;

    public void setLastName(int n){
    	lastName=n;
    }
    
    public void setLastLamportCount(int l){
    	lastLamportCount=l;
    }
    
	private NodeTalk nodeTalk; 
    
    public DocumentEventCapturer(DistributedTextEditor dte,LamportCounterObj lamportCounterObj ) {
    	this.dte = dte;
    	this.lamportCounterObj=lamportCounterObj;
    }

    public void setNodeTalk(NodeTalk nodeTalk){
    	this.nodeTalk=nodeTalk;
    }
 
    public void disableSend(){
		send=false;
	}
	
	public void enableSend(){
		send=true;
	}

	/**
	 * If the queue is empty, then the call will block until an element arrives.
	 * If the thread gets interrupted while waiting, we throw
	 * InterruptedException.
	 * 
	 * @return Head of the recorded event queue.
	 */
	MyTextEvent take() throws InterruptedException {
		return eventHistory.take();
	}

	public void insertString(FilterBypass fb, int offset, String str, AttributeSet a) throws BadLocationException {
		/* Queue a copy of the event and then modify the textarea */
		if (send){
			lamportCounterObj.inc();
			r=lamportCounterObj.getLamportCounter();  // to make sure that the same countR are used in both queues
			nodeTalk.client.networkMessageClientSentQueue.add(new TextInsertEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, str, r,lastName,lastLamportCount));
			eventHistory.add(new TextInsertEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, str, r,lastName,lastLamportCount));
		}else{
			super.insertString(fb, offset, str, a);
		}
	}

	public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
		/* Queue a copy of the event and then modify the textarea */
		if (send){
			lamportCounterObj.inc();
			r=lamportCounterObj.getLamportCounter();  // to make sure that the same countR are used in both queues
			nodeTalk.client.networkMessageClientSentQueue.add(new TextRemoveEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, length, r,lastName,lastLamportCount));
			eventHistory.add(new TextRemoveEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, length, r,lastName,lastLamportCount));
		} else{
			super.remove(fb, offset, length);
		}
	}

	public void replace(FilterBypass fb, int offset, int length, String str, AttributeSet a)
			throws BadLocationException {
		/* Queue a copy of the event and then modify the text */
		if (send){
			if (length > 0) {
				lamportCounterObj.inc();
				r=lamportCounterObj.getLamportCounter();  // to make sure that the same countR are used in both queues
				nodeTalk.client.networkMessageClientSentQueue.add(new TextRemoveEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, length, r,lastName,lastLamportCount));
				eventHistory.add(new TextRemoveEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, length, r,lastName,lastLamportCount));
			}
			lamportCounterObj.inc();
			r=lamportCounterObj.getLamportCounter();  // to make sure that the same countR are used in both queues
			nodeTalk.client.networkMessageClientSentQueue.add(new TextInsertEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, str, r,lastName,lastLamportCount));
			eventHistory.add(new TextInsertEvent(nodeTalk.myServerName,nodeTalk.BROADCAST,nodeTalk.getMyName(),offset, str, r,lastName,lastLamportCount));
		}else{
			super.replace(fb, offset, length, str, a);
		}
	}
	
	public void insertTextObject(MyTextEvent mt) {

		/* Queue a copy of the event and then modify the textarea */
		eventHistory.add(mt);
	}
	
	public void setMayWeSwitch(Boolean mws) {
		mayWeSwitch = mws;
	}

}