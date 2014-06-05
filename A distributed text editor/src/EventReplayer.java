import java.awt.EventQueue;
import java.util.LinkedList;
import java.util.ListIterator;

import javax.swing.JTextArea;


/**
 * 
 * Takes the event recorded by the DocumentEventCapturer and replays them in a
 * JTextArea. The delay of 1 sec is only to make the individual steps in the
 * reply visible to humans.
 * 
 * @author Jesper Buus Nielsen
 * 
 */
public class EventReplayer implements Runnable {

	private DocumentEventCapturer dec;
	private JTextArea area;
	private NodeTalk nodeTalk;
	private LinkedList<TEntry> tabel;

	private LinkedList<PEvent> priorityList;
	private boolean shouldWeACK;
	
	public void resetTabel() {
		tabel.clear();
	}

	public EventReplayer(DocumentEventCapturer dec, JTextArea area,
			NodeTalk nodeTalk) {
		this.dec = dec;
		this.area = area;
		this.nodeTalk=nodeTalk;
	}

	public void run() {
		boolean wasInterrupted = false;
		tabel = new LinkedList<TEntry>();
		priorityList=new LinkedList<PEvent>();
		while (!wasInterrupted) {
			try {
				MyTextEvent mte = dec.take();
				shouldWeACK=true;
				if (!(mte instanceof TextEventACK)) {
					insertInPrioList(mte);
					if ((mte.getName()!=nodeTalk.getMyName())&&(shouldWeACK)){
						nodeTalk.client.networkMessageClientSentQueue.add(new TextEventACK(nodeTalk.myClientName, nodeTalk.BROADCAST, nodeTalk.getMyName(), 0, mte.getLamportCounter(), mte.getName()));
					}else{
						shouldWeACK=true;
					}
				} else {
					TextEventACK mteACK = (TextEventACK) mte;
					MyTextEvent readymte=updateACK(mteACK.getName(), mteACK.getOrgName(),
							mteACK.getLamportCounter());
					if (readymte!=null){
						dec.setLastLamportCount(readymte.getLamportCounter());
						dec.setLastName(readymte.getName());
						if (readymte instanceof TextInsertEvent ){
							tabel.add(new TEntry(readymte.getLamportCounter(), readymte
									.getName(), readymte.getOffset(),
									((TextInsertEvent) readymte).getText().length()));
						} else { // TextRemoveEvent
							tabel.add(new TEntry(readymte.getLamportCounter(), readymte
									.getName(), readymte.getOffset(),
									-((TextRemoveEvent) readymte).getLength()));
						}
						cleanTabel(readymte.getName(),readymte.getLastName(),readymte.getLastLamportCount());
						readymte.setOffset(calculateOffset(readymte));
						sendACKer(readymte);
						doSomething(readymte);
					}
				}
			} catch (Exception e) {
				System.err.println("evplayer "+e);
				wasInterrupted = true;
			}
		}
	}

	private void cleanTabel(int name, int lastName, int lastLamportCount) {
		ListIterator<TEntry> myIter = tabel.listIterator(0);
		TEntry i;
		while (myIter.hasNext()) {
			i = myIter.next();
			if ((i.getName() == lastName)
					&& (i.getLamportCount() < lastLamportCount)) {
				i.insertInsaysYes(name);
			}
			if (i.getSizeOfSaysYes() == nodeTalk.getNumberOfNodes()) {
				myIter.remove();
			}
		}
	}
			
	private void sendACKer(MyTextEvent mte) {
		if (mte.getName() == nodeTalk.getMyName()) {
			int i = 0;
			while (i < priorityList.size()) {
				MyTextEvent pmte = priorityList.get(i).getMte();
				if (pmte.getName() != nodeTalk.getMyName()) {
					nodeTalk.client.networkMessageClientSentQueue
							.add(new TextEventACK(nodeTalk.myClientName,
									nodeTalk.BROADCAST, nodeTalk.getMyName(),
									0, pmte.getLamportCounter(), pmte.getName()));
				} else{
					break;
				}
				i++;
			}
		}
	}

	private MyTextEvent updateACK(int name, int orgName, int lamportCounter) {
		int i=0;
		while (i<priorityList.size()){
			MyTextEvent mte=priorityList.get(i).getMte();
			if ((mte.getLamportCounter()==lamportCounter)&&(mte.getName()==orgName)){
				priorityList.get(i).incNumberOfACK();
				int bla=priorityList.get(i).getNumberOfACK();
				if (bla==(nodeTalk.getNumberOfNodes()-1)){
					if (orgName==nodeTalk.getMyName()){
						nodeTalk.client.networkMessageClientSentQueue.add(new TextEventACK(nodeTalk.myClientName, nodeTalk.BROADCAST, nodeTalk.getMyName(), 0, lamportCounter, orgName));
					}
					return priorityList.remove(i).getMte();
				}
			}
			i++;
		}
		return null;
	}

	int calculateOffset(MyTextEvent mte){
		int i=0;
		boolean start=false;
		int offset=mte.getOffset();
		while (i < tabel.size()) {
			TEntry tdata = tabel.get(i);
			if ((mte.getLastLamportCount() == 0) && (mte.getLastName() == 0)) {
				if (((mte.getLamportCounter() != tdata.getLamportCount()) || (mte
								.getName() != tdata.getName()))) {
					if (tdata.getOffset() <= mte.getOffset()) {
						offset = offset + tdata.getChange();
					}
				}
			} else {
				if ((mte.getLastLamportCount() == tdata.getLamportCount())
						&& (mte.getLastName() == tdata.getName())) {
					start = true;
				} else {
					if (start
							&& ((mte.getLamportCounter() != tdata
									.getLamportCount()) || (mte.getName() != tdata
									.getName()))) {
						if (tdata.getOffset() <= mte.getOffset()) {
							offset = offset + tdata.getChange();
						}
					}
				}
			}
			i++;
		}
		return offset;
	}

	void insertInPrioList(MyTextEvent mte){
		priorityList.add(getIndex(mte), new PEvent(mte));
	}
	
	int getIndex(MyTextEvent mte){
		int i=0;
		while (i<priorityList.size()){
			MyTextEvent pmte=priorityList.get(i).getMte();
			if (mte.getLamportCounter() > 
					pmte.getLamportCounter()) {
				if (pmte.getName()==nodeTalk.getMyName() ){
					shouldWeACK=false;
				}
				i++;
			} else {
				if ((mte.getLamportCounter() == pmte.getLamportCounter())
						&& (mte.getName() > pmte.getName())) {
					if (pmte.getName()==nodeTalk.getMyName() ){
						shouldWeACK=false;
					}
					i++;
				} else {
					return i;
				}
			}
		}
		return i;
	}

	void doSomething(MyTextEvent mte) {
		if (mte instanceof TextInsertEvent) {
			final TextInsertEvent tie = (TextInsertEvent) mte;
			EventQueue.invokeLater(new Runnable() {
				public void run() {
					area.setEditable(false);
					dec.disableSend();
					try {
						area.insert(tie.getText(), tie.getOffset());
					} catch (Exception e) {
						System.err.println(e);
						/*
						 * We catch all axceptions, as an uncaught exception
						 * would make the EDT unwind, which is now healthy.
						 */
					}
					dec.enableSend();
					if (dec.mayWeSwitch) {
						area.setEditable(true);
					}
				}
			});
		} else if (mte instanceof TextRemoveEvent) {
			final TextRemoveEvent tre = (TextRemoveEvent) mte;
			EventQueue.invokeLater(new Runnable() {
				public void run() {
					area.setEditable(false);
					dec.disableSend();
					try {
						area.replaceRange(null, tre.getOffset(),
								tre.getOffset() + tre.getLength());
					} catch (Exception e) {
						System.err.println(e);
						/*
						 * We catch all axceptions, as an uncaught exception
						 * would make the EDT unwind, which is now healthy.
						 */
					}
					dec.enableSend();
					if (dec.mayWeSwitch) {
						area.setEditable(true);
					}
				}
			});
		}
	}

	class PEvent{
		private MyTextEvent mte;
		private int numberOfACK=0;
		private boolean ownACK;
		
		public PEvent(MyTextEvent mte){
			this.mte=mte;
		}
		
		public void incNumberOfACK(){
			numberOfACK++;
		}
		
		public int getNumberOfACK() {
			return numberOfACK;
		}
		
		public void setNumberOfACK(int numberOfACK) {
			this.numberOfACK = numberOfACK;
		}
		
		public MyTextEvent getMte() {
			return mte;
		}
		
		public String toString(){
			return mte.toString()+" ACKcount : "+Integer.toString(numberOfACK);
		}
	}

	class TEntry {

		private int name;
		private int lamportCount;
		private int offset;
		private int change;
		private LinkedList<Integer> saysYesToRemove; 

		public TEntry(int l, int r, int offset, int change) {
			lamportCount = l;
			name = r;
			this.offset = offset;
			this.change = change;
			saysYesToRemove=new LinkedList<Integer>();
		}

		public int getOffset() {
			return offset;
		}

		public int getLamportCount() {
			return lamportCount;
		}

		public int getName() {
			return name;
		}

		public int getChange() {
			return change;
		}

		public int getSizeOfSaysYes(){
			return saysYesToRemove.size();
		}
			
		public void insertInsaysYes(int name){
			Integer n=new Integer(name);
			if (!saysYesToRemove.contains(n)){
				saysYesToRemove.add(n);
			}
		}
		
		
		public String toString(){
			return "TEntry navn: "+name+" lamportcount: "+lamportCount+" offset: "+offset+" change: "+change+"sayyes "+saysYesToRemove+"\n";
		}

	}
}