package com.hartenbower.util;

public class ThreadingUtil {

	public static String dumpThreads() {
		StringBuilder sb = new StringBuilder("\n");
		ThreadGroup tg, parent = Thread.currentThread().getThreadGroup();
		while( (tg = parent.getParent()) != null) {
			parent = tg;
		}
		int size = 2 * parent.activeGroupCount();
		ThreadGroup[] list = new ThreadGroup[size];
		int actual = parent.enumerate(list,true);
		//System.out.print("found " + list);
		sb.append("tgs: " + actual + "\n");
		if(list != null && list.length > 0) {
			for(ThreadGroup g: list) { 
				if(g == null) {
					continue;
				}
				sb.append("   " + g.getName() + ": " + g.activeCount()+"\n");
				Thread[] threads = new Thread[g.activeCount()];
				g.enumerate(threads, false);
				for(Thread t: threads) {
					if(t != null) {
						sb.append("     " + t.getName()+"\n");
					}
				}
			}
		} else {
			System.out.println("empty thread group list");
		}
		return sb.toString() + "\n";
	}
}
