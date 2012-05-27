package com.hartenbower.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import scala.Tuple3;

import com.hartenbower.annotations.EpicMethod;
import com.hartenbower.util.NumberFormats;
import com.hartenbower.util.RuntimeSwitches;
import static com.hartenbower.util.NumberFormats.decimal3;

/**
 * Support class for profiling, works with Profiling.aj aspect
 * <p/>
 */
public class ProfilingMaps {
    public final static boolean DEBUG = false;

    // if profiling is enabled, keep track of total time spent in methods
    public static boolean AGGREGATE_METHOD_TIMES = true;

    public ProfilingMaps() {
    }

    final static long BlockedMethodCandidateTimeoutMs = 2000;
    final static int RUNNING_MAX_COUNT = 200;
    final static int MAX_COUNT = 700;
    
    final static String BLOCKED_METHOD_THREAD = "BlockedMethodThread";
    public static Map<String, Tuple3<Long, Long, Long>> CachableMethodProfiles = new HashMap<String, Tuple3<Long, Long, Long>>();

    static Thread BlockedMethodThread = new Thread(BLOCKED_METHOD_THREAD) {

        @EpicMethod
        public void run() {
            int counter = 1;
            try {
                StringBuffer sb = new StringBuffer();
                boolean foundBlocked = false;
                while (true) {
                    sleep(BlockedMethodCandidateTimeoutMs);
                    sb.setLength(0);
                    sb.append("\n\nblocked?\n");
                    long now = System.currentTimeMillis();
                    try {
                        if (deltaMap != null && deltaMap.keySet() != null) {
                            foundBlocked = false;
                            for (String sig : deltaMap.keySet()) {
                                List<Long> deltas = deltaMap.get(sig);
                                if (sig != null && deltas != null
                                        && !deltas.isEmpty()) {
                                    for (Long called : deltas) {
                                        if (!sig.startsWith(EpicMethod.class
                                                .getName())
                                                && called != null
                                                && now - called > (Util.SIGMA * 1000)) {
                                            foundBlocked = true;
                                            sb.append(sig);
                                            sb.append(": ");
                                            sb.append(TimingUtil
                                                    .fromSeconds((now - called) / 1000.));
                                            sb.append("\n");
                                        }
                                    }
                                }
                            }
                            sb.append("\n\n");
                        }
                    } catch (ConcurrentModificationException cme) {
                        Dbg.o("got a ConncurrentModificationException, ignoring ");
                    } catch (NullPointerException npe) {
                        if (DEBUG) {
                            npe.printStackTrace();
                        }
                    } catch (Exception other) {
                    	other.printStackTrace();
                    }
                }
            } catch (InterruptedException e) {
            }
        }
    };

    static Map<String, List<Long>> deltaMap;
    static {
        BlockedMethodThread.setDaemon(true);
        deltaMap = new HashMap<String, List<Long>>();
        int activeCount = Thread.activeCount();
        Thread[] threads = new Thread[activeCount];
        Thread.enumerate(threads);
        for (Thread t : threads) {
            if (BLOCKED_METHOD_THREAD.equals(t.getName())) {
                try {
                    t.interrupt();
                } catch (Exception e) {
                    e.printStackTrace();
                }
                Dbg.info("found exisitng BockedMethodThread, replacing");
                break;
            }
        }

        BlockedMethodThread.start();
        Util.startMemwatcher();
    }

    public static Map<String, List<Long>> getDeltaMap() {
        return deltaMap;
    }

    static Map<String, Double> aggregateMap = new HashMap<String, Double>();

    public static Map<String, Double> getAggregateMap() {
        return aggregateMap;
    }

    static Map<String, Long> counterMap = new HashMap<String, Long>();

    public static Map<String, Long> getCounterMap() {
        return counterMap;
    }

    static Map<String, Long> initializerMap = new HashMap<String, Long>();

    public static Map<String, Long> getInitializerMap() {
        return initializerMap;
    }

    public void finalize() {
        BlockedMethodThread.interrupt();
    }

    // comparable so method list sortable by execution time
    static class MethodExecutionProfile implements
            Comparable<MethodExecutionProfile> {
        private String signature;

        private Double time = 0.;

        private long counter = 0l;

        private long initializerCount = 0l;

        public MethodExecutionProfile(String signature, Double time,
                Long counter) {
            this.signature = signature;
            this.time = time;
            this.counter = counter;
        }

        public MethodExecutionProfile(String signature, Long initializerCount) {
            this.signature = signature;
            this.initializerCount = initializerCount;
        }

        public String getSignature() {
            return signature;
        }

        public void setSignature(String signature) {
            this.signature = signature;
        }

        public Double getTime() {
            return time;
        }

        public void setTime(Double time) {
            this.time = time;
        }

        public int compareTo(MethodExecutionProfile other) {
            if (other == null) {
                return Util.COMPARED_TO_NULL;
            }
            int ret = time.compareTo(other.getTime());
            if (ret == 0) {
                ret = (int) ( initializerCount - other.getInitializerCount());
                if (ret == 0) {
                    ret = signature.compareTo(other.getSignature());
                }
            }
            return -ret;
        }

        @Override
        public String toString() {
            if (counter > 1) {
                return signature + "\t" + counter + "\t"
                        + TimingUtil.fromSeconds(time) + "\t"
                        + decimal3.format(time / counter) ;
            } else if (initializerCount > 1) {
                return signature + "\t" + initializerCount + "\t NA \t NA";
            } else {
                return signature + "\t1\t" + TimingUtil.fromSeconds(time) + "\t" + TimingUtil.fromSeconds(time);
            }
        }

        public Long getCounter() {
            return counter;
        }

        public void setCounter(Long counter) {
            this.counter = counter;
        }

        public Long getInitializerCount() {
            return initializerCount;
        }

        public void setInitializerCount(Long initializerCount) {
            this.initializerCount = initializerCount;
        }
    }

    public static void resetTop100() {
        aggregateMap.clear();
        counterMap.clear();
        initializerMap.clear();
    }

    public static void printTopX(int maxCount) {
        Dbg.o(getTopX(maxCount));
    }

    public static String getTopXHtml(int maxCount) {
        String topX = getTopX(maxCount);
        
        StringBuilder sb = new StringBuilder("<h1>Top " + maxCount + " time-consuming invocations</h1>");
        sb.append("<table width=\"100%\">\n");
        BufferedReader br = new BufferedReader(new StringReader(topX));
        String s;
        int i = 0;
        String[] bgs = {"#EEEEFF","#FFFFFF"};        	
        	
        sb.append("<tr><th>Method</th><th>Times Called</th><th>Total Time</th><th>Time/Call</th></tr>\n");
        try {
	        while ( (s = br.readLine()) != null) {
	        	String[] parts = s.split("\t");
	        	if(parts.length < 4) {
	        		Dbg.o("bad line: " + s);
	        		continue;
	        	}
	        	sb.append("<tr bgcolor=\"" + bgs[i++ % 2] + "\">" );
	        	sb.append("<td>");
	        	sb.append(parts[0]);
	        	sb.append("</td><td>");
	        	sb.append(parts[1]);
	        	sb.append("</td><td>");
	        	sb.append(parts[2]);
	        	sb.append("</td><td>");
	        	sb.append(parts[3]);
	        	sb.append("</td></tr>\n");
	        }
        } catch (Exception e) {
	        e.printStackTrace();
        }
        sb.append("</table>");
        return sb.toString();
    }
    public static String getTop100Html() {
        return getTopXHtml(MAX_COUNT);
    }
    
    public static String getTopX(int maxCount) {
        Set<MethodExecutionProfile> topScores = new TreeSet<MethodExecutionProfile>();
        Map<String, Double> aggregateTimes = getAggregateMap();
        Map<String, Long> counters = getCounterMap();
        Double test;
        Long count;
        for (String methodShortSig : new ArrayList<String>(aggregateTimes.keySet())) {
            if (methodShortSig.startsWith(EpicMethod.class.getName())) {
                continue;
            }
            test = aggregateTimes.get(methodShortSig);
            count = counters.get(methodShortSig);
            topScores.add(new MethodExecutionProfile(methodShortSig, test,
                    count));
        }
        StringBuffer sb = new StringBuffer();
        //sb.append("Top time-consuming methods:\n\n");
        //sb.append("map sizes: aggTimes " + aggregateTimes.size() + ", counters " + counters.size() + "\n\n");  
        //sb.append("caching:  hits " + CacheUtil.hits + "; misses " + CacheUtil.misses);
        int howMany = 0;
        for (MethodExecutionProfile topScoreMethod : topScores) {
            howMany++;
            sb.append(topScoreMethod.toString());
            sb.append("\n");
            if (howMany > maxCount) {
                break;
            }
        }
        for (String method : new ArrayList<String>(CachableMethodProfiles.keySet())) {
            Tuple3<Long, Long, Long> tuple = CachableMethodProfiles.get(method);
            long CachableMethodAttempts = tuple._1();
            long CachableMethodHits = tuple._2();
            long CachableMethodMisses = tuple._3();
            sb.append(method + ":\n");
            sb.append("\tCachableMethodAttempts " + CachableMethodAttempts
                    + "\n");
            sb.append("\tCachableMethodHits " + CachableMethodHits + "\n");
            sb.append("\tCachableMethodMisses " + CachableMethodMisses + "\n");
            sb.append("\t\th to a "
                    + (CachableMethodHits / (CachableMethodAttempts * 1.))
                    + "\n");
            sb.append("\t\th to m "
                    + (CachableMethodHits / (CachableMethodMisses * 1.)) + "\n");
        }
        return sb.toString();
    }

    public static String getTopXInits(int maxCount) {
        Set<MethodExecutionProfile> topScores = new TreeSet<MethodExecutionProfile>();
        Long count;
        for (String methodShortSig : new ArrayList<String>(initializerMap.keySet())) {
            count = initializerMap.get(methodShortSig);
            topScores.add(new MethodExecutionProfile(methodShortSig, count));
        }
        StringBuffer sb = new StringBuffer();
        sb.append("Top 100 initializer-calling methods:\n\n");
        sb.append("map sizes: initializerMap " + initializerMap.size()+ "\n\n");  
        sb.append("@Cache and CacheUtil.cache(..) hits: " + CacheUtil.hits 
                + "; misses " + CacheUtil.misses + 
                " ratio " + NumberFormats.decimal3(1. * CacheUtil.hits / CacheUtil.misses) + "\n\n");
        
        int howMany = 0;
        for (MethodExecutionProfile topScoreMethod : topScores) {
            howMany++;
            sb.append("\t");
            sb.append(topScoreMethod.toString());
            sb.append("\n");
            if (howMany > maxCount) {
                break;
            }
        }
        return sb.toString();
    }

}