package com.hartenbower.util;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.apache.log4j.Logger;

public class ExpiringHashMap<K, V> implements Map<K, V>  {
    private static Logger log = Logger.getLogger(ExpiringHashMap.class);
    private final static boolean DEBUG = false;
    
    public static final long DEFAULT_VIABILITY_MS = 5 * TimingUtil.MINUTE_MS;
    public static final long DEFAULT_CULL_PERIOD_MS = 250;

    private static volatile int cullerCount = 1;

    private final ConcurrentHashMap<K, ExpiringObject> backingMap;

    private final CopyOnWriteArrayList<CullingListener<V>> listeners;

    private final CullingThread culler;  // TODO make this a shared thread; one per map seems heavy

    public ExpiringHashMap() {
        this(DEFAULT_VIABILITY_MS, DEFAULT_CULL_PERIOD_MS);
    }

    public ExpiringHashMap(long viabilityMs) {
        this(viabilityMs, DEFAULT_CULL_PERIOD_MS);
    }

    public ExpiringHashMap(long viabilityMs, long cullingPeriodMs) {
        this(new ConcurrentHashMap<K, ExpiringObject>(),
                new CopyOnWriteArrayList<CullingListener<V>>(), viabilityMs,
                cullingPeriodMs);
    }

    private ExpiringHashMap(ConcurrentHashMap<K, ExpiringObject> backingMap,
            CopyOnWriteArrayList<CullingListener<V>>  listeners,
            long viabilityMs, long cullingPeriodMs) {
        this.backingMap = backingMap;
        this.listeners = listeners;

        culler = new CullingThread();
        culler.setViabilityMs(viabilityMs);
        culler.setCullingPeriodMs(cullingPeriodMs);
        culler.startCulling();
    }

    public V put(K key, V value) {
        ExpiringObject answer = backingMap.put(key, new ExpiringObject(key,
                value, System.currentTimeMillis()));
        if (answer == null) {
            return null;
        }

        return answer.getValue();
    }

    public V get(Object key) {
        ExpiringObject object = backingMap.get(key);

        if (object != null) {
            object.setLastAccessTime(System.currentTimeMillis());

            return object.getValue();
        }

        return null;
    }

    public V remove(Object key) {
        ExpiringObject answer = backingMap.remove(key);
        if (answer == null) {
            return null;
        }

        return answer.getValue();
    }

    public boolean containsKey(Object key) {
        return backingMap.containsKey(key);
    }

    public boolean containsValue(Object value) {
        return backingMap.containsValue(value);
    }

    public int size() {
        return backingMap.size();
    }

    public boolean isEmpty() {
        return backingMap.isEmpty();
    }

    public void clear() {
        backingMap.clear();
    }

    @Override
    public int hashCode() {
        return backingMap.hashCode();
    }

    public Set<K> keySet() {
        return backingMap.keySet();
    }

    @Override
    public boolean equals(Object obj) {
        return backingMap.equals(obj);
    }

    public void putAll(Map<? extends K, ? extends V> inMap) {
        for (Entry<? extends K, ? extends V> e : inMap.entrySet()) {
            this.put(e.getKey(), e.getValue());
        }
    }

    public Collection<V> values() {
        throw new UnsupportedOperationException();
    }

    public Set<Map.Entry<K, V>> entrySet() {
        throw new UnsupportedOperationException();
    }

    public void addCullingListener(CullingListener<V> listener) {
        listeners.add(listener);
    }

    public void removeCullingListener(
            CullingListener<V> listener) {
        listeners.remove(listener);
    }

    public CullingThread getCullingThread() {
        return culler;
    }

    public long getCullingPeriod() {
        return culler.getCullingPeriodMs();
    }

    public long getViabilityMs() {
        return culler.getViabilityMs();
    }

    public void setCullingPeriod(long cullingPeriodMs) {
        culler.setCullingPeriodMs(cullingPeriodMs);
    }

    public void setViabilityMs(long viabilityMs) {
        culler.setViabilityMs(viabilityMs);
    }

    private class ExpiringObject {
        private K key;

        private V value;

        private long lastAccessTime;

        private final ReadWriteLock lastAccessLock = new ReentrantReadWriteLock();

        ExpiringObject(K key, V value, long lastAccessTime) {

            this.key = key;
            this.value = value;
            this.lastAccessTime = lastAccessTime;
        }

        public long getLastAccessTime() {
            lastAccessLock.readLock().lock();

            try {
                return lastAccessTime;
            } finally {
                lastAccessLock.readLock().unlock();
            }
        }

        public void setLastAccessTime(long lastAccessTime) {
            lastAccessLock.writeLock().lock();

            try {
                this.lastAccessTime = lastAccessTime;
            } finally {
                lastAccessLock.writeLock().unlock();
            }
        }

        public K getKey() {
            return key;
        }

        public V getValue() {
            return value;
        }

        @Override
        public boolean equals(Object obj) {
            return value ==  null ? obj == null : value.equals(obj);
        }

        @Override
        public int hashCode() {
            return value ==  null ? 0 : value.hashCode();
        }
        
    }
    
    public static interface CullingListener<E> {
        void culled(E victim);
    }
    
    public class CullingThread implements Runnable {
        private final ReadWriteLock lock = new ReentrantReadWriteLock();

        private long viabilityMs;

        private long cullingPeriodMs;

        private boolean running = false;

        private final Thread cullingThread;

        public CullingThread() {
            
            cullingThread = new Thread(this, "ExpiringHashMapCullingThread-"
                    + cullerCount++);
            if(DEBUG)log.info("created " + this);
            cullingThread.setDaemon(true);
        }

        public void run() {
            if(DEBUG)log.info(this + " is running");
            while (running) {
                cull();

                try {
                    Thread.sleep(cullingPeriodMs);
                } catch (InterruptedException e) {}
            }
        }

        private void cull() {
            long timeNow = System.currentTimeMillis();

            for (ExpiringObject o : backingMap.values()) {
                if(DEBUG)log.info(this + " considering " + o);
                if (viabilityMs <= 0) {
                    if(DEBUG)log.info("set to never exp");
                    continue;
                }

                long timeIdle = timeNow - o.getLastAccessTime();

                if (timeIdle >= viabilityMs) {
                    if(DEBUG)log.info(this + " culling " + o);
                    backingMap.remove(o.getKey());
                    
                    for (CullingListener<V> listener : listeners) {
                        listener.culled(o.getValue());
                    }
                } else {
                    if(DEBUG)log.info(this + " still viable " + o);
                }
            }
        }

        public void startCulling() {
            lock.writeLock().lock();

            try {
                if (!running) {
                    running = true;
                    cullingThread.start();
                }
            } finally {
                lock.writeLock().unlock();
            }
        }

        public void startIfNotStarted() {
            lock.readLock().lock();
            try {
                if (running) {
                    return;
                }
            } finally {
                lock.readLock().unlock();
            }

            lock.writeLock().lock();
            try {
                if (!running) {
                    running = true;
                    cullingThread.start();
                }
            } finally {
                lock.writeLock().unlock();
            }
        }

        public void stopCulling() {
            lock.writeLock().lock();

            try {
                if (running) {
                    running = false;
                    cullingThread.interrupt();
                }
            } finally {
                lock.writeLock().unlock();
            }
        }

        public boolean isRunning() {
            lock.readLock().lock();

            try {
                return running;
            } finally {
                lock.readLock().unlock();
            }
        }

        public long getViabilityMs() {
            lock.readLock().lock();

            try {
                return viabilityMs;
            } finally {
                lock.readLock().unlock();
            }
        }

        public void setViabilityMs(long viabilityMs) {
            lock.writeLock().lock();

            try {
                this.viabilityMs = viabilityMs;
            } finally {
                lock.writeLock().unlock();
            }
        }

        /**
         * Get the interval in which an object will live in the map before
         * it is removed.
         *
         * @return
         *  The time in seconds.
         */
        public long getCullingPeriodMs() {
            lock.readLock().lock();

            try {
                return cullingPeriodMs;
            } finally {
                lock.readLock().unlock();
            }
        }

        /**
         * Set the interval in which an object will live in the map before
         * it is removed.
         *
         * @param cullingPeriodMs
         *  The time in seconds
         */
        public void setCullingPeriodMs(long cullingPeriodMs) {
            lock.writeLock().lock();

            try {
                this.cullingPeriodMs = cullingPeriodMs;
            } finally {
                lock.writeLock().unlock();
            }
        }
     
        @Override
        public String toString() {
            return cullingThread + " [" + viabilityMs + "/" + cullingPeriodMs + "]";
        }

    }
}
