package com.hartenbower.util;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.apache.log4j.Logger;

import scala.Product;

public class CacheUtil {

    final private static boolean DEBUG = false;
    final private static boolean ENABLED = true;
    final static Object NULL = new Object();
    final static String CACHE_THREAD = "cacheThread";
    final static int CACHE_PERIOD_MS = 10000;
    final static Logger log = Logger.getLogger(CacheUtil.class);

    static Map<Class<?>, Map<Method, Map<Product, Object>>> classCachedResults = new HashMap<Class<?>, Map<Method, Map<Product, Object>>>();
    static Map<Class<?>, Map<Method, Map<Product, Object>>> classCachedNullResults = new HashMap<Class<?>, Map<Method, Map<Product, Object>>>();

    public static long hits, misses;

    public static <T> T cache(Object executor, String methodName,
            Product argTuple, long... viabilityOpt) throws Exception {
        Class<?> executorClass = executor instanceof Class ? (Class<?>) executor
                : executor.getClass();
        long viability = -1;
        if (viabilityOpt != null && viabilityOpt.length > 0) {
            viability = viabilityOpt[0];
        }
        Method method = MethodUtil.getLinealMethod(executorClass, methodName,
                TupleUtil.toSignature(argTuple));
        if (void.class == method.getReturnType()) {
            throw new IllegalArgumentException(
                    "can't cache a method returning void");
        }
        Object[] args = TupleUtil.toArray(argTuple);
        if (DEBUG)
            log.debug("around:@Cache " + method);
        if (ENABLED) {
            return (T) cache(method, executor, executorClass, viability,
                    argTuple, args);
        } else {
            return (T) method.invoke(executor, args);
        }
    }

    public static <T> T cache(Method method, Object executor,
            Class<?> executorClass, long viability, Product argTuple,
            Object[] argArray) throws Exception {
        Map<Method, Map<Product, Object>> cachedResults = classCachedResults.get(executorClass);
        if (cachedResults == null) {
            cachedResults = new HashMap<Method, Map<Product, Object>>();
            classCachedResults.put(executorClass, cachedResults);
        }
        Map<Method, Map<Product, Object>> cachedNullResults = classCachedNullResults
                .get(executorClass);
        if (cachedNullResults == null) {
            cachedNullResults = new HashMap<Method, Map<Product, Object>>();
            classCachedNullResults.put(executorClass, cachedNullResults);
        }

        Map<Product, Object> argResultMap = cachedResults.get(method);
        if (argResultMap == null) {
            argResultMap = viability > 0 ? new ExpiringHashMap<Product, Object>(viability)
                    : new ExpiringHashMap<Product, Object>();
            cachedResults.put(method, argResultMap);
        }

        Map<Product, Object> argNullResultMap = cachedNullResults.get(method);
        if (argNullResultMap == null) {
            argNullResultMap = new ExpiringHashMap<Product, Object>(2 * TimingUtil.MINUTE_MS);
            cachedNullResults.put(method, argNullResultMap);
        }

        T result = (T) argResultMap.get(argTuple);
        if (result == null) {
            result = (T) argNullResultMap.get(argTuple);
        }
        if (result == null) {
            misses++;
            result = (T) method.invoke(executor, argArray);
            if (result == null) {
                argNullResultMap.put(argTuple, NULL);
            } else {
                argResultMap.put(argTuple, result);
            }
            if (DEBUG)
                log.debug("around:@Cache " + method + " cached result "
                        + result + " for " + argTuple);
        } else if (result == NULL) {
            result = null;
        } else {
            if (DEBUG)
                log.debug(":@Cache reusing cached result for " + method
                        + " with " + argTuple);
            hits++;
        }
        return result;
    }
    
    public static void flush(Object executor, String methodName) {
        Class<?> executorClass = executor instanceof Class ? (Class<?>) executor
                : executor.getClass();
        Map<Method, Map<Product, Object>> cachedResults = classCachedResults.get(executorClass);
        if (cachedResults != null) {
            cachedResults.clear();
        } else {
            if(DEBUG)
                log.debug("nothing cached for " + executorClass.getSimpleName() + "." + methodName);
        }
        Map<Method, Map<Product, Object>> cachedNullResults = classCachedNullResults
                .get(executorClass);
        if (cachedNullResults != null) {
            cachedNullResults.clear();
        }
    }
}
