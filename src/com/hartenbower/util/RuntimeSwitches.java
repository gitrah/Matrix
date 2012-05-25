package com.hartenbower.util;

public class RuntimeSwitches {
    public final static String PROFILING_SWITCH = "profiling";
    public static boolean PROFILING = "true".equals(System.getProperty(PROFILING_SWITCH)); 
    
    public final static String TRACEALL_SWITCH = "traceall";
    public static boolean TRACEALL = "true".equals(System.getProperty(TRACEALL_SWITCH)); 
    public final static String TRACEANNOS_SWITCH = "traceannos";
    public static boolean TRACEANNOS = "true".equals(System.getProperty(TRACEANNOS_SWITCH)); 
    public final static String TRACEHTTP_SWITCH = "tracehttp";
    public static boolean TRACEHTTP = "true".equals(System.getProperty(TRACEHTTP_SWITCH)); 
    public static String TRACEPREFIX = null; 
    public final static String OUTPUT_PROFILE_AFTER_TEST_SWITCH = "profiletests";
    public static boolean OUTPUT_PROFILE_AFTER_TEST = "true".equals(System.getProperty(OUTPUT_PROFILE_AFTER_TEST_SWITCH)); 
    public final static String TRACE_SESSION_CALLS_SWITCH = "tracesession";
    public static boolean TRACE_SESSION_CALLS = "true".equals(System.getProperty(TRACE_SESSION_CALLS_SWITCH)); 
    public final static String TRACE_TX_CALLS_SWITCH = "tracetx";
    public static boolean TRACE_TX_CALLS = "true".equals(System.getProperty(TRACE_TX_CALLS_SWITCH)); 
    public final static String AUTOFLUSH_SWITCH = "autoflush";
    public static boolean AUTOFLUSH = "true".equals(System.getProperty(AUTOFLUSH_SWITCH)); 
}
