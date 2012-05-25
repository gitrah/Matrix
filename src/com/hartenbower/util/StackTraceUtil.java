package com.hartenbower.util;

import java.io.*;

/**
 */
public final class StackTraceUtil {

    /**
     * Gets the stack trace.
     * 
     */
    public static String getStackTrace(Throwable aThrowable) {
        final Writer result = new StringWriter();
        final PrintWriter printWriter = new PrintWriter(result);
        aThrowable.printStackTrace(printWriter);
        return result.toString();
    }

    /**
         */
    public static String getCustomStackTrace(Throwable aThrowable) {
        // add the class name and any message passed to constructor
        final StringBuilder result = new StringBuilder("BOO-BOO: ");
        result.append(aThrowable.toString());
        final String NEW_LINE = System.getProperty("line.separator");
        result.append(NEW_LINE);

        // add each element of the stack trace
        for (StackTraceElement element : aThrowable.getStackTrace()) {
            result.append(element);
            result.append(NEW_LINE);
        }
        return result.toString();
    }

    /**
     * @return true if in the cflow of a junit or testng test
     */
    public static boolean inTestCFlow() {
        try {
            for (StackTraceElement stm : Thread.currentThread().getStackTrace()) {
                if (stm.getClassName().indexOf("org.junit") != -1
                        || stm.getClassName().indexOf("org.testng") != -1) {
                    return true;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static String dumpStack(int... levelsDeep) {
        try {
            StringBuffer sb = new StringBuffer();
            StackTraceElement[] trace = Thread.currentThread().getStackTrace();
            for (int i = 2; i < trace.length
                    && (levelsDeep.length == 0 || i < levelsDeep[0]); i++) {
                sb.append(trace[i].toString());
                sb.append("\n");
            }
            return (sb.toString());
        } catch (Exception e) {
            e.printStackTrace();
            return e.getMessage();
        }
    }

    public static String dumpStackRange(int start, int stop) {
        try {
            StringBuffer sb = new StringBuffer();
            StackTraceElement[] trace = Thread.currentThread().getStackTrace();
            for (int i = start; i < trace.length && i <= stop; i++) {
                sb.append(trace[i].toString());
                if (i < trace.length - 1 && i < stop) {
                    sb.append("\n");
                }
            }
            return (sb.toString());
        } catch (Exception e) {
            e.printStackTrace();
            return e.getMessage();
        }
    }

    public static String caller() {
        return dumpStackRange(5, 5);
    }
}
