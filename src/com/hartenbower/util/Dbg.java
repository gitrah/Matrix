package com.hartenbower.util;

import static com.hartenbower.util.NumberFormats.decimal3;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Map;

import org.apache.log4j.Logger;

public class Dbg {

    public static boolean TRACE_HIBERNATE_SESSION = false;
    public static DateFormat datef = new SimpleDateFormat("MM-dd-yy");
    public static boolean ADD_TIMESTAMP = false;
    public static DateFormat timestampf = new SimpleDateFormat(
            "MM-dd-yy HH:mm:ss");
    static Logger log = Logger.getLogger(Dbg.class.getSimpleName());

    /**
     * @param object
     */
    public static void dumpObject(Object object) {
        Class baseClass = (object instanceof Class) ? ((Class) object) : object
                .getClass();
        if (object != null) {
            o("" + object + " is:");
            for (Field theField : baseClass.getDeclaredFields()) {
                o("   field: " + theField.getName());
            }
            for (Method theMethod : baseClass.getDeclaredMethods()) {
                o("   method: " + theMethod.toGenericString());
            }
            for (Annotation anno : baseClass.getAnnotations()) {
                o("   anno: " + anno);
            }
            for (Class theClass : baseClass.getDeclaredClasses()) {
                o("   class: " + theClass.getName());
            }
            for (Class theClass : baseClass.getInterfaces()) {
                o("   i/f: " + theClass.getName());
            }
        }
    }

    /**
     * print object's field name/values
     * 
     * @param object
     */
    public static void dumpFields(Object object) {
        Class baseClass = (object instanceof Class) ? ((Class) object) : object
                .getClass();
        o("fields of " + object + " (" + object.getClass() + ")");
        for (Field theField : FieldUtil.getLinealFields(baseClass)) {
            theField.setAccessible(true);
            try {
                o(theField.getName() + ": " + theField.get(object));
            } catch (IllegalArgumentException e) {
                // e.printStackTrace();
            } catch (IllegalAccessException e) {
                // e.printStackTrace();
            }

        }
    }

    /**
     * Print contents of is
     * 
     * @param is
     * @return
     */
    public static String dumpOutput(InputStream is) {
        String output = "";
        try {
            byte[] buffer = new byte[1600];
            while (is.read(buffer) != -1) {
                output = new String(buffer);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return output;
    }

    /**
     * Free memory info
     */
    public static void dumpMemStats() {
        // o("Util.dumpMemStats()");
        double freeMBs = Runtime.getRuntime().freeMemory() / 1000000.;
        info("([" + decimal3.format(freeMBs) + "MB ");
    }

    /**
     * Returns formatted stacktrace
     */
    public static String oS(Exception e) {
        if (e == null) {
            return null;
        }
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        return sw.toString();
    }

    /**
     * Returns formatted stacktrace to depth
     */
    public static String oS(Exception e, int depth) {
        if (e == null) {
            return null;
        }
        StringBuffer sb = new StringBuffer();
        int curr = 0;
        for (StackTraceElement ste : e.getStackTrace()) {
            sb.append("\tat ");
            sb.append(ste.toString());
            sb.append("\n");
            curr++;
            if (curr >= depth) {
                break;
            }
        }
        return sb.toString();
    }

    /**
     * Returns formatted String of Collection
     */
    public static <T> String oS(Collection<T> c) {
        return Util.toString(c);
    }

    /**
     * Returns formatted String of Collection, with specified object not
     * included.
     */
    public static <T> String oS(Collection<T> c, T except) {
        return Util.toString(c, except);
    }

    /****************************************************************************
     * Returns formatted String of Map
     */
    public static <K, V> String oS(Map<K, V> m) {
        return m != null ? Util.toString(m.entrySet()) : "null"; 
    }

    /****************************************************************************
     * Returns a String-formatted Collection surrounded by parenthesis.
     */
    public static <T> String oSInParens(Collection<T> c) {
        if (c.isEmpty()) {
            throw new IllegalArgumentException(
                    "oSInParens called on empty list");
        }
        StringBuffer sb = new StringBuffer("(");
        for (Iterator<T> i = c.iterator(); i.hasNext();) {
            sb.append(i.next());
            if (i.hasNext()) {
                sb.append(", ");
            }
        }
        sb.append(")");
        return sb.toString();
    }

    public static <T> String oS(Collection<T> c, String... fieldNames) {
        return Util.toString(c, fieldNames);
    }

    public static String oSA(int[] a) {
        StringBuffer sb = new StringBuffer("{");
        int count = 0;
        for (int o : a) {
            sb.append(o);
            if (count++ < a.length - 1) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }
    
    public static String oSAln(Object[] a) {
        StringBuffer sb = new StringBuffer("{");
        int count = 0;
        for (Object o : a) {
            sb.append(o);
            if (count++ < a.length - 1) {
                sb.append("\n");
            }
        }
        sb.append("}");
        return sb.toString();
    }


    public static String oSA(double[] a) {
        StringBuffer sb = new StringBuffer("{");
        int count = 0;
        for (double o : a) {
            sb.append(o);
            if (count++ < a.length - 1) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }

    public static String oSA(Object[] a) {
        StringBuffer sb = new StringBuffer("{");
        int count = 0;
        for (Object o : a) {
            sb.append(o);
            if (count++ < a.length - 1) {
                sb.append(", ");
            }
        }
        sb.append("}");
        return sb.toString();
    }

    /****************************************************************************
     * Logs INFO-level message using log4j pattern.
     */
    public static void o(Object obj) {
        System.out.println(obj);
    }

    /****************************************************************************
     * Logs message and elapsed time since ltime. Returns currentTime.
     */
    public static long o(String msg, long ltime) {
        long curTime = System.currentTimeMillis();
        o(msg + " took " + TimingUtil.fromSeconds((curTime - ltime) / 1000.));
        return curTime;
    }

    /****************************************************************************
     * Logs empty INFO-level message using log4j pattern,.
     */
    public static void info() {
        info("");
    }

    /****************************************************************************
     * Logs INFO-level message using log4j pattern,.
     */
    public static void info(Object obj) {
        log.info("\n" + obj);
    }

    public static void infoCaller(Object obj) {
        log.info("\n" + StackTraceUtil.caller() + ": " + obj);
    }

    /****************************************************************************
     * Logs INFO-level message and exception trace, using log4j pattern.
     */
    public static void info(Object obj, Throwable t) {
        log.info("\n" + StackTraceUtil.caller() + ": " + obj + " -> " + t);
    }

    /****************************************************************************
     * Logs message and elapsed time since elapseMillis. Returns currentTime.
     */
    public static long out(String msg, long elapsedMillis) {
        long now = System.currentTimeMillis();
        info(msg + " took "
                + TimingUtil.fromSeconds(((now - elapsedMillis) / 1000.)));
        return now;
    }

    /****************************************************************************
     * out plus stack trace to depth
     * 
     * @param o
     * @param depth
     * @param ignoreInvokeMethods
     */
    public static void outDepth(Object o, int depth, boolean ignoreInvokeMethods) {
        Throwable t = new Throwable();

        StackTraceElement[] els = t.getStackTrace();
        if (els.length > 3) {
            StringBuffer sb = new StringBuffer("\n");
            String className = els[2].getClassName();
            String methName = els[2].getMethodName();
            int lineNo = els[1].getLineNumber();
            if (methName.indexOf("_aroundBody") != -1) {
                methName = methName.substring(0,
                        methName.indexOf("_aroundBody"));
            }
            sb.append(StringUtil.dePackage(className) + "." + methName + "(#: "
                    + lineNo + "):  " + (o == null ? "null" : o.toString())
                    + "\n");
            depth++;
            for (int i = 3; i < els.length && i < depth; i++) {
                className = els[i].getClassName();
                methName = els[i].getMethodName();
                lineNo = els[i].getLineNumber();
                if (methName.indexOf("_aroundBody") != -1) {
                    methName = methName.substring(0,
                            methName.indexOf("_aroundBody"));
                }
                if (ignoreInvokeMethods
                        && (methName.startsWith("invoke")
                                || methName.indexOf("Invoke") != -1
                                || methName.indexOf("Filter") != -1 || methName
                                .startsWith("proceed"))) {
                    continue;
                }
                sb.append("\tfrom " + StringUtil.dePackage(className) + "."
                        + methName + "(#: " + lineNo + ")\n");
            }
            info(sb);
        }
    }

    /****************************************************************************
     * logs INFO-level message of formatted Iterator, using log4j pattern.
     */
    public static <T> void out(Iterator<? extends T> i) {
        info(oS(i));
    }

    /****************************************************************************
     * Logs INFO-level message of formatted Enumeration.
     */
    public static <T> void out(Enumeration<? extends T> e) {
        info(oS(e));
    }

    /****************************************************************************
     * Logs INFO-level message of formatted Collection.
     */
    public static <T> void out(Collection<T> c) {
        if (c.isEmpty()) {
            outDepth("collection was empty", 6);
        }
        info("{ " + Util.toString(c) + "}");
    }

    /****************************************************************************
     * Logs INFO-level message of formatted array.
     */
    public static <T> void out(T[] c) {
        if (c == null || c.length == 0) {
            outDepth("array was empty", 6);
        }
        info("{ " + Util.toString(c) + "}");
    }

    public static void debug(Object obj, Throwable t) {
        log.debug(obj + " -> " + t);
    }

    public static void debug(Object obj) {
        log.debug("" + obj);
    }

    public static void warn(Object obj, Throwable t) {
        log.warn(obj + " -> " + t);
    }

    public static void warn(Object obj) {
        log.warn("" + obj);
    }

    public static void error(Object obj, Throwable t) {
        log.error(obj + " -> " + t);
    }

    public static void error(Object obj) {
        log.error("" + obj);
    }

    /**
     * Log to file (append)
     * 
     * @param fileName
     * @param o
     */
    public static void aF(String fileName, Object o) {
        try {
            FileWriter f = new FileWriter(new File(fileName), true);
            String s = o.toString();
            if (s != null && !s.endsWith("\n")) {
                s += "\n";
            }
            f.append(s);
            f.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void outDepth(Object o, int depth) {
        outDepth(o, depth, true);
    }

    /****************************************************************************
     * Returns message and elapsed time since ltime.
     */
    public static String oS(String msg, long ltime) {
        return msg
                + " took "
                + TimingUtil
                        .fromSeconds((System.currentTimeMillis() - ltime) / 1000.);
    }

    /****************************************************************************
     * Returns formatted String of supplied Iterator.
     */
    public static <T> String oS(Iterator<? extends T> i) {
        StringBuffer sb = new StringBuffer("{");
        while (i.hasNext()) {
            sb.append(i.next());
            if (i.hasNext()) {
                sb.append(", ");
            }
        }
        sb.append("}\n");
        return sb.toString();
    }

    /****************************************************************************
     * Return formatted String of supplied Enumeration.
     */
    public static <T> String oS(Enumeration<? extends T> e) {
        StringBuffer sb = new StringBuffer("{");
        while (e.hasMoreElements()) {
            sb.append(e.nextElement());
            if (e.hasMoreElements()) {
                sb.append(", ");
            }
        }
        sb.append("}\n");
        return sb.toString();
    }

    /**
     * Assert
     * 
     * @param expression
     */
    public static void a(boolean expression) {
        if (expression) {
            return;
        }
        outDepth("\n\nASSERTION FAILED!!!\n\n", 5);
        // throw new AssertionError();
    }

    /**
     * Assert with context
     * 
     * @param expression
     * @param ctx
     */
    public static void a(boolean expression, Object ctx) {
        if (expression) {
            return;
        }
        outDepth("\n\nASSERTION FAILED: " + ctx + "\n\n", 5);

        // throw new AssertionError(ctx);
    }

    /**
     * Print enclosing StackTraceElement
     */
    public static void caller() {
        StackTraceElement[] stes = Thread.currentThread().getStackTrace();
        if (stes != null && stes.length > 3) {
            info("caller:  " + stes[3].getClassName() + "."
                    + stes[3].getMethodName() + "(# " + stes[3].getLineNumber()
                    + ")");
        }
    }

    /**
     * Print enclosing StackTraceElements to level
     * 
     * @param level
     */
    public static void caller(int level) {
        StackTraceElement[] stes = Thread.currentThread().getStackTrace();
        if (stes != null && stes.length > 2) {
            for (int i = 2; i < stes.length && i < level + 2; i++) {
                info("caller:  " + stes[i].getClassName() + "."
                        + stes[i].getMethodName() + "(# "
                        + stes[i].getLineNumber() + ")");
            }
        }
    }

    public static String callerS(int level) {
        StackTraceElement[] stes = Thread.currentThread().getStackTrace();
        StringBuffer sb = new StringBuffer();
        if (stes != null && stes.length > 3) {
            for (int i = 3; i < stes.length && i < level + 3; i++) {
                sb.append("caller:  " + stes[i].getClassName() + "."
                        + stes[i].getMethodName() + "(# "
                        + stes[i].getLineNumber() + ")\n");
            }
        }
        return sb.toString();
    }

    public static String iS(Object o) {
        if (o != null) {
            try {
                Method m = MethodUtil.getIdAccessor(o);
                return (o + " id ( " + m.invoke(o, Util.VOID_ARG) + " )");
            } catch (Exception e) {
                error("Failed to getIdAccessor for object: " + o, e);
            }
        }
        return "null";
    }
}
