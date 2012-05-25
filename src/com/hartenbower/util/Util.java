package com.hartenbower.util;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.io.PrintWriter;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import org.apache.log4j.Logger;

public class Util {
	public final static String MEM_THREAD = "memThread";
	public static Thread memThread = null;
	public static NumberFormat memnf = NumberFormat.getNumberInstance();
	final static long MEM_PERIOD_MS = 10 * TimingUtil.SECOND_MS;

	static Logger log = Logger.getLogger(Util.class);

	public static double SIGMA = 10;
	public final static int COMPARED_TO_NULL = 1;

	public final static Object[] VOID_ARG = new Object[] {};

	public final static Class<?>[] VOID_SIG = new Class[] {};

	public static <T> String toString(Collection<T> c) {
		return toString(c, (String[]) null);
	}

	public static <T> String toString(Collection<T> c, T except) {
		return toString(null, null, "{", "}", ", ", c, except, (String[]) null);
	}

	public static <T> String toString(String open, String close,
	        String separator, Collection<T> c) {
		return toString(null, null, open, close, separator, c, null,
		        (String[]) null);
	}

	public static <T> String toString(Collection<T> c, String... fieldNames) {
		return toString(null, null, "{", "}", ", ", c, null, fieldNames);
	}

	public static <T> String toString(NumberFormat nf, DateFormat df,
	        String open, String close, String separator, Collection<T> c,
	        T except, String... fieldNames) {

		StringBuffer sb = new StringBuffer(open);
		try {
			T entity;
			Field f = null;
			Method m = null;
			Class<?> entityClass = null;
			for (Iterator<T> i = c.iterator(); i.hasNext();) {
				entity = i.next();
				if (except != null && except.equals(entity)) {
					continue;
				}
				if (fieldNames != null) {
					for (String fieldName : fieldNames) {
						if (entityClass == null) {
							entityClass = entity.getClass();
						}
						if ("toString".equals(fieldName)) {
							sb.append(entity.toString());
							sb.append("\t");
							continue;
						} else if (fieldName.startsWith("##")) {
							sb.append(fieldName.substring(2));
							sb.append("\t");
							continue;
						}
						try {
							f = FieldUtil
							        .getLinealField(entityClass, fieldName);
							f.setAccessible(true);
						} catch (Exception e) {
							Dbg.caller();
							log.warn(e.getClass().getName());
						}
						if (f != null) {
							try {
								Object o = f.get(entity);
								if (o != null) {
									if ((o instanceof Double || o instanceof Float)
									        && nf != null) {
										sb.append(nf.format(o));
									} else if (o instanceof Date && df != null) {
										sb.append(df.format(o));
									} else {
										sb.append(o);
									}
									sb.append("\t");
								}
							} catch (Exception e) {
								e.printStackTrace();
							}
						} else {
							try {
								m = MethodUtil.getLinealMethod(entityClass,
								        fieldName, Util.VOID_SIG);
							} catch (Exception e) {
								log.warn(e.getMessage());
							}
							if (m != null) {
								Object o = m.invoke(entity, Util.VOID_ARG);
								if (o != null) {
									sb.append(o);
									sb.append("\t");
								}
							}
						}
					}
					sb.append(close);
				} else {
					if (entity instanceof Map.Entry) {
						Map.Entry me = (Map.Entry) entity;
						sb.append(me.getKey() + " -> " + me.getValue());
					} else {
						sb.append(entity);
					}
				}
				if (i.hasNext()) {
					sb.append(separator);
				}
			}
		} catch (Exception lie) {
			sb.append(lie);
		} finally {
			sb.append(close);
		}
		return sb.toString();
	}

	public static String toString(InputStream is) {
		StringBuffer logBuffer = new StringBuffer();
		StringBuffer lineBuffer = new StringBuffer();
		BufferedInputStream bis = new BufferedInputStream(is);
		try {
			int ichr;
			while ((ichr = bis.read()) != -1) {
				if ('\n' != ichr) {
					lineBuffer.append((char) ichr);
				} else {
					String line = lineBuffer.toString();
					lineBuffer.setLength(0);
					if (line.indexOf("ERROR") != -1) {
						logBuffer.append("<font color='yellow'>");
						logBuffer.append(line);
						logBuffer.append("</font>");
					} else if (line.indexOf("[STDOUT]") != -1) {
						if (line.indexOf("at com.") != -1
						        || line.indexOf("at java") != -1
						        || line.indexOf("at net.") != -1
						        || line.indexOf("at org.") != -1) {
							logBuffer.append("<font color='red'>");
							logBuffer.append(line);
							logBuffer.append("</font>");
						} else {
							logBuffer.append("<font color='black'>");
							logBuffer.append(line);
							logBuffer.append("</font>");
						}
					} else {
						logBuffer.append("<font color='white'>");
						logBuffer.append(line);
						logBuffer.append("</font>");
					}
					logBuffer.append("<br/>\n");
				}
			}
			bis.close();
			return logBuffer.toString();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return "";
	}

	public static void toString(InputStream is, PrintWriter writer) {
		BufferedInputStream bis = new BufferedInputStream(is);
		try {
			int ichr;
			while ((ichr = bis.read()) != -1) {
				if ('\n' != ichr) {
					writer.append((char) ichr);
				} else {
					writer.append("<br/>\n");
				}
			}
			bis.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static String toString(int[] a) {
		StringBuffer sb = new StringBuffer();
		int count = 0;
		for (int o : a) {
			sb.append(o);
			if (count++ < a.length - 1) {
				sb.append(" ");
			}
		}
		return sb.toString();
	}

	public static <T> String toString(T[] c) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < c.length; i++) {
			sb.append(c[i]);
			if (i < c.length - 1) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}

	public static boolean isMemwatcherStopped() {
		return memThread == null || memThread.isInterrupted();
	}

	static int counter = 0;
	static String lastTD = "";

	public static void dumpMemStats() {
		Dbg.o("Util.dumpMemStats()");
		double freeMBs = Runtime.getRuntime().freeMemory() / 1000000.;
		System.out.println("([" + memnf.format(freeMBs) + "MB ");
		if (counter++ % 3 == 0) {
			// Dbg.o(ProfilingMaps.getTopX(500));
			String td = ThreadingUtil.dumpThreads();
			if (lastTD == null || !lastTD.equals(td)) {
				Dbg.o(td);
				lastTD = td;
			}
		}
	}

	public static void startMemwatcher() {
		int activeCount = Thread.activeCount();
		Thread[] threads = new Thread[activeCount];
		Thread.enumerate(threads);
		for (Thread t : threads) {
			if (MEM_THREAD.equals(t.getName())) {
				Dbg.o("reusing existing memthread");
				break;
			}
		}
		if (memThread == null) {
			memThread = new Thread(MEM_THREAD) {

				public void run() {
					while (true) {
						System.gc();
						try {
							dumpMemStats();
							Thread.sleep(MEM_PERIOD_MS);
						} catch (InterruptedException e) {
							break;
						}
					}
				}
			};
			System.out.println("starting " + MEM_THREAD);
			memThread.start();
		}
	}

	public static void stopMemwatcher() {
		if (memThread != null) {
			System.out.println("interrupting " + memThread.toString());
			memThread.interrupt();
			memThread = null;
		}
	}

	public static boolean evenQ(long val) {
		return (val & 1) == 0;
	}

	public static boolean evenQ(int val) {
		return (val & 1) == 0;
	}

	public static boolean propQ(String property, Map<String, String> list) {
		return "true".equalsIgnoreCase(list.get(property));
	}
}
