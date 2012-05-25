package com.hartenbower.aspects;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.TimerTask;
import javax.annotation.processing.AbstractProcessor;

import org.aspectj.lang.Signature;
import org.aspectj.lang.reflect.CodeSignature;
import org.aspectj.lang.reflect.MethodSignature;

import com.hartenbower.annotations.EpicMethod;
import com.hartenbower.annotations.DumpProfile;
import com.hartenbower.util.RuntimeSwitches;
import com.hartenbower.util.ProfilingMaps;
import com.hartenbower.util.Util;
import com.hartenbower.util.StringUtil;
import com.hartenbower.util.TimingUtil;
import org.junit.Test;

public aspect Profiling {
	enum _ {Interceptors,Monitors};
	
    final static int RECURSION_THRESHOLD = 5;
    final private static boolean DEBUG = true;
    final private static boolean ACTIVE = true;
    final private static boolean TRACK_JAVA_UTIL = false;
    final private static boolean TRACK_JAVA_LANG = false;
    
    pointcut scoped() :
     within(com.hartenbower.matrix.*);

    
    pointcut methodExecution() :
        scoped()
        && ( execution( * *.*(..))
                  || execution(new(..)));
    
    pointcut pubMethodExecution() :
        scoped()
        && ( execution( public * *.*(..))
                  || execution(new(..)) )
        ;

    pointcut anyJavaUtilCall() :
        call(* java.util..*(..));
    pointcut ifAnyJavaUtilCall() :
        (if(TRACK_JAVA_UTIL) && anyJavaUtilCall())
        || (if(!TRACK_JAVA_UTIL) && !anyJavaUtilCall());
        
    pointcut anyJavaLangCall() :
        call(* java.util..*(..));
    pointcut ifAnyJavaLangCall() :
        (if(TRACK_JAVA_LANG) && anyJavaLangCall())
        || (if(!TRACK_JAVA_LANG) && !anyJavaLangCall());
        
    pointcut anyMethodCall() :
    scoped()
    && call( public * *.*(..))
    && ifAnyJavaUtilCall()
    && ifAnyJavaLangCall()
    && !call(public void TimerTask+.run() )
    && !call(public void Object+.wait() )
    && !call(public static void Thread.sleep(..) )
    && !target(Exception+);

    pointcut anyHibMethodCall() :
    scoped()
    &&  call(  * org.hibernate..*(..))
    && !call(public void TimerTask+.run() )
    && !call(public static void Thread.sleep(..) )
    && !target(Exception+);

//    pointcut initializer() :
//    scoped()
//    && cflow(anyMethodCall())
//    && execution( *.new(..));

    private static String typeString(Class<?>[] types) {
        StringBuffer sb = new StringBuffer("(");
        for (Iterator<Class<?>> i = Arrays.asList(types).iterator(); i
                .hasNext();) {
            sb.append(StringUtil.dePackage(i.next().getName()));
            if (i.hasNext()) {
                sb.append(", ");
            }
        }
        sb.append(")");
        return sb.toString();
    }

    private static String methodKey(Signature sig) {
        try {
            CodeSignature cs = (CodeSignature) sig;
            String typeString = typeString(cs.getParameterTypes());
            boolean epicMethod = false;
            if (MethodSignature.class.isAssignableFrom(sig.getClass())
                    && ((MethodSignature) sig).getMethod() != null) {
                epicMethod = ((MethodSignature) sig).getMethod().getAnnotation(
                        EpicMethod.class) != null;
            }
            String methodKey;
            if (!"()".equals(typeString)) {
                methodKey = cs.toShortString().replace("(..)",
                        typeString(cs.getParameterTypes()));
            } else {
                methodKey = cs.toShortString();
            }
            if (epicMethod) {
                return EpicMethod.class.getName() + " " + methodKey;
            }
            return methodKey;
        }catch(NullPointerException npe) {
            System.out.println("NPE in Profiling.methodKey(..), ignore if this is happening during a re-deployment");
            throw npe;
        }
    }

    private static String threadNamedMethodKey(Signature sig) {
        return methodKey(sig) + Thread.currentThread();
    }

//    before() : if(ACTIVE) && ( pubMethodExecution() || anyMethodCall() ||  anyHibMethodCall() ) && if(RuntimeSwitches.PROFILING)  {
    before() :  pubMethodExecution() && if(RuntimeSwitches.PROFILING)  {
        Signature sig = thisJoinPointStaticPart.getSignature();
        boolean epicMethod = false;
        if (MethodSignature.class.isAssignableFrom(sig.getClass())
                && ((MethodSignature) sig).getMethod() != null) {
            epicMethod = ((MethodSignature) sig).getMethod().getAnnotation(
                    EpicMethod.class) != null;
        }
        String methodKey = threadNamedMethodKey(sig);
//        if(RuntimeSwitches.TRACEALL && !epicMethod) {
//            System.out.println("pr-enter " + methodKey);
//        }
        try {
            Map<String, List<Long>> map = ProfilingMaps.getDeltaMap();
            List<Long> times = map.get(methodKey);
            if (times == null) {
                times = new ArrayList<Long>();
                times.add(System.currentTimeMillis());
                try {
                    map.put(methodKey, times);
                } catch (NullPointerException e) {
                    if (methodKey == null || map == null || times == null) {
                        System.out.println("methodKey " + (methodKey == null) + ", map "
                                + (map == null) + ", times " + (times == null));
                        e.printStackTrace();
                    }
                }
            } else {
                if (times.size() > RECURSION_THRESHOLD) {
                    System.out.println(methodKey + " recursed " + times.size());
                }
                times.add(System.currentTimeMillis());
            }
        } catch (NullPointerException n) {
            System.out.println("Profiling.aj got NPE");
        }
        System.out.flush();
    }

//    after() : if(ACTIVE) && ( pubMethodExecution() || anyMethodCall() || anyHibMethodCall() ) && if(RuntimeSwitches.PROFILING) {
    after()  : pubMethodExecution() && if(RuntimeSwitches.PROFILING) {
        Signature sig = thisJoinPointStaticPart.getSignature();
        boolean epicMethod = false;
        if (MethodSignature.class.isAssignableFrom(sig.getClass())
                && ((MethodSignature) sig).getMethod() != null) {
            epicMethod = ((MethodSignature) sig).getMethod().getAnnotation(
                    EpicMethod.class) != null;
        }
        String methodKey = threadNamedMethodKey(sig);
//        if(RuntimeSwitches.TRACEALL && !epicMethod) {
//            System.out.println("pr-exit " + methodKey);
//        }
//
        String methodKeyForAggregateTimes = methodKey(sig);
        try {
            Map<String, List<Long>> map = ProfilingMaps.getDeltaMap();
            List<Long> times = map.get(methodKey);
            long ltime = 0;
            double time = 0;
            try {
                ltime = times.remove(times.size() - 1);
                time = (System.currentTimeMillis() - ltime) / 1000.;
            } catch(ArrayIndexOutOfBoundsException ex) {
                System.out.println("bad idx " + (times.size() - 1));
                return;
            }
            if (ProfilingMaps.AGGREGATE_METHOD_TIMES) {
                Double dTotalTime = ProfilingMaps.getAggregateMap().get(
                        methodKeyForAggregateTimes);
                if (dTotalTime == null) {
                    dTotalTime = time;
                } else {
                    dTotalTime += time;
                }
                Long count = ProfilingMaps.getCounterMap().get(
                        methodKeyForAggregateTimes);
                if (count == null) {
                    count = 1l;
                } else {
                    count++;
                }
                ProfilingMaps.getAggregateMap().put(methodKeyForAggregateTimes,
                        dTotalTime);
                ProfilingMaps.getCounterMap().put(methodKeyForAggregateTimes,
                        count);
            }
            if (time > Util.SIGMA
                    && methodKey.indexOf(EpicMethod.class.getName()) == -1) {
                System.out.println(methodKey + " took "
                        + TimingUtil.fromSeconds(time));
            }
        } catch (NullPointerException e) {
            if (DEBUG)
                System.out.println("mismatched before / after for " + methodKey);
        } 
        System.out.flush();
    }


    after() : if(ACTIVE) && execution(@Test * *.*(..)) {
        Signature sig = thisJoinPointStaticPart.getSignature();
        if (sig instanceof MethodSignature) {
            Method method = ((MethodSignature) sig).getMethod();
            DumpProfile dump = method.getAnnotation(DumpProfile.class);
            if ((RuntimeSwitches.OUTPUT_PROFILE_AFTER_TEST || dump != null) && RuntimeSwitches.PROFILING) {
                System.out.println("Profiling.aj @Test" + (dump != null ? " @DumpProfile" : ""));
                ProfilingMaps.printTopX(200);
            } 
        }
    }
    
 }