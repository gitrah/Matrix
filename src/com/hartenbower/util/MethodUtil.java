package com.hartenbower.util;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.TreeSet;

import org.apache.log4j.Logger;

import scala.Tuple2;

/**
 * Reflection support for methods, with caching
 * <p/>
 */
public class MethodUtil {

    private static Logger log = Logger.getLogger(MethodUtil.class);
    private final static boolean DEBUG = false;

    private final static boolean DEBUG_INIT = false;

    @SuppressWarnings("unused")
    private final static Object _NPIF = null;
    private static Field NONPUBLIC_INHERITED_FIELD;
    static {
        try {
            NONPUBLIC_INHERITED_FIELD = MethodUtil.class
                    .getDeclaredField("_NPIF");
        } catch (Exception e) {
        }
    }

    private static Map<Class<?>, Map<Method, Method>> classActators = new HashMap<Class<?>, Map<Method, Method>>();

    private static Map<Class<?>, List<Field>> classFieldLists = new HashMap<Class<?>, List<Field>>();

    private static Map<Class<?>, Method> classIdAccessors = new HashMap<Class<?>, Method>();

    public static Class<?>[] NO_ARG_TYPE;

    public static Object[] NO_ARG;

    public static synchronized void scan(Class<?> entityType) {
        if (classFieldLists.get(entityType) == null) {

            List<Field> linealFields = FieldUtil.getLinealFields(entityType);
            if (DEBUG_INIT)
                log.debug(entityType + " has \n" + linealFields);
            // fieldSet.addAll(Arrays.asList(entityType.getFields()));
            Map<Method, Method> actators = new HashMap<Method, Method>();

            if (DEBUG_INIT)
                log.debug("nothing in classFieldSets for " + entityType);

            String fieldName;
            Class<?> fieldType;

            addMethods(entityType);

            Map<Tuple2<String, Class<?>[]>, Method> linealMethods = classLinealMethodNameMethod
                    .get(entityType);
            for (Field field : linealFields) {
                fieldName = field.getName();
                fieldType = field.getType();
                if (fieldName.startsWith("ajc$")) {
                    // ignore aspectj glue
                    continue;
                }
                // find actator pair
                fieldName = Character.toUpperCase(fieldName.charAt(0))
                        + fieldName.substring(1);
                String accName = ((Boolean.class == fieldType || boolean.class == fieldType) ? "is"
                        : "get")
                        + fieldName;
                String mutName = "set" + fieldName;
                if (DEBUG)
                    Dbg.o("trying " + accName + ", " + mutName);
                try {
                    Method accessor = linealMethods
                            .get(new Tuple2<String, Class<?>[]>(accName,
                                    Util.VOID_SIG));
                    Method mutator = null;
                    try {
                        mutator = entityType.getMethod(mutName,
                                new Class<?>[] { fieldType });
                    } catch (Exception e) {
                        if (DEBUG)
                            log.debug("no mutator " + mutName + " on "
                                    + entityType);
                    }
                    actators.put(accessor, mutator);
                    if ("Id".equals(fieldName)) {
                        if (DEBUG)
                            Dbg.o("and is Id field");
                        classIdAccessors.put(entityType, accessor);
                    }

                } catch (Exception e) {
                    if (DEBUG)
                        log.debug(field.getName()
                                + " lacked properly-named actator!");
                }
                // }
            }
            classActators.put(entityType, actators);
            classFieldLists.put(entityType, linealFields);
            if (DEBUG)
                log.debug("added " + linealFields.size() + " fields, "
                        + actators.size() + " actators to cache for "
                        + entityType.getName());
        }
    }

    public static Map<Method, Method> getPropertyMethods(Class<?> entityClass) {
        Map<Method, Method> actators = classActators.get(entityClass);
        if (actators == null) {
            scan(entityClass);
            actators = classActators.get(entityClass);
        }
        return actators;
    }

    public static Method getIdAccessor(Object entity) {
        Method getId = classIdAccessors.get(entity.getClass());
        if (getId == null) {
            scan(entity.getClass());
            getId = classIdAccessors.get(entity.getClass());
            if (getId == null) {
                throw new IllegalStateException("can't find a getId method on "
                        + entity);
            }
        }
        return getId;
    }

    public static Serializable getId(Object entity) {
        if (entity == null) {
            throw new IllegalArgumentException("entity is null!!");
        }
        Method getId = getIdAccessor(entity);
        try {
            return (Serializable) getId.invoke(entity, NO_ARG);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public static void setId(Object entity, Object id) {
        if (entity == null) {
            throw new IllegalArgumentException("entity is nul!!");
        }
        Method getId = classIdAccessors.get(entity.getClass());
        if (getId == null) {
            scan(entity.getClass());
            getId = classIdAccessors.get(entity.getClass());
            if (getId == null) {
                throw new IllegalStateException("can't find a getId method on "
                        + entity);
            }
        }
        Method setId = classActators.get(entity.getClass()).get(getId);
        try {
            setId.invoke(entity, id);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static Class<?> getIdClass(Class<?> entityClass) {
        Method getId = classIdAccessors.get(entityClass);
        if (getId == null) {
            scan(entityClass);
            getId = classIdAccessors.get(entityClass);
        }
        return getId.getReturnType();
    }

    public static void dumpFields(Object o) {
        if (o == null) {
            log.debug("null object");
            return;
        }

        Class<?> entityType = o.getClass();
        List<Field> fields = classFieldLists.get(entityType);
        if (fields == null) {
            scan(entityType);
            fields = classFieldLists.get(entityType);
        }
        Map<String, Field> map = new HashMap<String, Field>();
        for (Field f : fields) {
            map.put(f.getName(), f);
        }
        StringBuffer sb = new StringBuffer("\n" + o + "\n");

        Field field;
        for (String fieldName : new TreeSet<String>(map.keySet())) {
            if (fieldName.startsWith("ajc$tjp")) {
                continue;
            }
            field = map.get(fieldName);
            sb.append("\t");
            if (field == NONPUBLIC_INHERITED_FIELD) {
                log.debug("id field is non-public, trying accessor");
                fieldName = "id";
                sb.append(fieldName);
                sb.append(":  ");
                sb.append(getId(o));
            } else {
                try {
                    field.setAccessible(true);
                    sb.append(fieldName);
                    sb.append(":  ");
                    sb.append(field.get(o));
                } catch (Exception e) {
                    e.printStackTrace();
                    break;
                }

            }
            sb.append("\n");
        }
        log.debug(sb);
    }

    private static Map<Class<?>, Map<Tuple2<String, Class<?>[]>, Method>> classLinealMethodNameMethod = new HashMap<Class<?>, Map<Tuple2<String, Class<?>[]>, Method>>();
    private static Map<Class<?>, Set<Method>> classLinealMethod = new HashMap<Class<?>, Set<Method>>();

    private static synchronized <T> Map<Tuple2<String, Class<?>[]>, Method> addMethods(
            Class<T> theClass) {
        Map<Tuple2<String, Class<?>[]>, Method> nameMethod = classLinealMethodNameMethod
                .get(theClass);
        if (nameMethod == null) {
            nameMethod = new HashMap<Tuple2<String, Class<?>[]>, Method>();
            Set<Method> linealMethods = new HashSet<Method>();
            for (Class<?> currClass : FieldUtil.lineage(theClass)) {
                linealMethods.addAll(Arrays.asList(currClass
                        .getDeclaredMethods()));
            }
            for (Method method : linealMethods) {
                if (method.getName().startsWith("ajc$")) {
                    continue;
                }
                // if (Modifier.isStatic(method.getModifiers())
                // || (Modifier.isFinal(method.getModifiers()) && !theClass
                // .isEnum())) {
                // continue;
                // }
                method.setAccessible(true);
                nameMethod.put(new Tuple2<String, Class<?>[]>(method.getName(),
                        method.getParameterTypes()), method);
            }
            classLinealMethodNameMethod.put(theClass, nameMethod);
        }
        classLinealMethod.put(theClass,
                new HashSet<Method>(nameMethod.values()));
        return nameMethod;
    }

    public static Map<Tuple2<String, Class<?>[]>, Method> getLinealMethods(
            Class<?> theClass) {
        Map<Tuple2<String, Class<?>[]>, Method> nameMethod = classLinealMethodNameMethod
                .get(theClass);
        if (nameMethod == null) {
            nameMethod = addMethods(theClass);
        }
        return nameMethod;
    }

    private static boolean isBoxing(Object o1, Object o2) {
        return ((o1 == Integer.class && o2 == int.class || o2 == Integer.class
                && o1 == int.class)
                || (o1 == Long.class && o2 == long.class || o2 == Long.class
                        && o1 == long.class)
                || (o1 == Boolean.class && o2 == boolean.class || o2 == Boolean.class
                        && o1 == boolean.class)
                || (o1 == Double.class && o2 == double.class || o2 == Double.class
                        && o1 == double.class)
                || (o1 == Float.class && o2 == float.class || o2 == Float.class
                        && o1 == float.class)
                || (o1 == Short.class && o2 == short.class || o2 == Short.class
                        && o1 == short.class)
                || (o1 == Character.class && o2 == char.class || o2 == Character.class
                        && o1 == char.class) || (o1 == Byte.class
                && o2 == byte.class || o2 == Byte.class && o1 == byte.class));
    }

    final private static boolean DEBUG_GET_LINEAL = false;

    public static Method getLinealMethod(Class<?> theClass, String methodName,
            Class<?>[] argSig) {
        Map<Tuple2<String, Class<?>[]>, Method> nameMethod = getLinealMethods(theClass);
        if (DEBUG_GET_LINEAL)
            log.info(theClass.getSimpleName() + " has " + Dbg.oS(nameMethod));

        Tuple2<String, Class<?>[]> tup = new Tuple2<String, Class<?>[]>(
                methodName, argSig);

        Class<?> c2 = tup._2.getClass();

        for (Tuple2<String, Class<?>[]> t : nameMethod.keySet()) {
            if (DEBUG_GET_LINEAL)
                log.info(t._1 + " eq " + tup._1 + " " + t._1.equals(tup._1));
            if (t._1.equals(tup._1)) {
                if (DEBUG_GET_LINEAL)
                    log.info(t._2 + " eq " + tup._2 + " " + t._2.equals(tup._2));
                if (!t._2.equals(tup._2)) {
                    if (t._2 != null && tup._2 != null) {
                        Class<?> c1 = t._2.getClass();
                        if (c1.isArray() && c2.isArray()) {
                            if (c1.getComponentType().equals(
                                    c2.getComponentType())) {
                                Object[] a1 = (Object[]) t._2;
                                Object[] a2 = (Object[]) tup._2;
                                if (DEBUG_GET_LINEAL)
                                    log.info("a1.length == a2.length "
                                            + (a1.length == a2.length));
                                if (a1.length == a2.length) {
                                    boolean foundIt = true;
                                    for (int i = 0; i < a1.length; i++) {

                                        if (a1[i] == null && a2[i] != null) {
                                            if (DEBUG_GET_LINEAL)
                                                log.info("a1[i] == null && a2[i] != null");
                                            foundIt = false;
                                            break;
                                        } else if (a1[i] != null) {
                                            if (DEBUG_GET_LINEAL)
                                                log.info(a1[i] + " eq " + a2[i]
                                                        + " "
                                                        + a1[i].equals(a2[i]));
                                            if (a1[i].equals(a2[i])) {
                                                continue;
                                            }
                                            if (isBoxing(a1[i], a2[i])) {
                                                if (DEBUG_GET_LINEAL)
                                                    log.info("one boxes the other");
                                                continue;
                                            } else if (Collection.class
                                                    .isAssignableFrom((Class) a1[i])
                                                    && ((Class) a1[i])
                                                            .isAssignableFrom((Class) a2[i])
                                                    || ((Class) a2[i])
                                                            .isAssignableFrom((Class) a1[i])) {
                                                if (DEBUG_GET_LINEAL)
                                                    log.info("one subclasses the other");
                                                continue;
                                            }
                                            foundIt = false;
                                            break;
                                        }
                                    }
                                    if (foundIt) {
                                        return nameMethod.get(t);
                                    }
                                }
                            }
                        }
                    }
                } else {
                    return nameMethod.get(t);
                }
            }
        }
        throw new IllegalArgumentException("no " + methodName + " in "
                + theClass.getSimpleName());
    }

}
