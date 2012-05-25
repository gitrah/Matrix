package com.hartenbower.util;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import org.apache.log4j.Logger;

public class FieldUtil {
    protected static Logger log = Logger.getLogger(FieldUtil.class);
    protected static Random rnd = new Random(System.currentTimeMillis());
    protected static Map<Class<?>, Map<String, Field>> classLinealFieldNameField = new HashMap<Class<?>, Map<String, Field>>();
    protected static Map<Class<?>, List<Field>> classLinealField = new HashMap<Class<?>, List<Field>>();

    
    static Class Id;
    static {
        try {
            Id = Class.forName("javax.persistence.Id");
        } catch (Exception e) {
            log.warn("" + e);
        }
        log.info("classLinealField  " +classLinealField);
    }

    public static Class DOUBLE = double.class;
    public static Class FLOAT = float.class;
    public static Class LONG = long.class;
    public static Class INTEGER = int.class;
    public static Class BOOLEAN = boolean.class;


    protected static <T> Map<String, Field> addFields(Class<T> theClass, int... mods) {
        Map<String, Field> nameField = classLinealFieldNameField.get(theClass);
        if (nameField == null) {
            nameField = new HashMap<String, Field>();
            List<Field> linealFields = new ArrayList<Field>();
            int fmods;
            for (Class<?> currClass : lineage(theClass)) {
                for(Field f : currClass.getDeclaredFields()) {
                    fmods = f.getModifiers();
                    if (f.getName().startsWith("ajc$") 
                            || hasAnyMod(fmods, mods)) {
                        continue;
                    }
                    if(!linealFields.contains(f)) {
                        linealFields.add(f);
                    }
                }
            }
            //log.info(theClass.getSimpleName() + " has fields \n" + Dbg.oS(linealFields));
            for (Field field : linealFields) {
                field.setAccessible(true);
                nameField.put(field.getName(), field);
            }
            classLinealFieldNameField.put(theClass, nameField);
            classLinealField.put(theClass, linealFields);
        }
        return nameField;
    }

    /**
     * returns the set of superclasses classes, down to and including theClass
     * 
     * @param theClass
     * @return
     */
    public static Set<Class<?>> lineage(Class<?> theClass) {
        Set<Class<?>> lineage = new HashSet<Class<?>>();
        lineage.add(theClass);
        Class<?> current = theClass;
        while (current != Object.class
                && Object.class != current.getSuperclass()) {
            lineage.add(current.getSuperclass());
            current = current.getSuperclass();
        }
        return lineage;
    }

    public static boolean simpleTypeQ(Class<?> c) {
        return c.isPrimitive() || String.class == c
                || Number.class.isAssignableFrom(c) || Date.class.isAssignableFrom(c);
    }


    public static boolean hasAnyMod(int mod, int... mods) {
        for(int m : mods) {
            if( (mod & m) == m) {
                return true;
            }
        }
        return false;
    }
    
    public static synchronized List<Field> getLinealFieldsWithoutMods(Class<?> theClass, int... mods) {
        try {
            List<Field> field = classLinealField.get(theClass);
            if (field == null) {
                addFields(theClass, mods);
            }
            return classLinealField.get(theClass);
        }catch(Exception e) {
            log.error("got " + e + " for theClass " + theClass + " and mods " + mods + " and classLinealField " + classLinealField );
            return null;
        }
    }
    /**
     * @param theClass
     * @return the set of all Fields declared in theClass and its superclasses
     *         Note: will return multiple Fields with the same name if declared
     *         in multiple classes in the hierarchy
     */
    public static synchronized List<Field> getLinealFields(Class<?> theClass) {
        return getLinealFieldsWithoutMods(theClass );
    }
    
    public static Field getLinealField(Class<?> theClass, String fieldName) {
        Map<String, Field> nameField = classLinealFieldNameField.get(theClass);
        if (nameField == null) {
            nameField = addFields(theClass);
        }
        Field f = nameField.get(fieldName);
        if (f == null) {
            throw new IllegalArgumentException("no such field '" + theClass.getSimpleName() + "." + fieldName + "'");
        }
        return f;
    }

    
    public static Object set(Object o, String fieldName, Object value) {
        Class<?> c = o.getClass();
        Field f = getLinealField(c, fieldName);
        f.setAccessible(true);
        Object ret = null;
        try {
            ret = f.get(o);
            f.set(o, value);
        }catch(Exception e) {
            log.error(c.getSimpleName() + "." +  fieldName + " := " + value + " ==> " + e);
            e.printStackTrace();
        }
        return ret;
    }

    public static Object get(Object o, String fieldName) throws IllegalArgumentException, IllegalAccessException {
        Class<?> c = o.getClass();
        Field f = getLinealField(c, fieldName);
        f.setAccessible(true);
        return f.get(o);
    }


    public static boolean flatEquals(Collection<?> src, Collection<?> trg) {
        if (src.size() != trg.size()) {
            return false;
        }
        Iterator<?> iSrc = src.iterator();
        Iterator<?> iTrg = trg.iterator();
        while (iSrc.hasNext()) {
            if (!iSrc.next().equals(iTrg.next())) {
                return false;
            }
        }
        return true;
    }

    public static boolean flatEquals(Object[] src, Object[] trg) {
        if (src.length != trg.length) {
            return false;
        }
        for (int i = 0; i < src.length; i++) {
            if (!src[i].equals(trg[i])) {
                return false;
            }
        }
        return true;
    }

    public static boolean lineallyStructurallyEquals(Object o1, Object o2, 
            String... ignoredFieldNames ) {
        List<String> ignored = Arrays.asList(ignoredFieldNames);
        try {
           // log.info("o1 " + o1 + ", o2 " + o2);

            if (o1 != null) {
                if (o2 != null) {
                    Class c1 = o1.getClass();
                    Class c2 = o2.getClass();
                    if (c1.isAssignableFrom(c2) || c2.isAssignableFrom(c1)) {
                        if (c1.isArray()) {
                            Object[] a1 = (Object[]) o1;
                            Object[] a2 = (Object[]) o2;
                            if (a1.length == a2.length) {
                                for (int i = 0; i < a1.length; i++) {
                                    if (!lineallyStructurallyEquals(a1[i],
                                            a2[i])) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        } else if (Collection.class.isAssignableFrom(c1)) {
                            Collection col1 = (Collection) o1;
                            Collection col2 = (Collection) o2;
                            if (col1.size() == col2.size()) {
                                Iterator i1 = col1.iterator();
                                Iterator i2 = col2.iterator();
                                while (i1.hasNext()) {
                                    if (lineallyStructurallyEquals(i1.next(),
                                            i2.next())) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        } else if (Map.class.isAssignableFrom(c1)) {
                            Map m1 = (Map) o1;
                            Map m2 = (Map) o2;
                            if (m1.size() == m2.size()) {
                                Iterator i1 = m1.entrySet().iterator();
                                Iterator i2 = m2.entrySet().iterator();
                                while (i1.hasNext()) {
                                    if (lineallyStructurallyEquals(i1.next(),
                                            i2.next())) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        } else {
                            Method eq = null;
                            try {
                                eq = c1.getDeclaredMethod("equals",
                                        Object.class);
                            } catch (Exception eaten) {
                            }
                            if (eq != null) {
                                return o1.equals(o2);
                            } else {
                                //log.info(c1 + " doesn't impl equals()");
                                List<Field> fs1 = getLinealFields(c1);
                                List<Field> fs2 = getLinealFields(c2);

                                if (!flatEquals(fs1, fs2)) {
                                    log.info("field set mismatch");
                                    return false;
                                }

                                int mods;
                                for (Field f : fs1) {
                                    if (ignored.contains(f.getName())) {
                                        continue;
                                    }
                                    mods = f.getModifiers();
                                    if (Modifier.isStatic(mods)) {
                                        continue;
                                    }
                                    f.setAccessible(true);
                                    Object v1 = f.get(o1);
                                    Object v2 = f.get(o2);
                                     
                                    if (!lineallyStructurallyEquals(v1, v2)) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        }
                    }
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    public static boolean structurallyEquals(Object o1, Object o2,
            String... ignoredFieldNames) {
        List<String> ignored = Arrays.asList(ignoredFieldNames);
        try {

            if (o1 != null) {
                if (o2 != null) {
                    Class c1 = o1.getClass();
                    Class c2 = o2.getClass();
                    if (c1.isAssignableFrom(c2) || c2.isAssignableFrom(c1)) {
                        if (c1.isArray()) {
                            Object[] a1 = (Object[]) o1;
                            Object[] a2 = (Object[]) o2;
                            if (a1.length == a2.length) {
                                for (int i = 0; i < a1.length; i++) {
                                    if (!lineallyStructurallyEquals(a1[i],
                                            a2[i])) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        } else if (Map.class.isAssignableFrom(c1)) {
                            Map m1 = (Map) o1;
                            Map m2 = (Map) o2;
                            if (m1.size() == m2.size()) {
                                Iterator i1 = m1.entrySet().iterator();
                                Iterator i2 = m2.entrySet().iterator();
                                while (i1.hasNext()) {
                                    if (lineallyStructurallyEquals(i1.next(),
                                            i2.next())) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        } else if (Collection.class.isAssignableFrom(c1)) {
                            Collection col1 = (Collection) o1;
                            Collection col2 = (Collection) o2;
                            if (col1.size() == col2.size()) {
                                Iterator i1 = col1.iterator();
                                Iterator i2 = col2.iterator();
                                while (i1.hasNext()) {
                                    if (lineallyStructurallyEquals(i1.next(),
                                            i2.next())) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        } else {
                            Method eq = null;
                            try {
                                eq = c1.getDeclaredMethod("equals",
                                        Object.class);
                            } catch (Exception eaten) {
                            }
                            if (eq != null) {
                                return o1.equals(o2);
                            } else {
                                Field[] fs1 = c1.getDeclaredFields();
                                Field[] fs2 = c2.getDeclaredFields();

                                if (!flatEquals(fs1, fs2)) {
                                    log.info("field set mismatch");
                                    return false;
                                }

                                int mods;
                                for (Field f : fs1) {
                                    if (ignored.contains(f.getName())) {
                                        continue;
                                    }
                                    mods = f.getModifiers();
                                    if (Modifier.isStatic(mods)) {
                                        continue;
                                    }
                                    f.setAccessible(true);
                                    Object v1 = f.get(o1);
                                    Object v2 = f.get(o2);

                                    if (!lineallyStructurallyEquals(o1, o2)) {
                                        return false;
                                    }
                                }
                                return true;
                            }
                        }
                    }
                }
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
        return false;
    }

    /**
     * instantiates a random value of type c
     * 
     * @param c
     * @return
     */
    public static Object random(Class<?> c) {
        Object ret = null;
        if (Collection.class.isAssignableFrom(c)) {
            if (Set.class.isAssignableFrom(c)) {
                return Collections.EMPTY_SET;
            } else if (List.class.isAssignableFrom(c)) {
                return Collections.EMPTY_LIST;
            } else if (Map.class.isAssignableFrom(c)) {
                return Collections.EMPTY_MAP;
            }
        } else if (Double.class.isAssignableFrom(c)
                || double.class.isAssignableFrom(c)) {
            ret = rnd.nextDouble();
        } else if (Float.class.isAssignableFrom(c)
                || float.class.isAssignableFrom(c)) {
            ret = rnd.nextFloat();
        } else if (Long.class.isAssignableFrom(c)
                || long.class.isAssignableFrom(c)) {
            long l = rnd.nextLong();
            ret = l > 0 ? l : -l;
        } else if (Integer.class.isAssignableFrom(c)
                || int.class.isAssignableFrom(c)) {
            int i = rnd.nextInt();
            ret = i > 0 ? i : -i;
        } else if (Short.class.isAssignableFrom(c)
                || short.class.isAssignableFrom(c)) {
            int i = rnd.nextInt();
            ret = i > 0 ? i : -i;
        } else if (Boolean.class.isAssignableFrom(c)
                || boolean.class.isAssignableFrom(c)) {
            ret = rnd.nextBoolean();
        } else if (String.class.isAssignableFrom(c)) {
            ret = StringUtil.randomString();
        } else if (Date.class.isAssignableFrom(c)) {
            ret = new Date();
        } else {
            try {
                ret = c.newInstance();
            } catch (Exception e) {
                e.printStackTrace();
                ret = null;
            }
        }

        return ret;
    }

    public static void populate(Object[] args, Class<?>[] argSig) {
        int i = 0;
        for (Class<?> c : argSig) {
            args[i] = random(c);
            i++;
        }
    }

    public static Collection<Object> randomClassInstantiation(
            Collection<Class<?>> argSig) {
        Class<?> collClass = argSig.getClass();
        Collection<Object> coll = null;
        if (List.class.isAssignableFrom(collClass)) {
            coll = new ArrayList<Object>();
        } else if (Set.class.isAssignableFrom(collClass)) {
            coll = new HashSet<Object>();
        }
        for (Class<?> c : argSig) {
            coll.add(random(c));
        }
        return coll;
    }

    private static boolean complexTypeQ(Class type) {
        return !type.isPrimitive() && type != String.class
                && !Date.class.isAssignableFrom(type)
                && !Class.class.isAssignableFrom(type)
                && !Calendar.class.isAssignableFrom(type)
                && !Boolean.class.isAssignableFrom(type)
                && !Number.class.isAssignableFrom(type)
                && !Collection.class.isAssignableFrom(type)
                && !type.getName().startsWith("com.sun");
    }

    public static String dumpFields(Object object, Field f, int dist) {
        Class baseClass = (object instanceof Class) ? ((Class) object) : object
                .getClass();
        StringBuffer sb = new StringBuffer();
        String tabs = "";
        if (dist > 0) {
            char[] tabcs = new char[dist];
            Arrays.fill(tabcs, ' ');
            tabs = new String(tabcs);
        }
        sb.append(tabs + "fields of " + (f != null ? f.getName() : object)
                + " (" + object.getClass().getSimpleName() + ")\n");
        int mods;
        for (Field theField : FieldUtil.getLinealFields(baseClass)) {
            theField.setAccessible(true);
            mods = theField.getModifiers();
            if (Modifier.isStatic(mods) || Modifier.isTransient(mods)) {
                continue;
            }
            try {
                Object val = theField.get(object);
                if (val != null) {
                    Class type = val.getClass();
                    String types = type.getName();

                    if (complexTypeQ(type)) {
                        // log.info("recursing " + theField.getName() + " -> " +
                        // types);
                        sb.append(dumpFields(val, theField, dist + 1));
                    } else {
                        sb.append(tabs + " " + theField.getName() + ": " + val
                                + "\n");
                    }
                }
            } catch (IllegalArgumentException e) {
                // e.printStackTrace();
            } catch (IllegalAccessException e) {
                // e.printStackTrace();
            }

        }

        return sb.toString();
    }

    public static void dumpFields(Object object) {
        log.info("\n" + dumpFields(object, null, 0));
    }
    
    private static void set(Map m, Field f, Object val) {
        m.put(f.getName(), val);
    }
    
    private static Collection newInstance(Class<?> type) {
        if(Set.class.isAssignableFrom(type)) {
            return new HashSet();
        } else if (Map.class.isAssignableFrom(type)) {
            return new HashSet<Map.Entry>();
        }
        return new ArrayList();
    }

    // if different simple, new value
    public static Map<String, ?> mapDiff(Object a, Object b) {
        Map<String, Object> map = new HashMap<String, Object>();
        Class<?> c = a.getClass();
        if (c != b.getClass()) {
            if (b.getClass().isAssignableFrom(c)) {
                // log.info(b.getClass() + " is the base");
                c = b.getClass();
            } else if (!c.isAssignableFrom(b.getClass())) {
                throw new IllegalArgumentException(a + " and " + b
                        + " aren't related");
            }
        }

        Class<?> type;
        for (Field f : FieldUtil.getLinealFields(c)) {
            f.setAccessible(true);
            type = f.getType();
            int mods;
            mods = f.getModifiers();
            if (!Modifier.isFinal(mods) && !Modifier.isStatic(mods)
                    && !Modifier.isTransient(mods)) {
                // log.info(f.getName());
                Object av = null;
                Object bv = null;
                try {
                    av = f.get(a);
                    bv = f.get(b);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                //log.info(f.getName() + " av " + av + " bv " + bv);
                if (av != null) {
                    if (bv != null) {
                        if (Collection.class.isAssignableFrom(type) || type.isArray() || Map.class.isAssignableFrom(type)) {
                           
                            Collection<?> collA, collB;
                            int sizea,sizeb;
                            if(type.isArray()) {
                                Object[] arrA = (Object[]) av;
                                Object[] arrB = (Object[]) bv;
                                collA = Arrays.asList(arrA);
                                collB = Arrays.asList(arrB);
                            } else if(Map.class.isAssignableFrom(type)) {
                                collA = ((Map<?,?>)av).entrySet();
                                collB = ((Map<?,?>)bv).entrySet();
                            } else {
                                collA = (Collection<?>) av;
                                collB = (Collection<?>) bv;
                            }
                            sizea=collA.size();
                            sizeb=collB.size();
                            
                            if(sizea > sizeb && collA.containsAll(collB)) {
                                Collection collC = newInstance(type);
                                collC.addAll(collA);
                                collC.removeAll(collB);
                                set(map, f, "removed " + Dbg.oS(collC));
                            } else if(sizeb > sizea && collB.containsAll(collA)) {
                                Collection collC = newInstance(type);
                                collC.addAll(collB);
                                collC.removeAll(collA);
                                set(map, f, "added " + Dbg.oS(collC));
                            } else {
                                if(sizea != sizeb) {
                                    set(map,f, "size " + sizea + " -> " + sizeb);
                                } else {
                                    if(!collA.equals(collB)) {
                                        set(map,f, collB);
                                    }
                                }
                            }
                        }  else {
                            boolean delta = false;
                            if(type.isPrimitive()) {
                                delta = !av.equals(bv);
                            } else {
                                Method eq = null;
                                try {
                                    eq = type.getDeclaredMethod("equals",
                                            Object.class);
                                } catch (Exception eaten) {
                                }
                                if (eq != null) {
                                    delta = !av.equals(bv);
                                } else {
                                    Field[] fs = type.getDeclaredFields();
                                    for (Field field : fs) {
                                        mods = field.getModifiers();
                                        if (Modifier.isStatic(mods)) {
                                            continue;
                                        }
                                        field.setAccessible(true);
                                        try { 
                                            Object v1 = field.get(av);
                                            Object v2 = field.get(bv);
            
                                            delta |= !lineallyStructurallyEquals(v1, v2);
                                        }catch(Exception e) {
                                            e.printStackTrace();
                                        }
                                    }
                                }
                            }
                            if (delta) {
                                if (complexTypeQ(type)) {
                                    Map<String, ?> submap = mapDiff(av, bv);
                                    // log.info( "next (" + Dbg.oS(next) + ") is empty " +
                                    // next.isEmpty());
                                    if (!submap.isEmpty()) {
                                        map.put(f.getName(), submap);
                                    }
                                } else {
                                        map.put(f.getName(), bv);
                                }
                            }
                        }
                    } else {
                        set(map, f, "->null");
                    }
                } else {
                    if(bv != null) {
                        set(map, f, "null->" + bv);
                    }
                }

            }
        }
        return map;
    }

    public static Field idField(Object entity) {
        for (Field f : getLinealFields(entity.getClass())) {
            if (f.getAnnotation(Id) != null) {
                return f;
            }
        }
        throw new IllegalArgumentException(entity
                + " has no Id-annotated field");
    }

    public static Field idFieldClass(Class entityClass) {
        for (Field f : getLinealFields(entityClass)) {
            if (f.getAnnotation(Id) != null) {
                return f;
            }
        }
        throw new IllegalArgumentException(entityClass
                + " has no Id-annotated field");
    }
    
    public static String dump(Object o) {
        return dumpButNot(o);
    }
    
    public static String dumpButNot(Object o, int...mods) {
        Class<?> c = o instanceof Class ? (Class)o : o.getClass();
        StringBuffer sb = new StringBuffer();
        int targetMods;
        boolean skip;
        for (Field f : getLinealFields(c)) {
            skip=false;
            targetMods = f.getModifiers();
            for(int m : mods) {
                if((targetMods & m) == m) {
                    skip=true;
                    break;
                }
            }
            if( skip ) {
                continue;
            } 
            f.setAccessible(true);
            try {
                Object val = f.get(o);
                Class<?> type = f.getType();
                if(val != null ) {
                    if(Collection.class.isAssignableFrom(type)) {
                        sb.append(f.getName() + ": " + Dbg.oS((Collection)val));
                        sb.append("\n");
                        continue;
                    }
                } 
                sb.append(f.getName() + ": " + val);
            } catch (IllegalAccessException e) {
                sb.append(f.getName() + ": " + e);
            }
            sb.append("\n");
        }
        return sb.toString();
    }
    
    public static String toCsv(Collection<Object> os) {
        if(os.isEmpty()) {
            return "";
        }
        Iterator i = os.iterator();
        Object first = i.next();
        Class<?> c = first instanceof Class ? (Class)first : first.getClass();
        int targetMods;
        List<Field> fields = getLinealFields(c);
        Iterator<Field> iF = fields.iterator();
        Field f;
        Class<?> type;
        while(iF.hasNext()) {
            f = iF.next();
            targetMods = f.getModifiers();
            type = f.getType();
            if(Modifier.isFinal(targetMods) 
                    || Modifier.isStatic(targetMods)
                    || Modifier.isTransient(targetMods)
                    || complexTypeQ(type)
               ) {
                iF.remove();
            } else {
                f.setAccessible(true);
            }
        }
        
        StringBuffer sb = new StringBuffer();
        for(Field ff : fields) {
            sb.append(ff.getName());
            sb.append(", ");
        }
        sb.setLength(sb.length()-2);
        sb.append("\n");
        for(Object o : os) {
            toCsv(sb, o, fields);
        }
        return sb.toString();
    }
    
    public static void toCsv(StringBuffer sb, Object o, List<Field> fields) {
        for (Field f : fields) {
            try {
                sb.append(f.get(o));
            } catch (Exception e) {
                e.printStackTrace();
            } 
            sb.append(", ");
        }
        sb.setLength(sb.length()-2);
        sb.append("\n");
    }
    
    public static <T> T clone(T obj) throws Exception {
        Class<?> c = obj.getClass();
        //log.info("got class " + c.getSimpleName());
        T clone = (T) c.newInstance();
        //log.info("got clone " + clone);
        Object v;
        List<Field> fs = getLinealFieldsWithoutMods(c, Modifier.FINAL, Modifier.STATIC, Modifier.TRANSIENT  ) ;
        //log.info(c.getSimpleName() + " has fields " + Dbg.oS(fs));
        for(Field f : fs) {
            f.setAccessible(true);
            v = f.get(obj);
            f.set(clone, v);
        }
        return clone;
    }
}