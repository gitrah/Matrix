package com.hartenbower.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import scala.Product;
import scala.Tuple1;
import scala.Tuple2;
import scala.Tuple3;
import scala.Tuple4;
import scala.Tuple5;
import scala.Tuple6;
import scala.Tuple7;
import scala.Tuple8;

public class TupleUtil {
    public static <K,V> Map<K,V> asMap(Collection<Tuple2<K,V>> tuples) {
        Map<K,V> map = new HashMap<K,V>();
        for(Tuple2<K,V> tuple : tuples) {
            map.put(tuple._1, tuple._2);
        }
        return map;
    }
    public static Product toTuple(Object... args) {
        switch(args.length) {
        case 1:
          return new Tuple1(args[0]);
        case 2:
          return new Tuple2(args[0], args[1]);
        case 3:
          return new Tuple3(args[0], args[1], args[2]);
        case 4:
          return new Tuple4(args[0], args[1], args[2], args[3]);
        case 5:
          return new Tuple5(args[0], args[1], args[2], args[3], args[4]);
        case 6:
          return new Tuple6(args[0], args[1], args[2], args[3], args[4], args[5]);
        case 7:
          return new Tuple7(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
        case 8:
          return new Tuple8(args[0], args[1], args[2], args[3],args[4], args[5], args[6], args[7]);
        default:
          throw new IllegalArgumentException("args.length <1 or >8!");
        }
      }

      public static Product toTupleWithHead(Object head, Object... args) {
        Object[] withHead = new Object[args.length +1];
        withHead[0] = head;
        System.arraycopy(args, 0, withHead, 1, args.length);
        return toTuple(withHead);
      }

      public static Object[] toArray(Product p) {
          Object[] oa = new Object[p.productArity()];
          for(int i = 0; i < p.productArity(); i++) {
              oa[i] = p.productElement(i);
          }
          return oa;
      }
      
      public static Class[] toSignature(Product p) {
          Class[] ca = new Class[p.productArity()];
          for(int i = 0; i < p.productArity(); i++) {
              ca[i] = p.productElement(i).getClass();
          }
          return ca;
      }
}
