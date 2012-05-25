package com.hartenbower.util;

import java.util.Random;


public class StringUtil {
    final static int DEFAULT_HEAD_COUNT = 100;
    final static int DEFAULT_TAIL_COUNT = DEFAULT_HEAD_COUNT;

    /**
     * Compares two Strings, regardless null or not.
     * Case sensitive.
     * 
     * @param src first string
     * @param dest second string
     * 
     * @return true if equals, false otherwise.
     */
    public static boolean compare(String src, String dest) {
        if (src == null) {
            return dest == null;
        } else {
            return src.equals(dest);
        }
    }
    /**Given any Object array this will return a String array by calling toString on each object.
     * 
     * @param original
     * @return
     */
    public static String[] convertArray(Object[] original){
        String[] result = new String[original.length];
        for (int i = 0; i < original.length; i++) {
                        result[i] = original[i].toString();
                }
        return result;
    }
    
    public static String upperCase(String original){
        return original.toUpperCase();
    }
    
    public static String randomString() {
        return randomString(8);
    }
    
    public static String randomString(int count) {
        Random rnd = new Random(System.currentTimeMillis());
        StringBuffer sb = new StringBuffer();
        for(int i = 0; i< count; i++) {
                sb.append((char) (rnd.nextInt(25) + 97));
        }
        return sb.toString();
    }
    
    public static String dePackage(String s) {
        if(s == null || s.length() < 2 || s.indexOf(".") == -1 ) {
                return s;
        }
        return s.substring(s.lastIndexOf(".") + 1);
    }
    
    public static int getLevenshteinDistance(String s, String t) {
        if (s == null || t == null) {
            throw new IllegalArgumentException("Strings must not be null");
        }

        /*
           The difference between this impl. and the previous is that, rather 
           than creating and retaining a matrix of size s.length()+1 by t.length()+1, 
           we maintain two single-dimensional arrays of length s.length()+1.  The first, d,
           is the 'current working' distance array that maintains the newest distance cost
           counts as we iterate through the characters of String s.  Each time we increment
           the index of String t we are comparing, d is copied to p, the second int[].  Doing so
           allows us to retain the previous cost counts as required by the algorithm (taking 
           the minimum of the cost count to the left, up one, and diagonally up and to the left
           of the current cost count being calculated).  (Note that the arrays aren't really 
           copied anymore, just switched...this is clearly much better than cloning an array 
           or doing a System.arraycopy() each time  through the outer loop.)

           Effectively, the difference between the two implementations is this one does not 
           cause an out of memory condition when calculating the LD over two very large strings.
         */

        int n = s.length(); // length of s
        int m = t.length(); // length of t

        if (n == 0) {
            return m;
        } else if (m == 0) {
            return n;
        }

        if (n > m) {
            // swap the input strings to consume less memory
            String tmp = s;
            s = t;
            t = tmp;
            n = m;
            m = t.length();
        }

        int p[] = new int[n+1]; //'previous' cost array, horizontally
        int d[] = new int[n+1]; // cost array, horizontally
        int _d[]; //placeholder to assist in swapping p and d

        // indexes into strings s and t
        int i; // iterates through s
        int j; // iterates through t

        char t_j; // jth character of t

        int cost; // cost

        for (i = 0; i<=n; i++) {
            p[i] = i;
        }

        for (j = 1; j<=m; j++) {
            t_j = t.charAt(j-1);
            d[0] = j;

            for (i=1; i<=n; i++) {
                cost = s.charAt(i-1)==t_j ? 0 : 1;
                // minimum of cell to the left+1, to the top+1, diagonally left and up +cost
                d[i] = Math.min(Math.min(d[i-1]+1, p[i]+1),  p[i-1]+cost);
            }

            // copy current distance counts to 'previous row' distance counts
            _d = p;
            p = d;
            d = _d;
        }

        // our last action in the above loop was to switch d and p, so p now 
        // actually has the most recent cost counts
        return p[n];
    }

    /**
     * @param str target str
     * @param count optional character count, otherwise first DEFAULT_HEAD_COUNT chars returned
     * @return
     */
    public static String head(String str, int... count) {
        int len  = str.length();
        if( count != null && count.length > 0) {
            return (count[0] < len ? str.substring(0, count[0]) : str);
        }
        return len > DEFAULT_HEAD_COUNT ? str.substring(0,DEFAULT_HEAD_COUNT) : str;
    }

    /**
     * @param str target str
     * @param count optional character count, otherwise last DEFAULT_HEAD_COUNT chars returned
     * @return
     */
    public static String tail(String str, int... count) {
        int len  = str.length();
        if( count != null && count.length > 0) {
            return (count[0] < len ? str.substring(len - count[0]) : str);
        }
        return len > DEFAULT_TAIL_COUNT ? str.substring(len - DEFAULT_TAIL_COUNT) : str;
    }
    
    public static String toString(int number, int spaces) {
        String ret = "" + number;
        while(ret.length() < spaces) {
            ret = "0" + ret;
        }
        return ret;
    }
    
}
