package com.hartenbower.util;

import java.text.NumberFormat;

public class NumberFormats {
    public static NumberFormat int2 = NumberFormat.getInstance();
    public static NumberFormat decimal3 = NumberFormat.getNumberInstance();
    public static NumberFormat dec2 = NumberFormat.getNumberInstance();

    static {
        int2.setMinimumIntegerDigits(2);
        int2.setMaximumIntegerDigits(2);
        decimal3.setMinimumFractionDigits(3);
        decimal3.setMaximumFractionDigits(3);
        dec2.setMinimumFractionDigits(2);
        dec2.setMaximumFractionDigits(2);
    }

    public static String decimal3(double v) {
        return decimal3.format(v);
    }

    public static String dec2(double v) {
        return dec2.format(v);
    }

    public static String int2(int v) {
        return int2.format(v);
    }
}