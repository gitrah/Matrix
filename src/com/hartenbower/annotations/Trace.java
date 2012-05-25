package com.hartenbower.annotations;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface Trace {
    boolean value() default false;  // true -> indicate executing thread
}
