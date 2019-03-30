package com.osustar;

import java.lang.annotation.ElementType;


/**
 * Created by ubuntu on 11/28/16.
 */

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface MethodLabel {

    int level() default 0;

}