Engine
~~~
getUberspect
~
if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
    return UberspectHolder.UBERSPECT;
}
~~~
getUberspect
~
if ((logger == null || logger.equals(LogFactory.getLog(JexlEngine.class))) && (strategy == null || strategy == JexlUberspect.JEXL_STRATEGY)) {
    return UberspectHolder.UBERSPECT;
}
~~~
getArithmetic
~~~
isDebug
~~~
isSilent
~~~
isStrict
~~~
setClassLoader
~~~
getCharset
~~~
createJxltEngine
~~~
putThreadLocal
~
CONTEXT.set(tls);
~~~
createCache
~~~
clearCache
~
cache.clear();
~
if (cache != null) {
    cache.clear();
}
~
synchronized (parser) {
    if (cache != null) {
        cache.clear();
    }
}
~~~
createInterpreter
~~~
createScript
~
throw new NullPointerException("source is null");
~
if (scriptText == null) {
    throw new NullPointerException("source is null");
}
~~~
createExpression
~
if (expression == null) {
    throw new NullPointerException("source is null");
}
~
if (info == null && debug) {
    info = createInfo();
}
~~~
getProperty
~~~
getProperty
~~~
setProperty
~
setProperty(null, bean, expr, value);
~~~
setProperty
~
setProperty(null, bean, expr, value);
~~~
invokeMethod
~~~
newInstance
~~~
newInstance
~~~
doCreateInstance
~~~
getVariables
~
getVariables(script, script, collector);
~~~
getVariables
~
getVariables(script, script, collector);
~~~
getParameters
~~~
getLocalVariables
~~~
parse
~
if (cached) {
    script = cache.get(src);
    if (script != null) {
        Scope f = script.getScope();
        if ((f == null && scope == null) || (f != null && f.equals(scope))) {
            return script;
        }
    }
}
~
if (cached) {
    cache.put(src, script);
}
~
final boolean cached = src.length() < cacheThreshold && cache != null;
~
synchronized (parser) {
    if (cached) {
        script = cache.get(src);
        if (script != null) {
            Scope f = script.getScope();
            if ((f == null && scope == null) || (f != null && f.equals(scope))) {
                return script;
            }
        }
    }
    script = parser.parse(info, src, scope, registers, expression);
    if (cached) {
        cache.put(src, script);
    }
}
~
return script;
~~~
trimSource
~~~
jxlt
~
jxlt = e;
