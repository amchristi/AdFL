CallMethodRule
~~~
getUseExactMatch
~~~
setUseExactMatch
~
this.useExactMatch = useExactMatch;
~~~
setDigester
~
// call superclass
super.setDigester(digester);
~
// if necessary, load parameter classes
if (this.paramClassNames != null) {
    this.paramTypes = new Class<?>[paramClassNames.length];
    for (int i = 0; i < this.paramClassNames.length; i++) {
        try {
            this.paramTypes[i] = digester.getClassLoader().loadClass(this.paramClassNames[i]);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(format("[CallMethodRule] Cannot load class %s at position %s", this.paramClassNames[i], i), e);
        }
    }
}
~~~
begin
~
fill(parameters, null);
~~~
body
~~~
end
~
for (int i = 0, size = parameters.length; i < size; i++) {
    getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
}
~
// so skip the method call
if (bodyText == null) {
    return;
}
~
if (paramTypes.length == 0) {
    paramTypes = new Class[] { String.class };
}
~
// Nothing special needs to be done here.
parameters = new Object[0];
~
paramTypes = new Class<?>[0];
~
paramValues[i] = convert((String) parameters[i], paramTypes[i]);
~
paramValues[i] = parameters[i];
~
formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
~
if (getDigester().getLogger().isTraceEnabled()) {
    for (int i = 0, size = parameters.length; i < size; i++) {
        getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
    }
}
~
throw new SAXException(format("[CallMethodRule]{%s} Call target is null (targetOffset=%s, stackdepth=%s)", getDigester().getMatch(), targetOffset, getDigester().getCount()));
~
Formatter formatter = new Formatter().format("[CallMethodRule]{%s} Call %s.%s(", getDigester().getMatch(), target.getClass().getName(), methodName);
~
for (int i = 0; i < paramValues.length; i++) {
    formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
}
~
formatter.format(")");
~
getDigester().getLogger().debug(formatter.toString());
~
// invoke using exact match
result = invokeExactMethod(target, methodName, paramValues, paramTypes);
~
// invoke using fuzzier match
result = invokeMethod(target, methodName, paramValues, paramTypes);
~
// Retrieve or construct the parameter values array
Object[] parameters;
~
if (paramCount > 0) {
    parameters = getDigester().popParams();
    if (getDigester().getLogger().isTraceEnabled()) {
        for (int i = 0, size = parameters.length; i < size; i++) {
            getDigester().getLogger().trace(format("[CallMethodRule]{%s} parameters[%s]=%s", getDigester().getMatch(), i, parameters[i]));
        }
    }
    // parameter, but it always has been so we can't change it now.
    if (paramCount == 1 && parameters[0] == null) {
        return;
    }
} else if (paramTypes != null && paramTypes.length != 0) {
    // so skip the method call
    if (bodyText == null) {
        return;
    }
    parameters = new Object[] { bodyText };
    if (paramTypes.length == 0) {
        paramTypes = new Class[] { String.class };
    }
} else {
    // Nothing special needs to be done here.
    parameters = new Object[0];
    paramTypes = new Class<?>[0];
}
~
// the specified paramType is not String.
Object[] paramValues = new Object[paramTypes.length];
~
for (int i = 0; i < paramTypes.length; i++) {
    // for non-stringy param types
    if (parameters[i] == null || (parameters[i] instanceof String && !String.class.isAssignableFrom(paramTypes[i]))) {
        paramValues[i] = convert((String) parameters[i], paramTypes[i]);
    } else {
        paramValues[i] = parameters[i];
    }
}
~
// Determine the target object for the method call
Object target;
~
if (targetOffset >= 0) {
    target = getDigester().peek(targetOffset);
} else {
    target = getDigester().peek(getDigester().getCount() + targetOffset);
}
~
if (target == null) {
    throw new SAXException(format("[CallMethodRule]{%s} Call target is null (targetOffset=%s, stackdepth=%s)", getDigester().getMatch(), targetOffset, getDigester().getCount()));
}
~
// Invoke the required method on the top object
if (getDigester().getLogger().isDebugEnabled()) {
    Formatter formatter = new Formatter().format("[CallMethodRule]{%s} Call %s.%s(", getDigester().getMatch(), target.getClass().getName(), methodName);
    for (int i = 0; i < paramValues.length; i++) {
        formatter.format("%s%s/%s", (i > 0 ? ", " : ""), paramValues[i], paramTypes[i].getName());
    }
    formatter.format(")");
    getDigester().getLogger().debug(formatter.toString());
}
~
Object result = null;
~
if (useExactMatch) {
    // invoke using exact match
    result = invokeExactMethod(target, methodName, paramValues, paramTypes);
} else {
    // invoke using fuzzier match
    result = invokeMethod(target, methodName, paramValues, paramTypes);
}
~
processMethodCallResult(result);
~~~
finish
~
bodyText = null;
~~~
processMethodCallResult
~~~
toString
~
if (paramTypes != null) {
    for (int i = 0; i < paramTypes.length; i++) {
        formatter.format("%s%s", (i > 0 ? ", " : ""), (paramTypes[i] != null ? paramTypes[i].getName() : "null"));
    }
}
~
formatter.format("}]");
