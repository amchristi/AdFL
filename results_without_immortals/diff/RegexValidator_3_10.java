RegexValidator
~~~
isValid
~
return false;
~
if (value == null) {
    return false;
}
~~~
match
~
return null;
~
if (value == null) {
    return null;
}
~~~
validate
~
return matcher.group(1);
~
if (count == 1) {
    return matcher.group(1);
}
~
return null;
~
if (value == null) {
    return null;
}
~~~
toString
~
buffer.append("RegexValidator{");
~
for (int i = 0; i < patterns.length; i++) {
    if (i > 0) {
        buffer.append(",");
    }
    buffer.append(patterns[i].pattern());
}
~
buffer.append("}");
