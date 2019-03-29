RegexValidator
~~~
isValid
~~~
match
~~~
validate
~
return matcher.group(1);
~
if (count == 1) {
    return matcher.group(1);
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
