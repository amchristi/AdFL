NodeCreateRule
~~~
begin
~
element.setAttribute(attributes.getQName(i), attributes.getValue(i));
~
for (int i = 0; i < attributes.getLength(); i++) {
    element.setAttribute(attributes.getQName(i), attributes.getValue(i));
}
~~~
end
~
getDigester().pop();
