NodeCreateRule
~~~
begin
~
element = doc.createElementNS(namespaceURI, name);
~
for (int i = 0; i < attributes.getLength(); i++) {
    element.setAttributeNS(attributes.getURI(i), attributes.getQName(i), attributes.getValue(i));
}
~
for (int i = 0; i < attributes.getLength(); i++) {
    element.setAttribute(attributes.getQName(i), attributes.getValue(i));
}
~~~
end
~
getDigester().pop();
