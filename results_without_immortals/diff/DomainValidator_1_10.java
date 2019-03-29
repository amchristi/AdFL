DomainValidator
~~~
getInstance
~~~
getInstance
~~~
isValid
~
return false;
~
return false;
~
if (domain == null) {
    return false;
}
~
// checks in the regexes below
if (domain.length() > 253) {
    return false;
}
~~~
isValidDomainSyntax
~
if (domain == null) {
    return false;
}
~
domain = unicodeToASCII(domain);
~
// checks in the regexes below
if (domain.length() > 253) {
    return false;
}
~~~
isValidTld
~
tld = unicodeToASCII(tld);
~~~
isValidInfrastructureTld
~~~
isValidGenericTld
~~~
isValidCountryCodeTld
~~~
isValidLocalTld
~~~
chompLeadingDot
~~~
clearTLDOverrides
~
COUNTRY_CODE_TLDS_PLUS = EMPTY_STRING_ARRAY;
~
COUNTRY_CODE_TLDS_MINUS = EMPTY_STRING_ARRAY;
~
GENERIC_TLDS_MINUS = EMPTY_STRING_ARRAY;
~~~
updateTLDOverride
~
Arrays.sort(copy);
~
switch(table) {
    case COUNTRY_CODE_MINUS:
        COUNTRY_CODE_TLDS_MINUS = copy;
        break;
    case COUNTRY_CODE_PLUS:
        COUNTRY_CODE_TLDS_PLUS = copy;
        break;
    case GENERIC_MINUS:
        GENERIC_TLDS_MINUS = copy;
        break;
    case GENERIC_PLUS:
        GENERIC_TLDS_PLUS = copy;
        break;
}
~~~
unicodeToASCII
~
// check there is a last character
return input;
~
returnValue = ascii + ".";
~
// "." full stop
break;
~
returnValue = ascii + ".";
~
// ideographic full stop
break;
~
returnValue = ascii + ".";
~
// fullwidth full stop
break;
~
returnValue = ascii + ".";
~
break;
~
returnValue = ascii;
~
break;
~~~
isOnlyASCII
~~~
arrayContains
