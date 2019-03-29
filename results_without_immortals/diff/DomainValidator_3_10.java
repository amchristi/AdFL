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
domain = unicodeToASCII(domain);
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
~
// skip possibly expensive processing
return input;
~
// fetch original last char
char lastChar = input.charAt(length - 1);
~
switch(lastChar) {
    case '\u002E':
        returnValue = ascii + ".";
        // "." full stop
        break;
    case '\u3002':
        returnValue = ascii + ".";
        // ideographic full stop
        break;
    case '\uFF0E':
        returnValue = ascii + ".";
        // fullwidth full stop
        break;
    case // halfwidth ideographic full stop
    '\uFF61':
        returnValue = ascii + ".";
        break;
    // restore the missing stop
    default:
        returnValue = ascii;
        break;
}
~
// input is not valid
return input;
~
if (isOnlyASCII(input)) {
    // skip possibly expensive processing
    return input;
}
~
try {
    final String ascii = IDN.toASCII(input);
    if (IDNBUGHOLDER.IDN_TOASCII_PRESERVES_TRAILING_DOTS) {
        return ascii;
    }
    final int length = input.length();
    if (length == 0) {
        // check there is a last character
        return input;
    }
    // fetch original last char
    char lastChar = input.charAt(length - 1);
    switch(lastChar) {
        case '\u002E':
            returnValue = ascii + ".";
            // "." full stop
            break;
        case '\u3002':
            returnValue = ascii + ".";
            // ideographic full stop
            break;
        case '\uFF0E':
            returnValue = ascii + ".";
            // fullwidth full stop
            break;
        case // halfwidth ideographic full stop
        '\uFF61':
            returnValue = ascii + ".";
            break;
        // restore the missing stop
        default:
            returnValue = ascii;
            break;
    }
} catch (IllegalArgumentException e) {
    // input is not valid
    return input;
}
~
return returnValue;
~~~
isOnlyASCII
~
if (input == null) {
    return true;
}
~
for (int i = 0; i < input.length(); i++) {
    if (input.charAt(i) > 0x7F) {
        return false;
    }
}
~~~
arrayContains
