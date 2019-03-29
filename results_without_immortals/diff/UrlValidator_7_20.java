UrlValidator
~~~
getInstance
~~~
isValid
~
if (authority.contains(":")) {
    // but cannot allow trailing :
    return false;
}
~
return false;
~
return false;
~
// Special case - file: allows an empty authority
if (!"".equals(authority)) {
    if (authority.contains(":")) {
        // but cannot allow trailing :
        return false;
    }
}
~
return false;
~
return false;
~
if (value == null) {
    return false;
}
~
// Check the whole url address structure
Matcher urlMatcher = URL_PATTERN.matcher(value);
~
if (!urlMatcher.matches()) {
    return false;
}
~
String scheme = urlMatcher.group(PARSE_URL_SCHEME);
~
if (!isValidScheme(scheme)) {
    return false;
}
~
String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
~
if ("file".equals(scheme)) {
    // Special case - file: allows an empty authority
    if (!"".equals(authority)) {
        if (authority.contains(":")) {
            // but cannot allow trailing :
            return false;
        }
    }
} else {
    // Validate the authority
    if (!isValidAuthority(authority)) {
        return false;
    }
}
~
if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
    return false;
}
~
if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
    return false;
}
~
if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
    return false;
}
~
return true;
~~~
isValidScheme
~
if (scheme == null) {
    return false;
}
~
// TODO could be removed if external schemes were checked in the ctor before being stored
if (!SCHEME_PATTERN.matcher(scheme).matches()) {
    return false;
}
~
if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
    return false;
}
~~~
isValidAuthority
~
return false;
~
// try a hostname first since that's much more likely
DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
~
if (!domainValidator.isValid(hostLocation)) {
    // try an IPv4 address
    InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
    if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
        // isn't IPv4, so the URL is invalid
        return false;
    }
}
~
return false;
~
if (authority == null) {
    return false;
}
~
// convert to ASCII if possible
final String authorityASCII = DomainValidator.unicodeToASCII(authority);
~
Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
~
if (!authorityMatcher.matches()) {
    return false;
}
~
// We have to process IPV6 separately because that is parsed in a different group
String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
~
if (ipv6 != null) {
    InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
    if (!inetAddressValidator.isValidInet6Address(ipv6)) {
        return false;
    }
} else {
    String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
    // try a hostname first since that's much more likely
    DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
    if (!domainValidator.isValid(hostLocation)) {
        // try an IPv4 address
        InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
        if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
            // isn't IPv4, so the URL is invalid
            return false;
        }
    }
}
~
String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
~
if (extra != null && extra.trim().length() > 0) {
    return false;
}
~
return true;
~~~
isValidPath
~
return false;
~
return false;
~
if (// Trying to go via the parent dir
norm.startsWith("/../") || norm.equals("/..")) {
    // Trying to go to the parent dir
    return false;
}
~
return false;
~
return false;
~
if (path == null) {
    return false;
}
~
if (!PATH_PATTERN.matcher(path).matches()) {
    return false;
}
~
try {
    URI uri = new URI(null, null, path, null);
    String norm = uri.normalize().getPath();
    if (// Trying to go via the parent dir
    norm.startsWith("/../") || norm.equals("/..")) {
        // Trying to go to the parent dir
        return false;
    }
} catch (URISyntaxException e) {
    return false;
}
~
int slash2Count = countToken("//", path);
~
if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
    return false;
}
~
return true;
~~~
isValidQuery
~
if (query == null) {
    return true;
}
~~~
isValidFragment
~
if (fragment == null) {
    return true;
}
~~~
countToken
~~~
isOn
~~~
isOff
~~~
matchURL
