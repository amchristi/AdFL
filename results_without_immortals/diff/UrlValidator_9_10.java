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
~~~
isValidAuthority
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
~~~
countToken
~~~
isOn
~~~
isOff
~~~
matchURL
