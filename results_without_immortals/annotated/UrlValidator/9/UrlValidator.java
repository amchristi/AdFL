/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.validator.routines;

import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.*;

/**
 * <p><b>URL Validation</b> routines.</p>
 * Behavior of validation is modified by passing in options:
 * <ul>
 * <li>ALLOW_2_SLASHES - [FALSE]  Allows double '/' characters in the path
 * component.</li>
 * <li>NO_FRAGMENT- [FALSE]  By default fragments are allowed, if this option is
 * included then fragments are flagged as illegal.</li>
 * <li>ALLOW_ALL_SCHEMES - [FALSE] By default only http, https, and ftp are
 * considered valid schemes.  Enabling this option will let any scheme pass validation.</li>
 * </ul>
 *
 * <p>Originally based in on php script by Debbie Dyer, validation.php v1.2b, Date: 03/07/02,
 * http://javascript.internet.com. However, this validation now bears little resemblance
 * to the php original.</p>
 * <pre>
 * Example of usage:
 * Construct a UrlValidator with valid schemes of "http", and "https".
 *
 * String[] schemes = {"http","https"}.
 * UrlValidator urlValidator = new UrlValidator(schemes);
 * if (urlValidator.isValid("ftp://foo.bar.com/")) {
 * System.out.println("url is valid");
 * } else {
 * System.out.println("url is invalid");
 * }
 *
 * prints "url is invalid"
 * If instead the default constructor is used.
 *
 * UrlValidator urlValidator = new UrlValidator();
 * if (urlValidator.isValid("ftp://foo.bar.com/")) {
 * System.out.println("url is valid");
 * } else {
 * System.out.println("url is invalid");
 * }
 *
 * prints out "url is valid"
 * </pre>
 *
 * @see
 * <a href="http://www.ietf.org/rfc/rfc2396.txt">
 * Uniform Resource Identifiers (URI): Generic Syntax
 * </a>
 *
 * @version $Revision: 1713573 $
 * @since Validator 1.4
 */
public class UrlValidator implements Serializable {

    private static final long serialVersionUID = 7557161713937335013L;

    /**
     * Allows all validly formatted schemes to pass validation instead of
     * supplying a set of valid schemes.
     */
    public static final long ALLOW_ALL_SCHEMES = 1 << 0;

    /**
     * Allow two slashes in the path component of the URL.
     */
    public static final long ALLOW_2_SLASHES = 1 << 1;

    /**
     * Enabling this options disallows any URL fragments.
     */
    public static final long NO_FRAGMENTS = 1 << 2;

    /**
     * Allow local URLs, such as http://localhost/ or http://machine/ .
     * This enables a broad-brush check, for complex local machine name
     * validation requirements you should create your validator with
     * a {@link RegexValidator} instead ({@link #UrlValidator(RegexValidator, long)})
     */
    public static final long ALLOW_LOCAL_URLS = 1 << 3;

    /**
     * This expression derived/taken from the BNF for URI (RFC2396).
     */
    private static final String URL_REGEX = "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?";

    // 12            3  4          5       6   7        8 9
    private static final Pattern URL_PATTERN = Pattern.compile(URL_REGEX);

    /**
     * Schema/Protocol (ie. http:, ftp:, file:, etc).
     */
    private static final int PARSE_URL_SCHEME = 2;

    /**
     * Includes hostname/ip and port number.
     */
    private static final int PARSE_URL_AUTHORITY = 4;

    private static final int PARSE_URL_PATH = 5;

    private static final int PARSE_URL_QUERY = 7;

    private static final int PARSE_URL_FRAGMENT = 9;

    /**
     * Protocol scheme (e.g. http, ftp, https).
     */
    private static final String SCHEME_REGEX = "^\\p{Alpha}[\\p{Alnum}\\+\\-\\.]*";

    private static final Pattern SCHEME_PATTERN = Pattern.compile(SCHEME_REGEX);

    // Drop numeric, and  "+-." for now
    // TODO does not allow for optional userinfo.
    // Validation of character set is done by isValidAuthority
    // allows for IPV4 but not IPV6
    private static final String AUTHORITY_CHARS_REGEX = "\\p{Alnum}\\-\\.";

    // do this as separate match because : could cause ambiguity with port prefix
    private static final String IPV6_REGEX = "[0-9a-fA-F:]+";

    // userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
    // unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
    // sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
    // We assume that password has the same valid chars as user info
    private static final String USERINFO_CHARS_REGEX = "[a-zA-Z0-9%-._~!$&'()*+,;=]";

    // since neither ':' nor '@' are allowed chars, we don't need to use non-greedy matching
    private static final String USERINFO_FIELD_REGEX = // At least one character for the name
    USERINFO_CHARS_REGEX + "+:" + USERINFO_CHARS_REGEX + // password may be absent
    "*@";

    private static final String AUTHORITY_REGEX = "(?:\\[(" + IPV6_REGEX + ")\\]|(?:(?:" + USERINFO_FIELD_REGEX + ")?([" + AUTHORITY_CHARS_REGEX + "]*)))(:\\d*)?(.*)?";

    // 1                          e.g. user:pass@          2                                   3       4
    private static final Pattern AUTHORITY_PATTERN = Pattern.compile(AUTHORITY_REGEX);

    private static final int PARSE_AUTHORITY_IPV6 = 1;

    // excludes userinfo, if present
    private static final int PARSE_AUTHORITY_HOST_IP = 2;

    // Not needed, because it is validated by AUTHORITY_REGEX
    // private static final int PARSE_AUTHORITY_PORT = 3;
    /**
     * Should always be empty. The code currently allows spaces.
     */
    private static final int PARSE_AUTHORITY_EXTRA = 4;

    private static final String PATH_REGEX = "^(/[-\\w:@&?=+,.!/~*'%$_;\\(\\)]*)?$";

    private static final Pattern PATH_PATTERN = Pattern.compile(PATH_REGEX);

    private static final String QUERY_REGEX = "^(.*)$";

    private static final Pattern QUERY_PATTERN = Pattern.compile(QUERY_REGEX);

    /**
     * Holds the set of current validation options.
     */
    private final long options;

    /**
     * The set of schemes that are allowed to be in a URL.
     */
    // Must be lower-case
    private final Set<String> allowedSchemes;

    /**
     * Regular expressions used to manually validate authorities if IANA
     * domain name validation isn't desired.
     */
    private final RegexValidator authorityValidator;

    /**
     * If no schemes are provided, default to this set.
     */
    // Must be lower-case
    private static final String[] DEFAULT_SCHEMES = { "http", "https", "ftp" };

    /**
     * Singleton instance of this class with default schemes and options.
     */
    private static final UrlValidator DEFAULT_URL_VALIDATOR = new UrlValidator();

    /**
     * Returns the singleton instance of this class with default schemes and options.
     * @return singleton instance with default schemes and options
     */
    public static UrlValidator getInstance() {
        writelineStatic("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "7220378b-8ed4-4b20-8195-e03443a26cf1");
        return DEFAULT_URL_VALIDATOR;
    }

    /**
     * Create a UrlValidator with default properties.
     */
    public UrlValidator() {
        this(null);
    }

    /**
     * Behavior of validation is modified by passing in several strings options:
     * @param schemes Pass in one or more url schemes to consider valid, passing in
     * a null will default to "http,https,ftp" being valid.
     * If a non-null schemes is specified then all valid schemes must
     * be specified. Setting the ALLOW_ALL_SCHEMES option will
     * ignore the contents of schemes.
     */
    public UrlValidator(String[] schemes) {
        this(schemes, 0L);
    }

    /**
     * Initialize a UrlValidator with the given validation options.
     * @param options The options should be set using the public constants declared in
     * this class.  To set multiple options you simply add them together.  For example,
     * ALLOW_2_SLASHES + NO_FRAGMENTS enables both of those options.
     */
    public UrlValidator(long options) {
        this(null, null, options);
    }

    /**
     * Behavior of validation is modified by passing in options:
     * @param schemes The set of valid schemes. Ignored if the ALLOW_ALL_SCHEMES option is set.
     * @param options The options should be set using the public constants declared in
     * this class.  To set multiple options you simply add them together.  For example,
     * ALLOW_2_SLASHES + NO_FRAGMENTS enables both of those options.
     */
    public UrlValidator(String[] schemes, long options) {
        this(schemes, null, options);
    }

    /**
     * Initialize a UrlValidator with the given validation options.
     * @param authorityValidator Regular expression validator used to validate the authority part
     * This allows the user to override the standard set of domains.
     * @param options Validation options. Set using the public constants of this class.
     * To set multiple options, simply add them together:
     * <p><code>ALLOW_2_SLASHES + NO_FRAGMENTS</code></p>
     * enables both of those options.
     */
    public UrlValidator(RegexValidator authorityValidator, long options) {
        this(null, authorityValidator, options);
    }

    /**
     * Customizable constructor. Validation behavior is modifed by passing in options.
     * @param schemes the set of valid schemes. Ignored if the ALLOW_ALL_SCHEMES option is set.
     * @param authorityValidator Regular expression validator used to validate the authority part
     * @param options Validation options. Set using the public constants of this class.
     * To set multiple options, simply add them together:
     * <p><code>ALLOW_2_SLASHES + NO_FRAGMENTS</code></p>
     * enables both of those options.
     */
    public UrlValidator(String[] schemes, RegexValidator authorityValidator, long options) {
        this.options = options;
        if (isOn(ALLOW_ALL_SCHEMES)) {
            allowedSchemes = Collections.emptySet();
        } else {
            if (schemes == null) {
                schemes = DEFAULT_SCHEMES;
            }
            allowedSchemes = new HashSet<String>(schemes.length);
            for (int i = 0; i < schemes.length; i++) {
                allowedSchemes.add(schemes[i].toLowerCase(Locale.ENGLISH));
            }
        }
        this.authorityValidator = authorityValidator;
    }

    /**
     * <p>Checks if a field has a valid url address.</p>
     *
     * Note that the method calls #isValidAuthority()
     * which checks that the domain is valid.
     *
     * @param value The value validation is being performed on.  A <code>null</code>
     * value is considered invalid.
     * @return true if the url is valid.
     */
    public boolean isValid(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "d5418ecf-8685-4d6f-812b-6def92999d99");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "932c8f71-86cb-449d-be44-f283d8ca76c1");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "d5fa38ba-6bf4-44ae-864a-b4b2522b80a1");
        // Check the whole url address structure
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "55aa883a-82c2-4989-9383-3f8664c8a606");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "11b2fee5-7025-422c-80f7-ab1e80cf8cc7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "8802a13c-5b5e-4568-9896-1b289d021d6e");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "17e703d7-f89e-46af-a62b-913e8ca64819");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "1635d943-661c-45a7-bfef-bc517b410a37");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "84daace3-aef7-47de-adae-c7b94b8d6422");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "5f19bcff-a1f0-46b0-ae53-453c6359d43f");
        if ("file".equals(scheme)) {
            // drop through to continue validation
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "0526d2b7-8768-4b0e-b818-b86383a6155d");
            // Special case - file: allows an empty authority
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "7366f996-83f8-4475-8cac-1d6548933572");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "3c7cf197-4795-4e66-ba1c-82327c5ac8ba");
                    // but cannot allow trailing :
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "adbee080-5ada-49a5-a308-47b2de903cb8");
            // Validate the authority
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "94c2baec-9791-4857-bb0b-9ba232f6030a");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "872c432b-db8c-4ea8-a2ef-53cfe0c4b80b");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "83405a36-cab4-4009-8ac3-d79c5c704a0c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "1cab4f65-25f5-4681-a93f-2722cd8d5e5a");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "c54e0330-90f3-41e2-82c6-17222d81318e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "ad261444-7b4f-4797-abfe-168d93d21a6d");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "b3f79c57-33ed-44c1-b6f5-d97427f373a1");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "17f71741-9d88-491b-bce0-9dd0c7c3a3e4");
        return true;
    }

    /**
     * Validate scheme. If schemes[] was initialized to a non null,
     * then only those schemes are allowed.
     * Otherwise the default schemes are "http", "https", "ftp".
     * Matching is case-blind.
     * @param scheme The scheme to validate.  A <code>null</code> value is considered
     * invalid.
     * @return true if valid.
     */
    protected boolean isValidScheme(String scheme) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "5df2dae4-69e8-4064-a5e7-7d02a72fb5f4");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "365b03a8-cb53-4a15-888d-e3ad87870370");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "5a2ff826-5088-4808-8517-3eff28b05c81");
        // TODO could be removed if external schemes were checked in the ctor before being stored
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "7c8aad64-fdb4-46c4-89dd-08b7ebcdb2e0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "e2088e14-7800-402e-9f8f-94066fd8a576");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "a45c1daf-5979-48b7-b2a1-6e520e325151");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "417abb2a-73f0-42d2-a60f-9590c94cc1f9");
        return true;
    }

    /**
     * Returns true if the authority is properly formatted.  An authority is the combination
     * of hostname and port.  A <code>null</code> authority value is considered invalid.
     * Note: this implementation validates the domain unless a RegexValidator was provided.
     * If a RegexValidator was supplied and it matches, then the authority is regarded
     * as valid with no further checks, otherwise the method checks against the
     * AUTHORITY_PATTERN and the DomainValidator (ALLOW_LOCAL_URLS)
     * @param authority Authority value to validate, alllows IDN
     * @return true if authority (hostname and port) is valid.
     */
    protected boolean isValidAuthority(String authority) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "93c7e365-8f4b-4b1d-947e-d7514ef5dda3");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "af48f9d7-8132-45d5-bd57-a63274633ccb");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "7a90f87a-6836-45de-9062-38194850945e");
        // check manual authority validation if specified
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "08f9716d-b983-48c9-8806-3febc7a217ba");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "b1ad9e20-b819-4492-9de1-445844febc28");
        // convert to ASCII if possible
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "b192bfd0-f1e6-42bc-b4ba-2b1a685ede23");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "75da5949-e02f-48da-9778-797ec065bc6d");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "bd2e38b7-ae82-4d37-807f-bac2fec9cef6");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "d2ed314f-e58e-4828-8083-8d652b48925d");
        // We have to process IPV6 separately because that is parsed in a different group
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "9f6968f3-f447-43a6-8440-719103b1aa3d");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "e5049a96-ef8c-4edf-b310-b1b77a189371");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "a560d1ee-02c3-4cb9-825d-7e3442152877");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "c467884b-1272-48c3-bbcc-741c60866f7b");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "636e2a0d-0b92-4016-b629-a8d7ae788040");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "a5885768-dec4-41b9-aa11-70b4649d60fe");
            // check if authority is hostname or IP address:
            // try a hostname first since that's much more likely
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "43cf72e4-1740-42e0-b165-ae75e4e3e340");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "865c922a-e90e-43f2-9297-cf039cf782a8");
                // try an IPv4 address
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "3ced46b9-a7fb-4592-99de-93eb480d9c61");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "246c2b91-2bc0-4ccc-883b-37a0bb4f4571");
                    // isn't IPv4, so the URL is invalid
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "112b2772-362a-4734-b7d2-c1a8ee8856f8");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "b35472e6-67b9-441c-bbf6-76e69556ec61");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "6db9b7a0-b120-472e-8918-ee07f864a2a0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "84668d74-0505-4cba-944a-fb29df26a416");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "508a0714-f21b-4cf2-9b30-d01f4a456c90");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "7980d54f-c635-47a3-951d-36f8bbf8ef3d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "582cb07a-a582-4825-8daf-e21b539f0593");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "64c1a346-cfc0-4d47-8d9d-6910d71da837");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "c3182f79-fcbd-4a49-ab4a-25abcacba716");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "917725b5-83db-49ef-b85c-d470278012f8");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "012d4a7c-1dac-4ce9-a6f6-c3a1ba72ec2e");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "924cea1f-a048-4210-8b0f-f243cb7aef0b");
            if (// Trying to go via the parent dir
            norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "46c0dba4-04e8-4627-aab4-5e1f5ac1e00d");
                // Trying to go to the parent dir
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "5aed33fe-54ec-4eb5-8f13-6f6629ac1054");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "0f09eb64-0072-40e0-8e82-a869a596f223");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "45a4c35f-e8ab-41ff-a7d6-71b0f2587df7");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "4201ff48-ecd9-4580-8f2c-7b7edb32bd62");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "609f64c7-8657-4c23-83e1-f43419b70dce");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "3508ca7d-d6a8-4ba7-8158-2d1059a92904");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "13eeebd7-726a-43ad-a2d5-ba517b7c16fb");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "5a57dfcf-9ff7-46bb-90ab-698b1f516243");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "b531caf8-43a8-466b-9d90-819fc6644ac4");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "e48e510e-77f5-42ce-8a23-8615e040abad");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "417f8269-9062-4f16-9ff8-fa1445ba383a");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "3473c322-9795-42ff-b478-ecac10cadca9");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "f6510568-7dc6-477e-9a7f-c8df895fb730");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "c6daabd3-53da-4dae-bce8-ee69fa02800c");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "6a555423-83c3-4ff8-a1de-355d22221208");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "14bd0845-d4ab-4138-ba8c-ae1b17502afb");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "dac13543-48ce-4b68-b31b-0cf350769f05");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "f5b55dd9-fcaa-4ce8-9e2f-5db18ea41d29");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "35ff9bc6-2cd5-481f-be0a-e03fb1df28ca");
        return count;
    }

    /**
     * Tests whether the given flag is on.  If the flag is not a power of 2
     * (ie. 3) this tests whether the combination of flags is on.
     *
     * @param flag Flag value to check.
     *
     * @return whether the specified flag value is on.
     */
    private boolean isOn(long flag) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "629b1c18-689d-4392-8092-df3f5013ac1c");
        return (options & flag) > 0;
    }

    /**
     * Tests whether the given flag is off.  If the flag is not a power of 2
     * (ie. 3) this tests whether the combination of flags is off.
     *
     * @param flag Flag value to check.
     *
     * @return whether the specified flag value is off.
     */
    private boolean isOff(long flag) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "e7af8364-ccbb-42b4-807c-03a96e14a2ab");
        return (options & flag) == 0;
    }

    // Unit test access to pattern matcher
    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_9_10.coverage", "c905be66-f0fc-435b-bd09-67280ab61688");
        return URL_PATTERN.matcher(value);
    }

    void writeline(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    static void writelineStatic(String fullFilePath, String text) {
        try {
            File file = new File(fullFilePath);
            FileWriter fileWriter = new FileWriter(file, true);
            BufferedWriter output = new BufferedWriter(fileWriter);
            output.append(text);
            output.newLine();
            output.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
