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
        writelineStatic("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "17bd1d20-2785-422a-b0f5-6429660edb92");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "af436f38-6abe-4deb-a069-35e23b51f69e");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "be4243dc-4390-4355-9fc4-15816f6e2188");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "aab4bcb0-aae0-4933-b4d9-f2ea0f2585b5");
        // Check the whole url address structure
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "acf092bc-7989-4fc7-8876-5730647335c3");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "0e4c3819-75ab-4744-abda-7d4dd57ea652");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "2df3bb1a-0667-42aa-8ac5-f3beb2215960");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "b16e690f-ae40-4664-874c-dc8327651a6e");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "8f5ca61f-c320-4ba2-86d0-850ffd594464");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "7785e91d-ea69-48eb-8eae-b22a1ebc5513");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "3d7b005b-0997-4372-9f46-3c070fea0622");
        if ("file".equals(scheme)) {
            // drop through to continue validation
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "33e81cf0-564e-4d8b-a8d7-71322a5d0fad");
            // Special case - file: allows an empty authority
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "5f6a5bfc-6e0c-40ad-bff4-02db599172f4");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f8dcb65a-383d-41e5-b50e-b218ada9c156");
                    // but cannot allow trailing :
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "98e1bb0d-377d-48b4-b41d-d978cdd9ee22");
            // Validate the authority
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "49d6e0b2-4ec5-4efa-92e4-b3bdf64241fe");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "56a34640-74fe-410d-8ba0-b1812e177b8e");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "547377bd-3307-486f-8b26-cd3d1529ee46");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "8447000e-f69f-4852-ae5b-0fc6398a2ad3");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d1e0f16a-8402-4a28-b11f-852bf11ce6c0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "02023f0c-6564-4b52-9d6d-62699f278c53");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "2593bca1-d944-4d87-9cc8-7477a4ff07ab");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "74819e34-cbf4-4446-b373-1983097f57d5");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "ffb92ffd-675b-4f2e-9b66-4e159f52841f");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "b9a7ebc1-8c9d-4113-98f6-3b0814b9587e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "efe27fd7-d7c0-43f2-b88d-b9138b1849b4");
        // TODO could be removed if external schemes were checked in the ctor before being stored
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "33bd90db-b2e4-4998-ac4b-5666a47d5201");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "a1ee5d86-a909-4c11-8703-652e800370ab");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "b1302111-81b7-4739-8419-ebb2e4cddbdb");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "09e63d3c-1da7-4c35-bc5f-576ecf001792");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f8883285-3728-4438-850a-514df642fac2");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "0cf3d2c0-a71e-49b5-914f-502ce60c6f68");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "740e1773-807a-4127-b230-d58c2cc7ca80");
        // check manual authority validation if specified
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "100ceaf2-3410-4c5c-b185-1343d95058c7");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "65f4807a-207d-4fa6-94cb-b1948b93401b");
        // convert to ASCII if possible
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "2fa0faaf-5d35-4951-aef3-683335c75f2e");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "8be7781d-c4c1-4e0d-a605-47ddd5c153c0");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "74c3d2e5-7016-44ea-b57b-0703089ba88b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "a7be9c04-ac19-45f4-bebb-46ae561b2711");
        // We have to process IPV6 separately because that is parsed in a different group
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "56a17bf4-a375-4b7e-a901-adde3f537323");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c1e92493-b146-4014-ba76-02b9598122cc");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "09e1f5d8-c62a-475d-b4ee-f269b4ac18e0");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d73c9c43-9f91-49e2-b2ca-ab7a8959b357");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "40eb40fa-6a77-4b32-ac76-f75179562f0d");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "a41acb66-2f07-449e-ae6b-2e8475ff27bc");
            // check if authority is hostname or IP address:
            // try a hostname first since that's much more likely
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "78b5aaa1-36e4-4659-97b4-578b664f5c58");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "534137c2-23f4-4b62-a6be-1fb5b377d83b");
                // try an IPv4 address
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "1ea90cc2-0955-413e-9d33-ad24b4e08966");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "081e5c29-9624-4ec6-a52a-99d3e91bf5c4");
                    // isn't IPv4, so the URL is invalid
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "4cd02165-d80a-4717-8ae0-655fdc5d2836");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "15acbb63-51a1-44c4-9fbd-5564d30557b5");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "423845db-7d71-46b0-8ead-5dd681f1df66");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "be2d9a78-aa51-487e-9222-a9850e0fd01c");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "a9489262-b7b7-444c-b4f2-4aac1bb52216");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "709f0300-8b1b-4031-89f9-bd11a1003233");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "7d830dee-6b95-4037-beb6-f625d4a067a0");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d40ac067-bdd5-47f3-92b6-fb41d5e4a3bc");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "7506bd2d-444a-4dda-b83f-6985735b78e6");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d46a1a44-1c34-4108-8aaf-31df8ce27041");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "86f8c8d6-ae15-40d1-9a60-11e7a6f7e3cf");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "3e9379ab-b311-44a1-894a-fd49c45a8f24");
            if (// Trying to go via the parent dir
            norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d529e06c-b16f-4ac8-a722-eb35d60cd342");
                // Trying to go to the parent dir
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "10cac4d6-eaab-441c-954e-93efd62cb6f9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "6e00f311-dfab-45cd-bf7e-b983c94ab659");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "42d10dfb-7806-4992-a768-85390b569ceb");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c6707cc9-ec3a-4ead-bd97-cb4cc76eabef");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f7f258a6-1f97-4c41-b4d2-5ac45c7b0e52");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "9243ce4c-21df-4f4f-8aef-707933db40af");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "95ef25a8-56fa-4b9b-8177-079fdc487884");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "cbfb82cc-fad9-4f20-b68c-7f7308da2a43");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "09678718-3a1e-417e-b09c-f6240a78fdab");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f6bc79a1-52a6-45e1-bda1-dedf6d714770");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "a4abedb6-2287-4fa9-b2de-5c598e0e48ff");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "1019a521-cbbd-4c38-a435-6175b87b3d08");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "383bd536-eb5c-4b6d-9963-e17b12b11511");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c3041160-2240-4854-9019-4538bd8fe2ce");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c10c51a6-be44-4977-a36c-e4c523dd513d");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f46c4ba6-5dc0-4d52-9b7b-83ae287f76cb");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "3468a457-9fba-42c9-a91e-fe4348266796");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "2314ea8e-36e6-437e-8c89-41b7acc60a6c");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "24a9d3e7-473e-4bd5-bcd4-a10c3b5c42a1");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "da353d5c-304a-4ac3-8023-7ebba1566f95");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "65d7ac62-6522-45ad-a27e-6d5c8eda428c");
        return (options & flag) == 0;
    }

    // Unit test access to pattern matcher
    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "5f67e285-9601-429f-8957-3aa7e84e9161");
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
