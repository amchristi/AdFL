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
        writelineStatic("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "16808931-b995-4747-a9d1-db2197402d9d");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "4d6bf203-0123-465e-a248-a193ae2bcde1");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "fcd14c75-03af-4601-b3c3-98d8ad3b6d48");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "8fc029cd-582a-4830-a7a2-381ca1a18bd9");
        // Check the whole url address structure
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "3af8491e-a39b-42e1-98a6-c45abcb58f7b");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "e7fc2e4a-51f3-4c53-a671-470e680af392");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "ab1f96c1-d28c-4d05-b1f0-ce80f1b6ae39");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "6c473385-e0cf-4f2c-8a0d-9e386f3fb29f");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "2e90365d-5527-49d8-8137-42f5e0e2bfa3");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "a2673512-90af-459a-8448-234f938011ce");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "4e7d0f62-6116-4138-ac46-20f9d04535d5");
        if ("file".equals(scheme)) {
            // drop through to continue validation
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "ce3014d8-c940-4b82-be25-9483742a9053");
            // Special case - file: allows an empty authority
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "f1869233-17ce-4394-b9a4-d1e30c9daa5a");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "01f8e59c-6e33-4df9-b838-8bba450754ba");
                    // but cannot allow trailing :
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "3d53b268-1cd3-4dd1-ba6b-cb2b24ea6982");
            // Validate the authority
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "88d97801-de77-486c-b2f4-9b7ae6739d94");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "3d141e9c-4559-4a32-b229-9558a0a5e4b9");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "de061a90-cb8f-4acd-a700-735faaafa1b0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "dbdc1186-4c3b-4b1e-bdcb-329e13348c77");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "e886491e-864d-42c3-801e-4eb16def7bd8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "449f820d-5343-4123-b5d1-ad8456509c0b");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "199c062e-6485-4b7a-a501-9c2fd62c267b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "8175a372-ab15-4496-b533-4f4ef4b3fa95");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "4bbd5e76-08da-478c-95f6-00ea06236c88");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "e6f1c530-17d5-4db1-94c2-3648c1ec3585");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "a910083a-0738-4298-858c-49081d49d170");
        // TODO could be removed if external schemes were checked in the ctor before being stored
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "7407e86e-0587-4854-8d57-851407188491");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "21ef0649-6ba0-4548-8d5a-5ba11b1786b8");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "74dd8930-dc30-478d-b239-1304dacebf9f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "cb875e9b-8276-46e9-ab54-7b64a530c122");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "2f5b6221-2c72-409e-b285-86247b77a90a");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "bbdf9378-a6c0-44dd-b04d-3fce6d8f810c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "df4f6076-53d6-499c-b1f4-f262f10d0c44");
        // check manual authority validation if specified
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "8ae85fc7-2d11-41a7-848c-493e0caffcb1");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "b48f3be0-7f0d-4b4f-a91d-d433722f6442");
        // convert to ASCII if possible
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "7aae6d28-a5b8-4b90-b7b5-88fef959f55b");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "9b63fc4b-bbdb-413b-9bac-7523d6984023");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "7c83e58c-695f-43d0-b15c-4fd8ac436df4");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "f177b734-71ad-407a-a92e-cf99e20d782c");
        // We have to process IPV6 separately because that is parsed in a different group
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "8a68dcc7-d0e5-43d2-b25e-dbc0f5de7b87");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "c24dd65d-0ab7-4ace-8d88-af328c8559bc");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "2922fbbf-7aa0-4a68-a403-5fc9c665df26");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "72937529-9ddb-48b7-99c1-f3bb5ba0e6fc");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "14b2ece8-3791-449c-8d57-1858e86efbe3");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "7aa0de3a-b7e2-4070-a417-5fc20dd23aba");
            // check if authority is hostname or IP address:
            // try a hostname first since that's much more likely
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "550d81a7-ce38-4d64-9245-8547833e57ac");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "4387365d-9503-43cd-8d2d-bca659eddbbb");
                // try an IPv4 address
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "15fc72e3-6143-4ffa-acd2-ad3d0edf97fb");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "9981ea7b-5100-471b-ad6f-c8c81c1bf7a7");
                    // isn't IPv4, so the URL is invalid
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "4d7bfa6f-5a12-4902-8f02-302465e0d855");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "3b0dcba3-6ba9-478d-8d88-62cd09485213");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "2b150908-0b43-448e-80d5-c7e6a03f9781");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "12931b61-16cf-4bad-b98e-d4a35f6ed6de");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "8abc4e85-9a4f-45af-9e1d-4e0465c65e8a");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "1302748a-27d1-4983-a24b-a6fb03f54ee9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "12978f7f-a2d1-4e3a-9f32-9f58a2031160");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "39ec5238-c0cd-4efc-bf6f-459d22203c7f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "ac188d22-3617-4f15-8242-9c0f475a5383");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "8d27201b-943c-468f-83a5-22a5915c8f5c");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "4a1f9073-a223-4dd8-9d50-b533604c0af3");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "afdd71c9-d15b-42d0-97f5-3bb51c75d347");
            if (// Trying to go via the parent dir
            norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "757520c0-2751-4056-a41d-0757661ec108");
                // Trying to go to the parent dir
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "683183c1-aa49-445c-8830-acf9d0c95b23");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "ed99d985-4efc-4f6b-bb71-aa345417a012");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "a37d3df9-7c88-4802-9752-f64b99645411");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "00a4350f-cde6-4278-89f7-eaa8549c75c4");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "7a2ce205-86f5-4bec-8153-14f74b9af217");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "084779b1-30e8-48a2-8f22-2d84511e564f");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "004909c4-66c9-4e96-84ec-341117ab772d");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "c95432bc-4b13-45a9-8071-698b39f14dea");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "d943bd7c-f03e-4e28-95b3-2f65a959b0f7");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "da2aa98f-15d2-4077-8130-50bfd0a0cd4f");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "e0ab6ff7-1b49-4147-95f2-5cf3343e43f0");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "ab180730-6308-4d63-a3b1-4e108b19044c");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "c844c2d4-d3b6-4ae7-8c91-2683413ebede");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "5ab505e7-1eef-463a-bd63-2035d29bd844");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "6ef4f387-0432-4364-9076-2038f662a561");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "7211fcf1-1839-4cda-ad93-c3b7b89cdfe9");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "1e20cbc2-464a-4a0a-961d-bd89baa43b41");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "a5bc145a-92d2-4e91-8930-02fe9fb660ca");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "9824a474-a6e2-433f-b103-8b2f2f447513");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "9794839a-9e70-43b0-bddf-76a292d73798");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "b0f1f366-157a-4054-8b88-d36bd50317c9");
        return (options & flag) == 0;
    }

    // Unit test access to pattern matcher
    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_6_10.coverage", "733b6956-90de-4ab1-834c-8cf44c73f63b");
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
