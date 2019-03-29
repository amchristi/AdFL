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
        writelineStatic("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "c65ddc68-2c21-4def-8b8b-4ba72527cd8d");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "259a9f6a-151f-4570-b18a-e4f836f3c682");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "11c04c88-298d-4b40-84bd-acf9818ac05f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "ed7a994d-b782-4616-9843-1626c7340f77");
        // Check the whole url address structure
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "ab966627-6260-4131-b83c-38bbeabebcea");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "bad478e6-404e-4f70-9358-3e8a5bb7fa65");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "df87814d-ce7f-48f4-b215-9de60ba192da");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "91ad863e-45e1-40bb-a39c-d5774473497b");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3522a9d1-ff5e-4069-9e83-65f4131cb567");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "b1530ebc-792a-4514-976e-563577d2c6dc");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "da252963-21ff-49a6-84c7-1d74e442ed70");
        if ("file".equals(scheme)) {
            // drop through to continue validation
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "1d3669de-ba00-4be9-83e4-868edc9e167e");
            // Special case - file: allows an empty authority
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8e107715-c372-4f68-aaf2-9404c157c6e7");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "59517f89-dd76-4211-aee4-5e632114aa63");
                    // but cannot allow trailing :
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "f266d12e-02aa-4670-ac66-1589db780f92");
            // Validate the authority
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "352b304b-bdb0-42c0-b6a1-c214604b86b9");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "5b1c632c-b343-47f5-a99f-fc85110d14eb");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "299206bd-484a-4b54-b290-f181fa0b1f19");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "6fdf8fb1-2fe2-4670-8498-bb1a53392fe3");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3113fece-6016-47d3-8b5a-6102225d0dfc");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "85c42103-ddc9-448b-aaa9-a001743b14f0");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8e4115fd-ae35-4042-a8e2-98f0e7fa2686");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "825d3b23-12a9-4c9b-99f9-a9030258fb4e");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "cecf0ea4-2e21-4534-ac78-cbbad6050adc");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "cc737c71-71d0-4565-8d3a-66279ec568fd");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "766a76e7-4d3d-4f7e-ab37-173a3619d3cf");
        // TODO could be removed if external schemes were checked in the ctor before being stored
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "5c3aa817-03c8-4142-8db3-f4ddf2d15632");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "59eda449-ee71-40f2-a034-a7c26c0bab4f");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "db1672af-a7a2-412d-b720-ddfbbbf691b2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "d30d784c-19b5-4ed6-9947-dd1e683b2103");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "23d6c1f3-f0f7-430b-9c6a-eddebb3385b7");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "d049392f-08c9-4265-8ef7-8c1aa472ca34");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "0426fc9a-4811-42fd-bd44-2d498a846b75");
        // check manual authority validation if specified
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "b6094986-5d06-49fd-9580-a3fbe729fa90");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "cd738a1e-65db-463e-8c7f-87589767ace0");
        // convert to ASCII if possible
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "5cb99861-44e8-4da4-ac23-055bbf14e503");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "89255090-aff7-43fe-8e22-81587e269e4c");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "69daf46c-c5a8-4cc5-8aa3-d70c10cb5d4f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "1761a8ba-de88-4084-b2d6-740006ac724d");
        // We have to process IPV6 separately because that is parsed in a different group
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3f487869-a2c5-4628-946d-3e68dd7d67e4");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "2c67000e-e77e-44df-b552-6a88efad19d9");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "85c312e8-97a5-4ed9-acd2-b4ecdb91e9b7");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "b07a506f-59e0-42bc-8acd-7b9f5a089621");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "ad72bd5e-4a2b-4c83-8423-4a3a67588ae1");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "2d8c2c27-7855-4aae-a9ca-c74d1d90fc40");
            // check if authority is hostname or IP address:
            // try a hostname first since that's much more likely
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "f421e204-9b0d-471e-b38b-5da8512ac466");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8def7249-db30-4307-aec8-94e256e90ba5");
                // try an IPv4 address
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "bc66031b-bcbb-4969-a104-e14087a179ce");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "915cd3f0-910e-4443-b8a9-a09db28ef96d");
                    // isn't IPv4, so the URL is invalid
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "aefc96cb-a23c-4a3e-8021-88499ee757b0");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "bb540d91-1c35-41e9-9009-e65947b6c145");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3507a8db-5b51-4d88-8d6c-7a4adc21592a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "cb4e6aa3-83a7-47c1-82fb-35288255baed");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8dbcf4b8-fd8e-43b6-9c32-1ebc70d255b7");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a7401489-1b8f-4c88-a28a-8163ac29863e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "64c49b48-ec2b-4f91-9adf-8fd299e6ea6c");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "9a8240af-97ad-4683-8846-2d2aa5979661");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "2cca2630-4883-4a6b-8a63-3b1a9cf89633");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "27e8eef9-39bb-4541-9e82-761c6daa3b76");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "c489ff52-a3fd-4d4c-9a40-a71f3a6d8428");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "0bf11623-699f-4127-acbc-9d394b575999");
            if (// Trying to go via the parent dir
            norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "032d662e-6dea-4a95-99cb-a934845563ec");
                // Trying to go to the parent dir
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "545c5243-8b48-4092-8cc2-77246a8567ff");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "b0975fef-1182-4360-af62-151071470f8c");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3bb5e5c5-6188-4b1a-9716-f4bf79716185");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "eb041d10-9817-4dc6-acd8-f4b5a2b16305");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a3b8020f-9441-4ee3-a0a2-7bfd505f0787");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "357154b4-7cd2-4d5c-92a6-a36a62581db6");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "23f80118-3321-448d-beee-1c36b18e8e81");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "22c5ec9c-f583-4754-84f3-6cd28c0fad5e");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "eb598311-4d01-4e02-9fb2-3a7d221e1aa1");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "2557ca7d-a447-4f68-93e0-90956c2b05a1");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "cd37a332-0e55-4ff4-8dd8-b2c8f5d53e14");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a89aaeea-8423-4062-922e-53a016737441");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "ff6884ed-b59f-4185-9023-94faf8897d36");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "378b64c0-7262-43b8-bbf0-12056a8265bf");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8aa506aa-7256-4cf9-9ef0-7a54bc97d001");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "9c2466fd-c860-4dda-a5e6-b058a50fbce3");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "778933f4-74a6-4835-b468-cd33f81ca6c3");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "274091f3-9f14-4b57-9df4-0430cd7e2028");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "d367547b-2f2b-4056-8cda-42da923f42bc");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "90456f83-ffbe-4330-a552-c3b52b7c37b5");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "726b1c58-20c4-4245-9120-d621d769c83d");
        return (options & flag) == 0;
    }

    // Unit test access to pattern matcher
    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "d9223b80-ece1-4c81-b80a-22ee78bbd69f");
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
