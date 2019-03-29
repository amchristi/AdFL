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
        writelineStatic("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "ce3cd1f6-1f3d-4e60-9492-6396be5f79c5");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "b494627d-e31a-40cf-bafd-36b49b720d42");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "130ca21a-a91d-4f79-9425-b39487a2bda9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "b459714e-4baa-4a47-ae5b-adb4cf855527");
        // Check the whole url address structure
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "db306d41-b16f-4777-bee8-49aa3e16f559");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "01214f7f-6123-4abf-8100-6f5b61b72e76");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "bdbe4664-cd77-4874-83bb-e87ed7539ece");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "daa2f15d-86ee-4cd8-a2ec-400488666fed");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "45bb00d5-43ec-4d5b-b133-2feb31254e31");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "f6469872-141d-4730-9c1d-de39366e82de");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "635ccee1-0883-48e9-b942-82866c4431ca");
        if ("file".equals(scheme)) {
            // drop through to continue validation
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "cafef3e1-90a2-4724-9d2c-90a4b4674c61");
            // Special case - file: allows an empty authority
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "685ebded-6de3-4825-98fd-040c3718e89d");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "7be9db5b-cb5a-4d90-b485-e0e173847083");
                    // but cannot allow trailing :
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "8cf726d7-193a-427f-b4d8-05a4bddcf8be");
            // Validate the authority
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "c15d52d2-64a8-4a23-937f-1c1e3e9e12c6");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "f616a474-0a6a-4c91-9935-ea8c109e3038");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "6fd9840c-2656-43ca-ac22-d0e8dd9324b8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "2e04edf3-a795-4f64-85ab-1ecea4901a60");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "af8e36b2-b78b-4d15-8dce-63a8c2c5f13f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "8c213f0c-ae6d-4b8b-9904-6e2bf5fd2a11");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "1af1befb-6bc0-49e1-8fcf-e6818abbb4fa");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "a1bbd252-2de4-4687-b112-38ef41195996");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "8697610c-6291-499b-98e0-8777962f5c77");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0d0c1b26-be65-413f-bf83-2ed64fb4e23d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "1a7eff03-d7b0-4bf5-a3e3-cb70c78cb717");
        // TODO could be removed if external schemes were checked in the ctor before being stored
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e334d15a-d65a-46da-83b9-983d07a4d201");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "66cee6f8-d78e-4af3-9f17-cace6017a89f");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "fce8e3c4-10bf-47ad-bbab-f52fbc772248");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "7155a68d-dfbd-42a4-bb8a-be4e8136ef8f");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "f5c709f7-bf07-4093-a062-afa421b4030f");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "bb16c161-8a07-41ff-8f9b-3a1ffc19418a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0c068057-465f-4632-a8aa-362b034c0a0f");
        // check manual authority validation if specified
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "32ded3c6-3940-4e54-94b6-c04cd19c3451");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "79dcdacf-f63c-4e0b-83c6-a953ab36e702");
        // convert to ASCII if possible
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "082ad920-8d8e-44c9-a018-95b1a7802047");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "7bca6943-e8bf-4a78-9095-1e085aa555bc");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "f1f5849d-87ce-4797-9435-816b8b5aadfe");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "93acb4d8-b67b-4292-8737-67366e306b47");
        // We have to process IPV6 separately because that is parsed in a different group
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e2166fbd-d416-4699-bb1f-fa90cc7dc1ac");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "42ce777d-5720-4f7a-bb94-c986931d12df");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "f6c52cfe-8a6c-464d-9134-4b86a0b0fbde");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e24b1b2d-dc2a-4a9a-b551-b915758c1819");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "684bee37-c306-45e0-97ed-c14b6220a28d");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "13f6a9fa-f192-46fe-8772-253a884bde66");
            // check if authority is hostname or IP address:
            // try a hostname first since that's much more likely
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "c9787e66-e985-4741-aa4c-4d346337f4fd");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "fc5231bd-55e3-4dce-9b4d-8063dc34af75");
                // try an IPv4 address
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "8dbc6cbb-dbdf-4c2c-b5c2-2555fe5bb09e");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "9fe6adcf-29a0-424c-8721-65ec00140adf");
                    // isn't IPv4, so the URL is invalid
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "56ea3669-105e-4792-a6f3-e31a25fccd01");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "4215f1b0-e5b7-494f-867f-44e57789ec34");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "70d66cda-d0db-4a5f-b3f5-8c6219487fff");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "369d0ff8-cef5-4424-939a-3b8e6242eb6c");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0feddbf2-2ff8-4d62-999d-6ee36bba6811");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "6bbe5492-08ca-4b6e-947c-00850b4c74be");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "1ff74983-1ea9-4804-912e-7a62b39fc42e");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "b7851e38-6d96-447f-a74f-965f0653447c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "ddef7a6d-55d1-422b-8f9e-e8fb7b6916ce");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "8cd9e96a-4e5f-4fc6-a176-ddb22a0fa97d");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0b4a6384-50d4-4b4d-a73b-8a4e28972f38");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "9a23e400-2aad-40a2-89a5-a0538f98bb4f");
            if (// Trying to go via the parent dir
            norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "aa1096f0-b3c2-4f33-b0f9-05a1091a876d");
                // Trying to go to the parent dir
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "2fedc538-1432-40ef-9905-b948991c16bb");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "bd6ffa1f-e809-4a4a-bded-d09cbf7a0994");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0814d4fc-1057-4d34-a6c1-efd1ec849b4a");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "78ad5af1-d9ee-4c32-ba63-17c31c88add8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "94de8d3c-8244-4280-b126-f78e3f9af06d");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "dbeae7a0-14fa-4fdb-90fd-62b2f2dadcb6");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "baf4390a-b239-41a9-af59-b6058ede7b0c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "07298447-8b32-4fa9-a3ba-173020622e7d");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "eeaae4ea-0c56-420d-8022-66e63becc735");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "d1c90154-1a52-4afb-a5ba-8c006416583e");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "08f40cbb-2bf6-4fdf-9182-90f088198c5d");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "c10fefc8-fa1b-40e9-91c5-dfcacf782df2");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e5b2386f-4293-4420-88dd-fd2944f564a3");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "53a2c535-5494-431e-919b-7a4eda1746d9");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "9b5c0e03-8c7b-4938-91c0-6965e873dba5");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e8f30e79-441e-4a2c-b6e7-5a81369cbc03");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "8fd3f470-9e1b-4385-aad8-56bc533f2a65");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "d4fd45ab-5645-453a-8f1c-51851352c70c");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "f368ff34-bfcd-416f-95c4-3e4fecb53cda");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e9eb856d-8e66-46eb-9b63-d9d0d8ce2247");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0c0e321a-1019-48b8-a23d-667fa8b55b30");
        return (options & flag) == 0;
    }

    // Unit test access to pattern matcher
    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "1fceb3c8-5a9b-4e5b-bec5-8cacbeb18a22");
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
