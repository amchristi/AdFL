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
 *   Example of usage:
 *   Construct a UrlValidator with valid schemes of "http", and "https".
 *
 *    String[] schemes = {"http","https"}.
 *    UrlValidator urlValidator = new UrlValidator(schemes);
 *    if (urlValidator.isValid("ftp://foo.bar.com/")) {
 *       System.out.println("url is valid");
 *    } else {
 *       System.out.println("url is invalid");
 *    }
 *
 *    prints "url is invalid"
 *   If instead the default constructor is used.
 *
 *    UrlValidator urlValidator = new UrlValidator();
 *    if (urlValidator.isValid("ftp://foo.bar.com/")) {
 *       System.out.println("url is valid");
 *    } else {
 *       System.out.println("url is invalid");
 *    }
 *
 *   prints out "url is valid"
 *  </pre>
 *
 * @see
 * <a href="http://www.ietf.org/rfc/rfc2396.txt">
 *  Uniform Resource Identifiers (URI): Generic Syntax
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
     *  validation requirements you should create your validator with
     *  a {@link RegexValidator} instead ({@link #UrlValidator(RegexValidator, long)})
     */
    public static final long ALLOW_LOCAL_URLS = 1 << 3;

    /**
     * This expression derived/taken from the BNF for URI (RFC2396).
     */
    private static final String URL_REGEX = "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?";

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

    private static final String AUTHORITY_CHARS_REGEX = "\\p{Alnum}\\-\\.";

    private static final String IPV6_REGEX = "[0-9a-fA-F:]+";

    private static final String USERINFO_CHARS_REGEX = "[a-zA-Z0-9%-._~!$&'()*+,;=]";

    private static final String USERINFO_FIELD_REGEX = USERINFO_CHARS_REGEX + "+:" + USERINFO_CHARS_REGEX + "*@";

    private static final String AUTHORITY_REGEX = "(?:\\[(" + IPV6_REGEX + ")\\]|(?:(?:" + USERINFO_FIELD_REGEX + ")?([" + AUTHORITY_CHARS_REGEX + "]*)))(:\\d*)?(.*)?";

    private static final Pattern AUTHORITY_PATTERN = Pattern.compile(AUTHORITY_REGEX);

    private static final int PARSE_AUTHORITY_IPV6 = 1;

    private static final int PARSE_AUTHORITY_HOST_IP = 2;

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
    private final Set<String> allowedSchemes;

    /**
     * Regular expressions used to manually validate authorities if IANA
     * domain name validation isn't desired.
     */
    private final RegexValidator authorityValidator;

    /**
     * If no schemes are provided, default to this set.
     */
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b17d4094-7db1-4674-9b9a-a5223eae209e");
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
     *        a null will default to "http,https,ftp" being valid.
     *        If a non-null schemes is specified then all valid schemes must
     *        be specified. Setting the ALLOW_ALL_SCHEMES option will
     *        ignore the contents of schemes.
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b1f6f64d-0fe5-4155-ad97-ba6cc70f58c2");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "ad6e358d-3e7b-405d-8065-da6c9d1dd883");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "06d2af2a-aa1a-441e-98bd-092efa4adabe");
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "34995060-a447-4b32-a786-05e58e475062");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "559197b1-a6f2-483d-847d-0955e6427d09");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "f44093f3-8c9d-4565-8d8a-ee18d5a4a9c2");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "4fdd4b54-7f7c-44d8-a93d-5eeee957a93f");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "f8fd4708-e880-4f58-bf4c-b19eda75e1ec");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "3c11c07d-e27d-420e-8910-2699a8006af9");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "3a337791-df6f-4194-8e8b-24b300c39329");
        if ("file".equals(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "08732ae0-8072-4073-a1ab-8f4723eac3c2");
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b75b77c8-9054-4fbc-963a-46dc8ceef323");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "cf84fb54-897f-4445-b606-5e7a9300a4ad");
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "7b9c726f-eeef-41d6-b0c0-9d5742bb91bd");
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "758a7197-aea7-43fc-a23e-13dd79327b6d");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "461a0cb1-701a-4d3d-bf0d-c7966f4c19c6");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "97758b29-28da-4b5d-b83c-7b108a6280bc");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "2b7081fd-2c86-4f0e-bdd4-ee658696292e");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "95c3933b-0701-442e-9425-495152453b1e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "41b8a95c-e617-4b38-93b7-21be7f9d99a5");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "01c2e1d3-ea94-4faa-9e86-915894e1e6da");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b3302592-ba39-41de-9a74-4a12d57a4855");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "a3b20848-6982-4fe3-99fe-5e23b3e27270");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "28ffe082-d6fb-404b-86c9-6a336a0d95e7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "9ad40d4e-81b7-4737-bd38-b5a925597af1");
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "405c0dc2-4737-449d-9766-ba3e92a686a2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "41f3553d-eabf-4aae-aefb-bde8d7d07f7a");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "219260d2-b609-45a0-bcfe-c03ccc9a85d7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "0ea8c614-23f1-43fb-8b71-92eacb609bba");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "50fc844d-e478-4733-9c94-70c107490a8e");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "e8e84d84-3bf5-4536-ba70-516b0d5602bd");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "784c728a-206c-4aa3-832b-3c878758ff30");
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "e36326b3-a0fa-485e-9d20-a7e04117c43d");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "d761200b-8f8a-4aef-aa6f-ac68edf22baa");
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "09d9cfc2-5861-4b69-9811-2f38b1bc647d");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "fc9fcb41-15e1-4b03-a319-71900be6e659");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "f8ef2461-d7a6-4728-811e-705f014dd2f2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "1326f242-257d-4c36-baa5-5ee5f4d1fdb0");
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "3db199bc-5510-4c36-9abc-ec151f0c5994");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "4940cf17-4a55-46ee-864c-7668257980e1");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "27f2d3ba-2fb3-406e-97ba-bb773dbb3274");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "07cb2b10-f880-48ed-9469-2e495cdcba82");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "ed2458e8-83a2-456a-8993-77cd01e1638e");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "bc24a10e-f55d-4a6b-adf9-c6b0972a43e5");
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "47e8b615-7e6b-46f8-848d-01d633310654");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "9ffff478-9ec3-4ced-a10d-c27a7bdb0fc9");
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b803572b-576e-4b3b-a382-264380ebbfe6");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "94ab6b64-4253-4a7b-978c-20b28752a9a4");
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "c11d4d14-d83e-41ea-a678-d6c0cd168d80");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "ef8bead9-4164-499b-9df1-0dcc8f26c1c1");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "c3f20e69-b5e8-40b7-9c01-82abbc3b5c6d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "666103d5-9d58-425f-9984-4bf30dae0344");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "dc541840-27c7-4163-b925-69055698777c");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "73136094-f72c-403b-b2dd-14e161fe672a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "6945fca0-4d83-4c5e-b20e-7ca76dcef840");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "ba7e36dc-2922-4a5b-94ec-323e2aad7f55");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "980f3d5f-242b-48b4-8d99-5913ef39eec3");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "f2278d6d-0e03-4101-83dc-6e4e2b23594e");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "d438d119-dc2f-468b-8ec8-ae019481d1bf");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "0a5fb497-dfc4-4a3a-9db8-5e538b789d18");
            if (norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "40ea7662-a295-4846-9d0c-2af1db2a00ed");
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "59ee47c9-cf67-4d93-bdb8-65375222b461");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "124e11f4-e459-4141-9cf8-0204cdce9509");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b36e80b6-e6e6-4f3f-afba-3c3ae82acc32");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "efb2f210-3d02-4d15-82cc-32206fb0e52b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "f2d0e5f8-6563-47d7-83a5-5eaba890aebe");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "dcb987c2-f7ac-4656-8d00-dd37c7a6e6b2");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "5b475f38-8190-4c8e-8450-013a9d5bd70c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "414ebb8a-d19c-487f-b643-a3e8fa5a0164");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "294fdfbf-c4e3-4930-892a-e36a84a0450a");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "e02109c7-0948-4978-9c17-31a69cfbb738");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "726e4707-4e9b-40a9-8fe2-0148b9813bd1");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b9952fc7-f036-4aa4-b5f4-cd608ef474a2");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "1bfab10a-1a7c-4158-8212-8c7620460e85");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "d0f128fc-ca4e-48e0-9bbd-0d800fd61a1e");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "b402edfc-ff8d-4ddd-90c0-e4cc495132d1");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "ea0bb63f-abed-4ff1-a158-af3dbb6abae2");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "ccb262e8-fe62-4517-8ba3-aacf4110e426");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "dbe94137-4ae8-438c-9a36-2cc176fc864c");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "2e1e1f22-7a13-45a7-837a-6bdfae9e5958");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "6ca036d3-eef5-4996-8300-f28742965bf7");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "fceee0c4-cdff-4012-b5b4-749ee1deba68");
        return (options & flag) == 0;
    }

    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_2_10.coverage", "2d3a7e2c-15e1-4ca7-b6cf-19559965f302");
        return URL_PATTERN.matcher(value);
    }

    public void writeline(String fullFilePath, String text) {
        try {
            java.io.File file = new File(fullFilePath);
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
