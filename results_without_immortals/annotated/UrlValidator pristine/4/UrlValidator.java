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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "cc386ac6-c136-40a6-93c4-06385c604d58");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "34aef78a-7db7-4849-96d6-f4f8e7f468dd");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "980f090c-b31c-4a8c-83ac-cf7efd8bc260");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "32623d4f-53d4-42ee-bfeb-079555a23156");
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e24ef89d-a73d-49ac-81b9-bba061a8b2de");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "5c218cc9-b08b-4667-acab-36176c060a88");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "3edc3448-d4b2-42a8-aecd-165d4c3824db");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e1267a6e-67c3-4dd4-896a-98f604f94d5d");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "d2078c24-3855-4270-98ee-02d258faed47");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "ad046c34-4403-4b65-83c0-3219cadda61f");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "34641997-a383-4ce5-a231-819d8bc5d167");
        if ("file".equals(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "6049c94f-af9d-4662-9d43-41dd69e26d68");
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "05f49a5b-d086-4068-a447-eec6c2045bef");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e73cd760-310b-48b3-9fd2-2da6d7009f1d");
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "b52a438e-3e09-498c-a083-66d8536789f4");
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "fdbe3a2f-0e25-4b5e-85cd-898f685ed675");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "1810c156-2a6b-46d5-958a-4ef531024a3b");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "10bc2125-e640-4452-bb8c-a45f7f29fd09");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "acfe20dc-9f84-4631-9fd7-0b02d622517e");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "b6c21d4e-79e6-4453-acbd-940bf28adb6a");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "f184765e-3ab1-4319-9a2e-ba8ecf03511e");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "19a74f7e-5a0e-4da3-a24b-cce1fe4ba192");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "d877b422-a18f-4eb1-b094-50ceaec05cbc");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "d94bd135-c9c9-4ebf-9c75-eff5e1bda796");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e899ac3c-66a2-48ef-b754-a6838b81c9c2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "6e9cac4a-86b9-4bfc-85c5-8a548d8bbb55");
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "56223e0f-c09c-4f30-9e3d-e6c23bef31e9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "b9dc6a0e-1507-4db8-987f-f9502c69aa80");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "cdd949d6-1a16-4e74-a7eb-b2856ac62250");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "dfd33807-93de-4c93-b62f-a1d2852596f4");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "f939912d-8c51-4e79-9cc1-bd4c6c80534d");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "0013ab0d-403a-4e14-a7fe-e61d220b24ce");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e3643ccf-7918-4dea-ae12-8c415f70fc66");
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "d1370e7c-3102-46d2-b06f-70da1be74e0c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "2a2bd60e-457e-4ecc-ae5e-9dd9ed40d2a9");
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "51a03bb9-591c-4738-8ab5-ec27f54ad4b2");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "cb8bc2bf-a3f3-4f09-9c3f-ef6b87ae46d8");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "37414f4d-d2e9-4fce-a2a7-0f268913e47c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "cce67308-45ed-4ddf-9276-95c42991a7c2");
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "d0a093f8-e81d-4bb9-be08-ddc2ff8e8c15");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "07269cdb-6156-4760-bd6b-8ecf15a9539f");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e438bf83-4d9d-48e5-ba7d-b4db65cfbe34");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "d36ee1ec-23f5-4a04-88d5-80ab53f49ff1");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "1c9cac3d-f112-458b-b728-d31f91d5bdc3");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "ce2923c9-71e6-4aaf-88f4-f033bb17d589");
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "f673f3fc-bc5c-4f3c-82f1-2544682bbe44");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "0e8d7cc3-b6d9-4f95-84d7-c991a38fdbbc");
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "945f6ee9-7487-4009-80b4-589944040ebe");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "695ce9b8-3129-4049-a0f8-7542084e7a08");
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "ea8dd586-8470-40b6-ae54-865599d9f614");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "7775199c-7123-417d-8e16-40ed92ea3dfe");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "83369fd4-53f6-46a7-bdb3-0cb3e1d1fa29");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "7bf2c68e-f5fb-4f7f-bed5-8834d69cea74");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e5b93ec8-7056-49d7-98dd-ad86bb29ba0e");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "5a91c6fe-70b9-4a28-a000-0300e7c874e0");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "45c363b3-4529-4b6d-8ef9-2f39c05248cb");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "ad31800a-c81e-4826-80b5-ff99cd55aca1");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "02d1d4b2-2514-490a-8c28-8e4a4b6479e9");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "61ada443-9610-4293-9b01-26ad04de5b4d");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "eacee0d1-edb1-471c-b8dd-fcc01d02289c");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "00da398c-2f50-44ff-bd7b-e13c9459add5");
            if (norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "3ad50d5b-cabc-4a7e-bd63-70fc75e0271b");
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "798973f5-f7f0-49d9-811b-187a3f444e73");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "582569ce-289e-471c-88e5-d6a9e6d74f95");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "b603cade-9419-424b-9322-bb77690e98e1");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "4a5e0e2d-d456-4afa-b8c2-2fe4055b6efa");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "9ffa6258-3394-4712-a764-d02b711fadb9");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "904abc47-a782-4ed3-b182-fae7d65d7ba7");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "5bd0155b-2931-4e5b-aa5c-d5042e02197c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "5a8e342a-01bd-4ba1-85c7-662181e26308");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "cb5286a1-c58a-48fb-aee8-50f3984c94b2");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "df19e2f8-e591-42e0-baad-208fd89b5da4");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "0aa625ca-03e1-4352-9856-ee552df4c19a");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "34058b3f-a46f-48eb-b2e3-867f6a9d787c");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "a5518298-27a4-4d3c-8638-94672accbe9b");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "b9b5a07a-a0ec-43a3-ace3-d116df7de31a");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "17643801-e975-4eb4-b1d8-681440ba7123");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "f409ac3e-c91b-4882-983f-364918928201");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "02c4c969-e007-43eb-b2d2-60efc76ba5ae");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "85b0d9c4-4c20-4086-8f89-92f4eb59bddc");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "e12ddc78-6f59-4a96-b18a-e54d1dfc0bba");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "20c7050e-c285-4367-8858-0e6fdd65e2da");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "d2bea50e-fa0d-41a3-b71f-22c184c6373b");
        return (options & flag) == 0;
    }

    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_4_10.coverage", "79230c00-c3c2-4d8a-bbae-82c0a03056d8");
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
