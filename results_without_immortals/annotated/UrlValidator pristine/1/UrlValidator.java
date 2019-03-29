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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "910f7da8-011a-4fc2-a66e-401084ab0fae");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f9bfa444-1f5e-422f-bd85-030b7effc556");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "bfd5fe0a-774a-43b2-bc3e-c4c8f36eaf30");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "6b7c9b90-c81e-49eb-858a-5239b1f67833");
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c4a1b7bd-7c66-46f7-ba87-b58fa1e18a09");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "1ae418ba-992f-4429-8aec-14c70ce964ee");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f4c3c4ae-891b-4b3f-a60c-3cef853f057b");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c38fe2e2-a811-4e91-ad2c-393e7baf9573");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "6e2ae1c3-a1fc-4143-b187-e1607aa2a502");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "7fb68b3c-fccb-403d-88e1-a0a26cec2e94");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "70bc9696-06b7-4e5f-ac8c-9643fb27adb5");
        if ("file".equals(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "bc508c18-fbe4-41be-baa4-0ad7365b8b82");
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "daa9d3f4-fc7d-4f25-b1f1-f5a6dd3632c9");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "209474ab-c9be-4da4-bb0b-c25f03b2587e");
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "228da065-1c93-476a-863c-2a63fb8ac9b4");
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "52c7b876-a897-4324-87f3-f4e96c7abe1b");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "9e80fc3f-e877-4520-a066-07c7beb466e4");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "bc77b30d-aac6-4ae3-bf27-f66a6b3c4ca8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "0b4dce16-e99f-495b-8e61-ae3b085e4adf");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "ac896e71-2594-4dea-920c-2fd5e7600ba9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "4d660166-1415-4fe2-aaea-bea9fd540bdf");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "71d64cb1-b42e-4d95-90d1-d8994abc27dc");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "dacec347-819e-4917-bcd3-e6b3e912e85c");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "9c412062-3962-4ec7-9458-4067fedae4a4");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "3641f0dc-1745-4699-8be4-73c464ef7271");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c661188c-fb65-4c81-9801-eac3c507c7d6");
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "28888922-a559-4d5d-aaf0-553d3e874523");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "523753cb-a03f-4942-a689-3e34ada95926");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "02216f22-acfa-4bc7-bb5e-9c946de9905c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "b5c45d30-4df5-482f-a79f-5ea203a37bf8");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "3f54d26b-4b25-459e-82bc-61c14ab1df5a");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "03e2b954-ae98-458f-aae4-f3dff2708f96");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "dcf8ca7d-79fa-4463-9519-1a978317e64a");
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "68a80f07-556c-488b-8463-7ec961a71a8c");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "6d6287d3-1ff8-42be-86d5-08f18996e577");
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "2504f72f-f743-4a66-be61-9d70d6a011b2");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "82cabb4f-b245-478e-92e1-37052d3dd0c5");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d6f4052a-093d-4de2-b471-186e6380c1a1");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "72374b1b-e31a-4f2f-92ad-b84991a9edfd");
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "9127f7b2-4fae-4b81-b7a4-190956ebe92e");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "4d0b8729-630f-4fa5-872d-5121d31bb5cd");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "57b69717-e251-4db3-a7c8-c895d8eb7b86");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f73739bd-318c-4202-9aef-606f92cc22eb");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "dfd3968b-6dd2-4b81-ba3a-94a6486d84f1");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "df121705-abfc-4981-ba52-bda03867ccf0");
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "b5087f36-96f1-4ae9-881e-839143a46dd1");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "b7510894-32c5-40cb-a429-921ebbd73115");
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d523a076-1c15-47e2-97bf-a6626ea26d42");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "9d55da04-c95a-4ff1-9897-5f7bb555fad2");
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "1348ab70-67a4-458f-98a0-e333ac70bea0");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c3f8af54-2d59-41ff-8bae-5eb251261b8e");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "2a63e20c-04b0-4a50-92e9-9ad9bb8fa054");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "13420827-b429-49b1-a7c6-f24108b615d7");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "5ab54f8f-afcd-4946-8c7a-a72d3b4de8d9");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "8c491d7a-3f3f-4c20-a0fa-3ab36ec75de1");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "ec4eb7fe-bdf0-4ddb-9ff8-6e6973c38d42");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "5f1699bc-72a5-4198-bf78-663a0031e019");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "fadf734d-d365-4ce1-89c3-858de23d70ec");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "474c7987-22eb-4637-8feb-a093333cebfe");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "9a00f9a3-5f2b-445f-9da6-97a3cb06b457");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c9e80c7b-10e3-4f74-ac25-0e9371c427be");
            if (norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "34ad2fda-882f-465f-a7a3-0bd4b72061ca");
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "4ca9514a-9411-43e8-98ec-db3c44cf356b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "910900ac-3267-4fbc-800d-bcadfb87b4b1");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "ebe91bd0-0b98-4749-876e-333cbf423c6c");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "d7b88120-bed0-4755-bdcb-33fe6e9e56eb");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "9e29aa89-a00c-4316-9ac5-e2f77a7ee9b0");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "a4c4e9d2-c443-45eb-af07-59b4d71f3d31");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "c722a48a-c52d-47a4-805d-bc431276f859");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "fc5fff97-9500-41c1-bb41-b8c35b21b2e2");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "fc09e300-931b-4164-974f-b9327793cc32");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "f56f550f-2a96-4684-9245-61fc1b6d32af");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "ceef90ab-05d7-4131-a7e0-281c00b55486");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "62bea254-3036-4e9c-b5b4-364779387ee9");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "46f3591c-9dec-420d-9cf1-4b22c0fbb784");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "145c850d-ac81-495a-8150-31d9fe11d57c");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "98d3e7a9-9732-47d9-b561-7bd0a769cd79");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "36c20b2a-92e6-4c60-8703-c304aa1ce50d");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "aa583bea-51a4-49bf-8140-e0cb08d403b1");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "2ca9a99d-289d-447e-b9ce-76854b70ee75");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "333c2a8c-4cbd-4d67-ae7d-0de595e04fca");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "99bce948-0398-4a49-86ab-6b13e22548a6");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "8e3b21bd-f1ad-4bf9-940f-b86038b29dce");
        return (options & flag) == 0;
    }

    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_1_10.coverage", "50ac28a5-164c-489d-a224-453fa7c2b5dc");
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
