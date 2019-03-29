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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "dc36b18e-5025-44db-bb44-d0177c73821c");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "ca985a7f-ea52-4d22-8783-b712b0f545aa");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "58ac720a-c3ec-4ba2-98fe-83b7e6c5ad36");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "fe05d316-ad08-4f76-8efb-2238df734599");
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "83126975-9fd6-4870-b869-db45097e665a");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "fafa6222-3126-43df-a03e-8a7f8c7646c3");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "ca659282-46f5-41ac-80f6-2365f7f7763e");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "e919db33-6729-4773-b1d9-a71c79dd7e9d");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "7525c88c-3f7a-40fd-af00-e3ee5ae8d7a5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "44365002-3428-4c5f-9316-e822d0cd62c0");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "77a35b0a-d0dd-446f-83ff-48dec7878825");
        if ("file".equals(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "be7466cc-7a14-4e60-a950-84931dd3a2b0");
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "e8bd5275-5b20-40ff-9e69-3bd4876b27c9");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "6a8b48bf-d01a-40fe-ac6c-2802fe6a6adf");
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "b4eb48a0-d67f-4558-8150-485aff84d8a8");
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "032b911e-5fc2-4307-b563-83e65aaccb85");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "da9afaa8-f165-4e0d-aeab-acca92b315cd");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "999d1fb8-5e92-49ad-97e9-1e4562ef45ce");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "5313650f-92f1-46a9-934b-a9f13c7bc7db");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "586ced3f-64c1-4a87-92cb-fd65d8090100");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "d227cb3d-ae2a-4339-bc8a-9a8e2824d349");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a3fb0d9c-4501-4618-87e4-63b92ee3e1d7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8a8605bd-21fa-4439-ac04-031d12d2192a");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "600b0dd4-27e8-4a8f-816f-10ef7913ffc1");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a2712e88-6abc-4b05-a1b0-bdea492951a9");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "bd8ee863-831c-4c0d-818a-a31721db4db0");
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "772252dc-9f02-4d0b-b0e8-8b7399485378");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "783d6133-2a3b-4c11-b2fb-61fc5a8495cb");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "24356d54-d5a1-468e-aa67-5917a1fd206b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "ee52a623-8a2f-404e-88fd-4495a4a57a90");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "dfe55cb7-018a-4def-832f-78e5185bfdda");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "1c7346c6-a72b-4645-8294-6134de6b7082");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "101693e1-b9ad-49f2-8f6c-3eb4543db4a3");
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "4ddfc98b-07c8-494c-909c-26fd07b2f383");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a8c9df78-1afa-4636-95fd-532971a2d0c6");
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3ffbe184-6cf2-4d6f-81f5-fe230a22143d");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "05202d63-f71f-4008-89ad-ed5bb8a1d93a");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "cefd2c71-9722-4b12-aaa0-2de52b29dc09");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "9ef27eb6-2754-4467-a840-ce84566cf9bc");
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "16a6903a-8651-4702-bd2f-4fe2cb56c430");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "baa0c39f-7cc3-423f-bd0b-36f7f61b97d5");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "81e132bb-042c-47fa-bb13-d04239500daa");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a9b0eb80-1773-4c5e-86bb-aed750ac4afe");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "14829fc1-49d9-4127-b012-2be98a501438");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "df874c46-ea83-4d3e-acf4-868b82347a70");
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "44e04a6d-1f2b-4088-b8e5-3bb19adcaea2");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "296db79d-63eb-4beb-9a5c-2162b0b14859");
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "aaf2a3fb-aff5-438a-a99f-bc71bee5d24f");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "17cd03a3-8f8d-4394-9443-ca65f37b87c5");
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "0923af3f-34db-4895-971f-12994ac29130");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8a471480-2ef3-45bc-ae0d-c771aac72781");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "789c9ae5-d2fe-41ac-b629-bc41b6b88ce5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "e208d359-9ddd-4968-912b-dd63c64cccfd");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "948dff5f-0f5f-472b-8d8e-a0293b2ddbd7");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "b8089bb3-f417-4309-b24f-f378cb846d33");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "8518dee0-48a3-48d9-8854-289bab5fe9e2");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "5f2eea26-3172-4bc5-a301-bc148be04880");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "81eaf808-95ea-4dd4-97a5-04f4a163f83f");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "087a479a-1f5f-40d7-a2ab-7da0a3811979");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "12bc683d-9fa7-430a-87f0-a2556f183610");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a253342a-c2fb-432d-acf1-f65f5fbc18dc");
            if (norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3e79338d-db8b-44bf-be4f-bfb6cd47d325");
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "91ac5ded-facf-4a62-9b5e-f7b0da240ef2");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "f979a277-9f9d-459b-a5f1-6de0dc2bf9d4");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "96ce15d6-97fb-44ee-80bc-8b04baeab1e6");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "0eaa0d46-30af-42fd-a6e8-d3db3f118e20");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "059c10d0-7666-4e3f-8f37-9f8007fd4724");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "460f5cdf-c53e-420a-abcd-63ef95f71ec8");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "fbc2270f-4091-4f18-8750-24db164e5b88");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "97d2ec8e-1756-4779-b8f3-1982b568e550");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "4fb0e437-74a0-46c9-8eb8-228daf1c60f1");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "cedf015d-ccea-4f75-8edf-27b2efeb5ca0");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "eb2704c8-e71e-47d2-b72b-e978caa9b807");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "27129166-9a4c-430f-b091-56e90559f3c0");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "603b4e46-5c82-436b-9746-63143fe0348d");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "6ac16077-e530-432e-8218-4a61434192d9");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "3c44673c-19d8-47d3-8c3f-fa1944a948cd");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "7583d4f8-f84e-4762-b59e-1427b1153f6f");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "2ccd7181-77f0-4b58-9d61-c33b2e692a59");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "74c3e7e8-6330-4ab9-91d5-1c0cd4aea9bb");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "a8745a0b-6d5e-4046-bb30-027bb045e6c7");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "05b74294-92ef-4167-9602-2a2bb2f3e20e");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "810f9f0b-10fb-4c7b-857d-93abf35a5a3f");
        return (options & flag) == 0;
    }

    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_5_10.coverage", "450ea58a-af02-49e0-95d5-9c86cffd9551");
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
