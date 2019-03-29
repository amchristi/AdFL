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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "a670203d-8225-4e11-9382-037f806258ba");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "975e70e1-ea93-4bb8-a02f-707de8821b3e");
        if (value == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "dfb1ec21-c720-4ae3-b1cd-0e0c381aa138");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "52e7d21e-a443-41ea-8845-567249267180");
        Matcher urlMatcher = URL_PATTERN.matcher(value);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "4d0c3660-52e5-4cdc-861b-53c66ecb612f");
        if (!urlMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "b2f0f49b-c0dc-4b21-aea7-d7faa53cb74b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "25c30451-d4fb-4b04-87e5-9c00bb148573");
        String scheme = urlMatcher.group(PARSE_URL_SCHEME);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e123111d-fd6b-4aed-91b1-e6b46b5fd6c7");
        if (!isValidScheme(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "c3119d3c-a5d5-4715-ae36-dab0aac2bd28");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0d6bc99f-2f6c-450d-a1fb-2a00991a6631");
        String authority = urlMatcher.group(PARSE_URL_AUTHORITY);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "ce00e573-1c7b-4f60-841f-19304fef1bf6");
        if ("file".equals(scheme)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "012fca36-6dea-4681-82e0-d54013bd69ac");
            if (!"".equals(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "e11a93e9-7f3f-4d08-b990-1610e6005b93");
                if (authority.contains(":")) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "bdccbdba-05a1-4cc6-8603-fbb056113293");
                    return false;
                }
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "74ccd9eb-65d2-4fc3-866c-aad864db37e7");
            if (!isValidAuthority(authority)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "d5a27dad-fb18-4431-911b-2b1b6ce739f5");
                return false;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "10509407-6df2-49b5-be67-2a255bd09ae1");
        if (!isValidPath(urlMatcher.group(PARSE_URL_PATH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "ec218de6-2968-4332-b2a2-a992dcc43bc7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "6944ae01-276a-484e-9d36-9921cd7c9b4d");
        if (!isValidQuery(urlMatcher.group(PARSE_URL_QUERY))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "44a9a555-c671-4e6b-80fc-f9c64454cc56");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "323b903d-d412-4982-9d37-528017f891a0");
        if (!isValidFragment(urlMatcher.group(PARSE_URL_FRAGMENT))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "a0cafca5-7e9b-4fd7-993b-4eda5e8b5afb");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "44d76a37-001c-4fc6-9419-148ff2d9ef30");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "717aa779-6cff-4de7-9230-780e17d4159c");
        if (scheme == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "59fb89c7-324a-4b7c-984a-02d5e2439c49");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "d23f13aa-cdca-41a5-b175-254e02d248f5");
        if (!SCHEME_PATTERN.matcher(scheme).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "ef209a0c-e80e-4856-a2a5-0cd997c365f5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "034e65ce-fb1c-4bed-a233-e0479deb9992");
        if (isOff(ALLOW_ALL_SCHEMES) && !allowedSchemes.contains(scheme.toLowerCase(Locale.ENGLISH))) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "9f5888bf-7227-4310-b2f3-5adad5639c96");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "227f3880-7320-4b46-a7be-affc8c4543d7");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "71bd9f93-f2fb-41f8-8167-b5402be600fb");
        if (authority == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "5249b14d-5ee1-4fe2-b422-7982740f85e8");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "5156d2af-7cfb-42b3-84d3-c05a6b21f370");
        if (authorityValidator != null && authorityValidator.isValid(authority)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "5c06b803-eca3-4330-930c-55d8cf962081");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "b5680f2b-a238-41e4-9c68-e81dc85a41ad");
        final String authorityASCII = DomainValidator.unicodeToASCII(authority);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "6007b0d3-755e-4c6d-9eb1-2172034a95d8");
        Matcher authorityMatcher = AUTHORITY_PATTERN.matcher(authorityASCII);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "9ce79992-8f56-498b-adc2-a32afeca7468");
        if (!authorityMatcher.matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "34f0dc04-ad9a-40a7-b7b4-af2aedfd7b4d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "57b36d46-5a47-4e07-90a8-e705ccf3f229");
        String ipv6 = authorityMatcher.group(PARSE_AUTHORITY_IPV6);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "401c9dbd-fcbc-48ca-b122-67b2b8684c9a");
        if (ipv6 != null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "88c3f754-7de0-4501-9191-f67048dce865");
            InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "1e08beaa-9a63-46db-b785-73be516ad20f");
            if (!inetAddressValidator.isValidInet6Address(ipv6)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "a31246fe-2098-444c-a24c-62cb97f3a6a6");
                return false;
            }
        } else {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0e21681b-f40d-49ab-82cf-75d556c4eef9");
            String hostLocation = authorityMatcher.group(PARSE_AUTHORITY_HOST_IP);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "f2fc506f-e8c3-40c9-9e76-cb057956cb96");
            DomainValidator domainValidator = DomainValidator.getInstance(isOn(ALLOW_LOCAL_URLS));
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "a6650483-9da6-46e3-a913-7bf867e85417");
            if (!domainValidator.isValid(hostLocation)) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "06abc20e-6e78-428b-bdd3-85b284379f8d");
                InetAddressValidator inetAddressValidator = InetAddressValidator.getInstance();
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "af34c6ab-c418-41ab-b963-50efc68c7be8");
                if (!inetAddressValidator.isValidInet4Address(hostLocation)) {
                    writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "7083cd9c-6484-48ed-844e-b6a19c82d632");
                    return false;
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "45b087c7-b2f0-4ee8-ae41-fd67e53e2a84");
        String extra = authorityMatcher.group(PARSE_AUTHORITY_EXTRA);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "0f7232f1-4b8d-47af-84f9-792da84cec36");
        if (extra != null && extra.trim().length() > 0) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "4ca4a22b-3af7-4fe6-846d-3029ccb6527e");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "da53f3dc-482c-4d37-8839-ce928db48698");
        return true;
    }

    /**
     * Returns true if the path is valid.  A <code>null</code> value is considered invalid.
     * @param path Path value to validate.
     * @return true if path is valid.
     */
    protected boolean isValidPath(String path) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "957d411a-f722-4da8-9c48-94a4003005a7");
        if (path == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "c70672ff-11af-4121-930b-5faabfdc62e7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "079e4d01-d035-4468-9490-9e1503de7f7f");
        if (!PATH_PATTERN.matcher(path).matches()) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "596086c8-a541-4751-8879-0b6532941ffe");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "53fd2b03-de2a-4fff-8217-d59f69ae6486");
        try {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "c6682260-9781-4bbe-880b-571dc89fe2a3");
            URI uri = new URI(null, null, path, null);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "efec8182-29ce-4134-af87-3bd21ece0f1c");
            String norm = uri.normalize().getPath();
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "8e263b94-4df8-4a81-a14d-26aac066f129");
            if (norm.startsWith("/../") || norm.equals("/..")) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "86500531-0dbf-473f-b4ce-5166cc525392");
                return false;
            }
        } catch (URISyntaxException e) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "cdd96a87-837b-456a-96d7-f7415374e277");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "349d8e85-0f5f-4975-8f7b-ee0e1980e17f");
        int slash2Count = countToken("//", path);
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "72faf3ad-7ed9-4d12-b0a2-5966e697fc0b");
        if (isOff(ALLOW_2_SLASHES) && (slash2Count > 0)) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "ed30eea3-22db-4333-88d8-66b6992e635d");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "4ff65df9-fd58-465f-b403-123211110fd1");
        return true;
    }

    /**
     * Returns true if the query is null or it's a properly formatted query string.
     * @param query Query value to validate.
     * @return true if query is valid.
     */
    protected boolean isValidQuery(String query) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "017b51c9-5a96-405d-9026-5ae316f0f1e9");
        if (query == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "681b03f8-aae4-4f71-be2d-5cfa7df49309");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "572a6e41-1eb2-4709-8e9c-c370d0d46e63");
        return QUERY_PATTERN.matcher(query).matches();
    }

    /**
     * Returns true if the given fragment is null or fragments are allowed.
     * @param fragment Fragment value to validate.
     * @return true if fragment is valid.
     */
    protected boolean isValidFragment(String fragment) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "5d20036a-c795-4eff-8f75-e5eed96a84d8");
        if (fragment == null) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "ca8186b6-7bb3-43f8-9260-4ef9384fcd7a");
            return true;
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "579a4d61-75a4-4a21-978f-344e785effeb");
        return isOff(NO_FRAGMENTS);
    }

    /**
     * Returns the number of times the token appears in the target.
     * @param token Token value to be counted.
     * @param target Target value to count tokens in.
     * @return the number of tokens.
     */
    protected int countToken(String token, String target) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "d62ec53f-8b36-437f-bc9d-b0acfc2cefe2");
        int tokenIndex = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "94458954-7bf1-4e2b-b6d0-c3932aaaa08f");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "52a9eaa9-515a-4cc8-ac60-6c562a7c8b27");
        while (tokenIndex != -1) {
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "938e187f-9799-4a43-9ccf-d38b74887856");
            tokenIndex = target.indexOf(token, tokenIndex);
            writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "9e3c90f1-b50d-4c28-b394-354a4d28748b");
            if (tokenIndex > -1) {
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "09336589-dc55-48f3-a4bd-7a20c5d4f9e9");
                tokenIndex++;
                writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "61345111-37b3-4f92-9cb1-00442079946e");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "9f32e5c6-316e-456b-9c66-9e37fcfa396e");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "fffb76e0-a274-4c18-9c2f-1cb4c19a13c8");
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
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "593b7369-776b-455c-9213-fb6a6a0ffeb9");
        return (options & flag) == 0;
    }

    Matcher matchURL(String value) {
        writeline("/home/ubuntu/results/coverage/UrlValidator/UrlValidator_3_10.coverage", "1cfc30b6-3469-4929-8ca2-39b3cee0b83c");
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
