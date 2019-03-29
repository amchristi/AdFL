package org.apache.commons.validator.routines;

import java.io.Serializable;
import java.net.IDN;
import java.util.Arrays;
import java.util.Locale;

/**
 * <p><b>Domain name</b> validation routines.</p>
 *
 * <p>
 * This validator provides methods for validating Internet domain names
 * and top-level domains.
 * </p>
 *
 * <p>Domain names are evaluated according
 * to the standards <a href="http://www.ietf.org/rfc/rfc1034.txt">RFC1034</a>,
 * section 3, and <a href="http://www.ietf.org/rfc/rfc1123.txt">RFC1123</a>,
 * section 2.1. No accommodation is provided for the specialized needs of
 * other applications; if the domain name has been URL-encoded, for example,
 * validation will fail even though the equivalent plaintext version of the
 * same name would have passed.
 * </p>
 *
 * <p>
 * Validation is also provided for top-level domains (TLDs) as defined and
 * maintained by the Internet Assigned Numbers Authority (IANA):
 * </p>
 *
 *   <ul>
 *     <li>{@link #isValidInfrastructureTld} - validates infrastructure TLDs
 *         (<code>.arpa</code>, etc.)</li>
 *     <li>{@link #isValidGenericTld} - validates generic TLDs
 *         (<code>.com, .org</code>, etc.)</li>
 *     <li>{@link #isValidCountryCodeTld} - validates country code TLDs
 *         (<code>.us, .uk, .cn</code>, etc.)</li>
 *   </ul>
 *
 * <p>
 * (<b>NOTE</b>: This class does not provide IP address lookup for domain names or
 * methods to ensure that a given domain name matches a specific IP; see
 * {@link java.net.InetAddress} for that functionality.)
 * </p>
 *
 * @version $Revision: 1713225 $
 * @since Validator 1.4
 */
public class DomainValidator implements Serializable {

    private static final String[] EMPTY_STRING_ARRAY = new String[0];

    private static final long serialVersionUID = -4407125112880174009L;

    private static final String DOMAIN_LABEL_REGEX = "\\p{Alnum}(?>[\\p{Alnum}-]{0,61}\\p{Alnum})?";

    private static final String TOP_LABEL_REGEX = "\\p{Alpha}(?>[\\p{Alnum}-]{0,61}\\p{Alnum})?";

    private static final String DOMAIN_NAME_REGEX = "^(?:" + DOMAIN_LABEL_REGEX + "\\.)+" + "(" + TOP_LABEL_REGEX + ")\\.?$";

    private final boolean allowLocal;

    /**
     * Singleton instance of this validator, which
     *  doesn't consider local addresses as valid.
     */
    private static final DomainValidator DOMAIN_VALIDATOR = new DomainValidator(false);

    /**
     * Singleton instance of this validator, which does
     *  consider local addresses valid.
     */
    private static final DomainValidator DOMAIN_VALIDATOR_WITH_LOCAL = new DomainValidator(true);

    /**
     * RegexValidator for matching domains.
     */
    private final RegexValidator domainRegex = new RegexValidator(DOMAIN_NAME_REGEX);

    private final RegexValidator hostnameRegex = new RegexValidator(DOMAIN_LABEL_REGEX);

    /**
     * Returns the singleton instance of this validator. It
     *  will not consider local addresses as valid.
     * @return the singleton instance of this validator
     */
    public static synchronized DomainValidator getInstance() {
        return DOMAIN_VALIDATOR;
    }

    /**
     * Returns the singleton instance of this validator,
     *  with local validation as required.
     * @param allowLocal Should local addresses be considered valid?
     * @return the singleton instance of this validator
     */
    public static synchronized DomainValidator getInstance(boolean allowLocal) {
        if (allowLocal) {
            return DOMAIN_VALIDATOR_WITH_LOCAL;
        }
        return DOMAIN_VALIDATOR;
    }

    /** Private constructor. */
    private DomainValidator(boolean allowLocal) {
        this.allowLocal = allowLocal;
    }

    /**
     * Returns true if the specified <code>String</code> parses
     * as a valid domain name with a recognized top-level domain.
     * The parsing is case-insensitive.
     * @param domain the parameter to check for domain name syntax
     * @return true if the parameter is a valid domain name
     */
    public boolean isValid(String domain) {
        domain = unicodeToASCII(domain);
        String[] groups = domainRegex.match(domain);
        if (groups != null && groups.length > 0) {
            return isValidTld(groups[0]);
        }
        return allowLocal && hostnameRegex.isValid(domain);
    }

    final boolean isValidDomainSyntax(String domain) {
        if (domain.length() > 253) {
            return false;
        }
        String[] groups = domainRegex.match(domain);
        return (groups != null && groups.length > 0) || hostnameRegex.isValid(domain);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined top-level domain. Leading dots are ignored if present.
     * The search is case-insensitive.
     * @param tld the parameter to check for TLD status, not null
     * @return true if the parameter is a TLD
     */
    public boolean isValidTld(String tld) {
        if (allowLocal && isValidLocalTld(tld)) {
            return true;
        }
        return isValidInfrastructureTld(tld) || isValidGenericTld(tld) || isValidCountryCodeTld(tld);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined infrastructure top-level domain. Leading dots are
     * ignored if present. The search is case-insensitive.
     * @param iTld the parameter to check for infrastructure TLD status, not null
     * @return true if the parameter is an infrastructure TLD
     */
    public boolean isValidInfrastructureTld(String iTld) {
        final String key = chompLeadingDot(unicodeToASCII(iTld).toLowerCase(Locale.ENGLISH));
        return arrayContains(INFRASTRUCTURE_TLDS, key);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined generic top-level domain. Leading dots are ignored
     * if present. The search is case-insensitive.
     * @param gTld the parameter to check for generic TLD status, not null
     * @return true if the parameter is a generic TLD
     */
    public boolean isValidGenericTld(String gTld) {
        final String key = chompLeadingDot(unicodeToASCII(gTld).toLowerCase(Locale.ENGLISH));
        return (arrayContains(GENERIC_TLDS, key) || arrayContains(GENERIC_TLDS_PLUS, key)) && !arrayContains(GENERIC_TLDS_MINUS, key);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * IANA-defined country code top-level domain. Leading dots are
     * ignored if present. The search is case-insensitive.
     * @param ccTld the parameter to check for country code TLD status, not null
     * @return true if the parameter is a country code TLD
     */
    public boolean isValidCountryCodeTld(String ccTld) {
        final String key = chompLeadingDot(unicodeToASCII(ccTld).toLowerCase(Locale.ENGLISH));
        return (arrayContains(COUNTRY_CODE_TLDS, key) || arrayContains(COUNTRY_CODE_TLDS_PLUS, key)) && !arrayContains(COUNTRY_CODE_TLDS_MINUS, key);
    }

    /**
     * Returns true if the specified <code>String</code> matches any
     * widely used "local" domains (localhost or localdomain). Leading dots are
     * ignored if present. The search is case-insensitive.
     * @param lTld the parameter to check for local TLD status, not null
     * @return true if the parameter is an local TLD
     */
    public boolean isValidLocalTld(String lTld) {
        final String key = chompLeadingDot(unicodeToASCII(lTld).toLowerCase(Locale.ENGLISH));
        return arrayContains(LOCAL_TLDS, key);
    }

    private String chompLeadingDot(String str) {
        if (str.startsWith(".")) {
            return str.substring(1);
        }
        return str;
    }

    private static final String[] INFRASTRUCTURE_TLDS = new String[] { "arpa" };

    private static final String[] GENERIC_TLDS = new String[] { "aaa", "aarp", "abb", "abbott", "abogado", "academy", "accenture", "accountant", "accountants", "aco", "active", "actor", "ads", "adult", "aeg", "aero", "afl", "agency", "aig", "airforce", "airtel", "allfinanz", "alsace", "amica", "amsterdam", "android", "apartments", "app", "apple", "aquarelle", "aramco", "archi", "army", "arte", "asia", "associates", "attorney", "auction", "audio", "auto", "autos", "axa", "azure", "band", "bank", "bar", "barcelona", "barclaycard", "barclays", "bargains", "bauhaus", "bayern", "bbc", "bbva", "bcn", "beats", "beer", "bentley", "berlin", "best", "bet", "bharti", "bible", "bid", "bike", "bing", "bingo", "bio", "biz", "black", "blackfriday", "bloomberg", "blue", "bms", "bmw", "bnl", "bnpparibas", "boats", "bom", "bond", "boo", "boots", "boutique", "bradesco", "bridgestone", "broker", "brother", "brussels", "budapest", "build", "builders", "business", "buzz", "bzh", "cab", "cafe", "cal", "camera", "camp", "cancerresearch", "canon", "capetown", "capital", "car", "caravan", "cards", "care", "career", "careers", "cars", "cartier", "casa", "cash", "casino", "cat", "catering", "cba", "cbn", "ceb", "center", "ceo", "cern", "cfa", "cfd", "chanel", "channel", "chat", "cheap", "chloe", "christmas", "chrome", "church", "cipriani", "cisco", "citic", "city", "claims", "cleaning", "click", "clinic", "clothing", "cloud", "club", "clubmed", "coach", "codes", "coffee", "college", "cologne", "com", "commbank", "community", "company", "computer", "condos", "construction", "consulting", "contractors", "cooking", "cool", "coop", "corsica", "country", "coupons", "courses", "credit", "creditcard", "cricket", "crown", "crs", "cruises", "csc", "cuisinella", "cymru", "cyou", "dabur", "dad", "dance", "date", "dating", "datsun", "day", "dclk", "deals", "degree", "delivery", "dell", "delta", "democrat", "dental", "dentist", "desi", "design", "dev", "diamonds", "diet", "digital", "direct", "directory", "discount", "dnp", "docs", "dog", "doha", "domains", "doosan", "download", "drive", "durban", "dvag", "earth", "eat", "edu", "education", "email", "emerck", "energy", "engineer", "engineering", "enterprises", "epson", "equipment", "erni", "esq", "estate", "eurovision", "eus", "events", "everbank", "exchange", "expert", "exposed", "express", "fage", "fail", "faith", "family", "fan", "fans", "farm", "fashion", "feedback", "ferrero", "film", "final", "finance", "financial", "firmdale", "fish", "fishing", "fit", "fitness", "flights", "florist", "flowers", "flsmidth", "fly", "foo", "football", "forex", "forsale", "forum", "foundation", "frl", "frogans", "fund", "furniture", "futbol", "fyi", "gal", "gallery", "game", "garden", "gbiz", "gdn", "gea", "gent", "genting", "ggee", "gift", "gifts", "gives", "giving", "glass", "gle", "global", "globo", "gmail", "gmo", "gmx", "gold", "goldpoint", "golf", "goo", "goog", "google", "gop", "gov", "graphics", "gratis", "green", "gripe", "group", "gucci", "guge", "guide", "guitars", "guru", "hamburg", "hangout", "haus", "healthcare", "help", "here", "hermes", "hiphop", "hitachi", "hiv", "hockey", "holdings", "holiday", "homedepot", "homes", "honda", "horse", "host", "hosting", "hoteles", "hotmail", "house", "how", "hsbc", "hyundai", "ibm", "icbc", "ice", "icu", "ifm", "iinet", "immo", "immobilien", "industries", "infiniti", "info", "ing", "ink", "institute", "insure", "int", "international", "investments", "ipiranga", "irish", "ist", "istanbul", "itau", "iwc", "jaguar", "java", "jcb", "jetzt", "jewelry", "jlc", "jll", "jobs", "joburg", "jprs", "juegos", "kaufen", "kddi", "kia", "kim", "kinder", "kitchen", "kiwi", "koeln", "komatsu", "krd", "kred", "kyoto", "lacaixa", "lancaster", "land", "landrover", "lasalle", "lat", "latrobe", "law", "lawyer", "lds", "lease", "leclerc", "legal", "lexus", "lgbt", "liaison", "lidl", "life", "lighting", "limited", "limo", "linde", "link", "live", "lixil", "loan", "loans", "lol", "london", "lotte", "lotto", "love", "ltd", "ltda", "lupin", "luxe", "luxury", "madrid", "maif", "maison", "man", "management", "mango", "market", "marketing", "markets", "marriott", "mba", "media", "meet", "melbourne", "meme", "memorial", "men", "menu", "meo", "miami", "microsoft", "mil", "mini", "mma", "mobi", "moda", "moe", "moi", "mom", "monash", "money", "montblanc", "mormon", "mortgage", "moscow", "motorcycles", "mov", "movie", "movistar", "mtn", "mtpc", "mtr", "museum", "mutuelle", "nadex", "nagoya", "name", "navy", "nec", "net", "netbank", "network", "neustar", "new", "news", "nexus", "ngo", "nhk", "nico", "ninja", "nissan", "nokia", "nra", "nrw", "ntt", "nyc", "obi", "office", "okinawa", "omega", "one", "ong", "onl", "online", "ooo", "oracle", "orange", "org", "organic", "osaka", "otsuka", "ovh", "page", "panerai", "paris", "partners", "parts", "party", "pet", "pharmacy", "philips", "photo", "photography", "photos", "physio", "piaget", "pics", "pictet", "pictures", "ping", "pink", "pizza", "place", "play", "playstation", "plumbing", "plus", "pohl", "poker", "porn", "post", "praxi", "press", "pro", "prod", "productions", "prof", "properties", "property", "protection", "pub", "qpon", "quebec", "racing", "realtor", "realty", "recipes", "red", "redstone", "rehab", "reise", "reisen", "reit", "ren", "rent", "rentals", "repair", "report", "republican", "rest", "restaurant", "review", "reviews", "rich", "ricoh", "rio", "rip", "rocher", "rocks", "rodeo", "rsvp", "ruhr", "run", "rwe", "ryukyu", "saarland", "sakura", "sale", "samsung", "sandvik", "sandvikcoromant", "sanofi", "sap", "sapo", "sarl", "saxo", "sbs", "sca", "scb", "schmidt", "scholarships", "school", "schule", "schwarz", "science", "scor", "scot", "seat", "security", "seek", "sener", "services", "seven", "sew", "sex", "sexy", "shiksha", "shoes", "show", "shriram", "singles", "site", "ski", "sky", "skype", "sncf", "soccer", "social", "software", "sohu", "solar", "solutions", "sony", "soy", "space", "spiegel", "spreadbetting", "srl", "stada", "starhub", "statoil", "stc", "stcgroup", "stockholm", "studio", "study", "style", "sucks", "supplies", "supply", "support", "surf", "surgery", "suzuki", "swatch", "swiss", "sydney", "systems", "taipei", "tatamotors", "tatar", "tattoo", "tax", "taxi", "team", "tech", "technology", "tel", "telefonica", "temasek", "tennis", "thd", "theater", "theatre", "tickets", "tienda", "tips", "tires", "tirol", "today", "tokyo", "tools", "top", "toray", "toshiba", "tours", "town", "toyota", "toys", "trade", "trading", "training", "travel", "trust", "tui", "ubs", "university", "uno", "uol", "vacations", "vegas", "ventures", "versicherung", "vet", "viajes", "video", "villas", "vin", "virgin", "vision", "vista", "vistaprint", "viva", "vlaanderen", "vodka", "vote", "voting", "voto", "voyage", "wales", "walter", "wang", "watch", "webcam", "website", "wed", "wedding", "weir", "whoswho", "wien", "wiki", "williamhill", "win", "windows", "wine", "wme", "work", "works", "world", "wtc", "wtf", "xbox", "xerox", "xin", "xn--11b4c3d", "xn--1qqw23a", "xn--30rr7y", "xn--3bst00m", "xn--3ds443g", "xn--3pxu8k", "xn--42c2d9a", "xn--45q11c", "xn--4gbrim", "xn--55qw42g", "xn--55qx5d", "xn--6frz82g", "xn--6qq986b3xl", "xn--80adxhks", "xn--80asehdb", "xn--80aswg", "xn--9dbq2a", "xn--9et52u", "xn--b4w605ferd", "xn--c1avg", "xn--c2br7g", "xn--cg4bki", "xn--czr694b", "xn--czrs0t", "xn--czru2d", "xn--d1acj3b", "xn--efvy88h", "xn--estv75g", "xn--fhbei", "xn--fiq228c5hs", "xn--fiq64b", "xn--fjq720a", "xn--flw351e", "xn--hxt814e", "xn--i1b6b1a6a2e", "xn--imr513n", "xn--io0a7i", "xn--j1aef", "xn--kcrx77d1x4a", "xn--kput3i", "xn--mgba3a3ejt", "xn--mgbab2bd", "xn--mk1bu44c", "xn--mxtq1m", "xn--ngbc5azd", "xn--nqv7f", "xn--nqv7fs00ema", "xn--nyqy26a", "xn--p1acf", "xn--pssy2u", "xn--q9jyb4c", "xn--qcka1pmc", "xn--rhqv96g", "xn--ses554g", "xn--t60b56a", "xn--tckwe", "xn--unup4y", "xn--vermgensberater-ctb", "xn--vermgensberatung-pwb", "xn--vhquv", "xn--vuq861b", "xn--xhq521b", "xn--zfr164b", "xperia", "xxx", "xyz", "yachts", "yamaxun", "yandex", "yodobashi", "yoga", "yokohama", "youtube", "zara", "zip", "zone", "zuerich" };

    private static final String[] COUNTRY_CODE_TLDS = new String[] { "ac", "ad", "ae", "af", "ag", "ai", "al", "am", "ao", "aq", "ar", "as", "at", "au", "aw", "ax", "az", "ba", "bb", "bd", "be", "bf", "bg", "bh", "bi", "bj", "bm", "bn", "bo", "br", "bs", "bt", "bv", "bw", "by", "bz", "ca", "cc", "cd", "cf", "cg", "ch", "ci", "ck", "cl", "cm", "cn", "co", "cr", "cu", "cv", "cw", "cx", "cy", "cz", "de", "dj", "dk", "dm", "do", "dz", "ec", "ee", "eg", "er", "es", "et", "eu", "fi", "fj", "fk", "fm", "fo", "fr", "ga", "gb", "gd", "ge", "gf", "gg", "gh", "gi", "gl", "gm", "gn", "gp", "gq", "gr", "gs", "gt", "gu", "gw", "gy", "hk", "hm", "hn", "hr", "ht", "hu", "id", "ie", "il", "im", "in", "io", "iq", "ir", "is", "it", "je", "jm", "jo", "jp", "ke", "kg", "kh", "ki", "km", "kn", "kp", "kr", "kw", "ky", "kz", "la", "lb", "lc", "li", "lk", "lr", "ls", "lt", "lu", "lv", "ly", "ma", "mc", "md", "me", "mg", "mh", "mk", "ml", "mm", "mn", "mo", "mp", "mq", "mr", "ms", "mt", "mu", "mv", "mw", "mx", "my", "mz", "na", "nc", "ne", "nf", "ng", "ni", "nl", "no", "np", "nr", "nu", "nz", "om", "pa", "pe", "pf", "pg", "ph", "pk", "pl", "pm", "pn", "pr", "ps", "pt", "pw", "py", "qa", "re", "ro", "rs", "ru", "rw", "sa", "sb", "sc", "sd", "se", "sg", "sh", "si", "sj", "sk", "sl", "sm", "sn", "so", "sr", "st", "su", "sv", "sx", "sy", "sz", "tc", "td", "tf", "tg", "th", "tj", "tk", "tl", "tm", "tn", "to", "tr", "tt", "tv", "tw", "tz", "ua", "ug", "uk", "us", "uy", "uz", "va", "vc", "ve", "vg", "vi", "vn", "vu", "wf", "ws", "xn--3e0b707e", "xn--45brj9c", "xn--80ao21a", "xn--90a3ac", "xn--90ais", "xn--clchc0ea0b2g2a9gcd", "xn--d1alf", "xn--fiqs8s", "xn--fiqz9s", "xn--fpcrj9c3d", "xn--fzc2c9e2c", "xn--gecrj9c", "xn--h2brj9c", "xn--j1amh", "xn--j6w193g", "xn--kprw13d", "xn--kpry57d", "xn--l1acc", "xn--lgbbat1ad8j", "xn--mgb9awbf", "xn--mgba3a4f16a", "xn--mgbaam7a8h", "xn--mgbayh7gpa", "xn--mgbbh1a71e", "xn--mgbc0a9azcg", "xn--mgberp4a5d4ar", "xn--mgbpl2fh", "xn--mgbtx2b", "xn--mgbx4cd0ab", "xn--node", "xn--o3cw4h", "xn--ogbpf8fl", "xn--p1ai", "xn--pgbs0dh", "xn--qxam", "xn--s9brj9c", "xn--wgbh1c", "xn--wgbl6a", "xn--xkc2al3hye2a", "xn--xkc2dl3a5ee0h", "xn--y9a3aq", "xn--yfro4i67o", "xn--ygbi2ammx", "ye", "yt", "za", "zm", "zw" };

    private static final String[] LOCAL_TLDS = new String[] { "localdomain", "localhost" };

    private static boolean inUse = false;

    private static volatile String[] COUNTRY_CODE_TLDS_PLUS = EMPTY_STRING_ARRAY;

    private static volatile String[] GENERIC_TLDS_PLUS = EMPTY_STRING_ARRAY;

    private static volatile String[] COUNTRY_CODE_TLDS_MINUS = EMPTY_STRING_ARRAY;

    private static volatile String[] GENERIC_TLDS_MINUS = EMPTY_STRING_ARRAY;

    /**
     * enum used by {@link DomainValidator#updateTLDOverride(ArrayType, String[])}
     * to determine which override array to update.
     * @since 1.5.0
     */
    enum ArrayType {

        /** Update the GENERIC_TLDS_PLUS table containing additonal generic TLDs */
        GENERIC_PLUS, /** Update the GENERIC_TLDS_MINUS table containing deleted generic TLDs */
        GENERIC_MINUS, /** Update the COUNTRY_CODE_TLDS_PLUS table containing additonal country code TLDs */
        COUNTRY_CODE_PLUS, /** Update the COUNTRY_CODE_TLDS_MINUS table containing deleted country code TLDs */
        COUNTRY_CODE_MINUS
    }

    ;

    static synchronized void clearTLDOverrides() {
    }

    /**
     * Update one of the TLD override arrays.
     * This must only be done at program startup, before any instances are accessed using getInstance.
     * <p>
     * For example:
     * <p>
     * {@code DomainValidator.updateTLDOverride(ArrayType.GENERIC_PLUS, new String[]{"apache"})}
     * <p>
     * To clear an override array, provide an empty array.
     *
     * @param table the table to update, see {@link DomainValidator#ArrayType}
     * @param tlds the array of TLDs, must not be null
     * @throws IllegalStateException if the method is called after getInstance
     * @since 1.5.0
     */
    public static synchronized void updateTLDOverride(ArrayType table, String[] tlds) {
        String[] copy = new String[tlds.length];
        for (int i = 0; i < tlds.length; i++) {
            copy[i] = tlds[i].toLowerCase(Locale.ENGLISH);
        }
        switch(table) {
            case COUNTRY_CODE_MINUS:
                break;
            case COUNTRY_CODE_PLUS:
                break;
            case GENERIC_MINUS:
                GENERIC_TLDS_MINUS = copy;
                break;
            case GENERIC_PLUS:
                GENERIC_TLDS_PLUS = copy;
                break;
        }
    }

    static String unicodeToASCII(String input) {
        String returnValue = "";
        if (isOnlyASCII(input)) {
            return input;
        }
        try {
            final String ascii = IDN.toASCII(input);
            if (IDNBUGHOLDER.IDN_TOASCII_PRESERVES_TRAILING_DOTS) {
                return ascii;
            }
            final int length = input.length();
            if (length == 0) {
                return input;
            }
            char lastChar = input.charAt(length - 1);
            switch(lastChar) {
                case '.':
                    break;
                case '。':
                    break;
                case '．':
                    break;
                case '｡':
                    break;
                default:
                    break;
            }
        } catch (IllegalArgumentException e) {
            return input;
        }
        return returnValue;
    }

    private static class IDNBUGHOLDER {

        private static boolean keepsTrailingDot() {
            final String input = "a.";
            return input.equals(IDN.toASCII(input));
        }

        private static final boolean IDN_TOASCII_PRESERVES_TRAILING_DOTS = keepsTrailingDot();
    }

    private static boolean isOnlyASCII(String input) {
        if (input == null) {
            return true;
        }
        for (int i = 0; i < input.length(); i++) {
            if (input.charAt(i) > 0x7F) {
                return false;
            }
        }
        return true;
    }

    /**
     * Check if a sorted array contains the specified key
     *
     * @param sortedArray the array to search
     * @param key the key to find
     * @return {@code true} if the array contains the key
     */
    private static boolean arrayContains(String[] sortedArray, String key) {
        return Arrays.binarySearch(sortedArray, key) >= 0;
    }
}
