package org.apache.hc.core5.net;

import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.apache.hc.core5.http.NameValuePair;
import org.apache.hc.core5.http.message.BasicNameValuePair;
import org.apache.hc.core5.util.TextUtils;
import java.io.*;

/**
 * Builder for {@link URI} instances.
 *
 * @since 4.2
 */
public class URIBuilder {

    private String scheme;

    private String encodedSchemeSpecificPart;

    private String encodedAuthority;

    private String userInfo;

    private String encodedUserInfo;

    private String host;

    private int port;

    private String path;

    private String encodedPath;

    private String encodedQuery;

    private List<NameValuePair> queryParams;

    private String query;

    private Charset charset;

    private String fragment;

    private String encodedFragment;

    /**
     * Constructs an empty instance.
     */
    public URIBuilder() {
        super();
        this.port = -1;
    }

    /**
     * Construct an instance from the string which must be a valid URI.
     *
     * @param string a valid URI in string form
     * @throws URISyntaxException if the input is not a valid URI
     */
    public URIBuilder(final String string) throws URISyntaxException {
        super();
        digestURI(new URI(string));
    }

    /**
     * Construct an instance from the provided URI.
     * @param uri
     */
    public URIBuilder(final URI uri) {
        super();
        digestURI(uri);
    }

    /**
     * @since 4.4
     */
    public URIBuilder setCharset(final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "942c1cb6-6cf1-48ba-b8fb-f93f18c490d8");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "976a6955-bfbf-4bed-9e53-1007679a8936");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "cd93e8ff-8f47-4608-ae31-2e5630e59634");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "1e40408f-7fde-4499-ad7b-94b286498d73");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "b64fc8a2-8c78-4ce2-b99d-15ecfd833f27");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "0c694f77-1daf-4450-8205-5235afb6b35d");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "d8e7d887-ba79-4f5d-aa4a-8d8ae1af8283");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3ebe523d-4d4e-4571-9dbb-2f9376d511f8");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "1a6c2b8d-44c5-45c6-a079-1c5a5659a0e8");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "564969d0-31aa-4965-aa30-a7fc70f031ce");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "72f0f57a-8a1e-4bdc-b444-87bbf5cf2bca");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "aa1559a4-f8af-4a48-9ab1-b727ea91665f");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "dce8d524-2388-46c9-a417-b5eb24dcf668");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "0490d4a2-8783-48e1-aced-0394ee4ac7eb");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "a8b904f2-b778-4620-a649-6bb6ce31b9f9");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "472c8fa1-6669-4fc0-a076-bab709437084");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "fff67ba4-4a2f-4b72-815b-59a03348ac6d");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "a502ef8e-e8c3-4c28-bf24-8fd54f7a2691");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "fbc20f27-7ece-4fb3-8cfd-b7387c384567");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "534edcb7-e7ae-4f8d-a7be-3a58cbc3f555");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f9a29077-3412-4763-b5ec-b8b8a121fa30");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "dde532cb-545a-4036-a8dd-c7b93e96d0e6");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "e3876fb7-16e0-476b-8454-a0207ed920c1");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "2f17dbfe-6513-4def-924b-05692d554e8b");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3ceb7b5e-e45e-4392-af05-38d9f76288df");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "c717cc4f-db0b-4288-bf6c-e0d1bbc92e18");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "1075709a-9f0e-4a96-a5b8-1de47838cd66");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3b4b6590-fd63-4574-a0be-2da464033c08");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "799c3708-2089-4a8e-8f99-2a0c794233ad");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "5f0aa56e-7733-437b-b63b-8b6a74815cd1");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "c83fbe39-b924-450a-a753-7ead98db7e5f");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7e6c80a4-369c-4a0b-a755-53fa65a26223");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "78bf4111-6084-49a8-9cbc-b4f5e1b799e5");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7e744d67-ac5c-4ff9-87cd-37eb4e7f751b");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "a94c123b-5663-409f-8658-86f3288f60a6");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7512868c-662f-4c60-b184-bd6ea40e615a");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "747826f9-130e-48a8-a528-109ba40b530b");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "d0eb5228-083a-41eb-964b-1908f909becc");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "cc15f8f7-ad54-47c4-b0f4-7478b80c9947");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7db6c734-eb24-495a-a6df-7c98ccfc4f2e");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "4f75e7dc-2322-4aa0-9167-138b2e4d9c61");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "155df981-225e-46e3-84b2-575735dcffc8");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "039b33e8-09b8-4ea2-bea5-8bb5c6bf84b8");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f8960cb8-fb5b-4ab5-8d39-b178d97562fd");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "455f0e62-dd7c-43eb-b2dd-4173892cb86f");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3ca62c15-5b87-4ec0-babd-3a03ca47529f");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "31debcf6-39f1-4c3e-b3fc-34cdfe4570ab");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "39a17147-4970-400d-9c21-f918bcd0cdb4");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "5132b49d-a404-41f7-a332-71bd718e0727");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "15e5d333-dec3-4c25-b1d1-87d88b13dccd");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "a528bf45-b921-468a-902e-5ce2f5b50d19");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "090f163f-e376-4f04-a20c-8a47272f822b");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "8db12729-3a85-4a1f-8847-57d8fdeeab45");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "856091c3-610e-415d-a335-0ae1c213e3b7");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "0d0496d8-e3ac-4fb7-a2be-2a25b1b19fe6");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "bd61ea4e-212c-4956-84c2-9adfe3ea7eb1");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7463c19a-099a-4ea3-9ebc-95a9669299ce");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "dc1e3ab4-fdc1-4b3d-8930-678c69912341");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "52ffcc29-35d9-4d82-89b6-1160f10196a3");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "bf4fd06d-499d-4778-a84c-f22b4fd2e899");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "2d64ef37-c4f2-4117-b12d-4a35eee724c0");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "d0688fff-16a3-47a2-b03a-e9321b308cf9");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "135997cf-a1ef-4bae-b995-8c35d09a9714");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "1282bf48-b014-4917-9b40-7437d82af6a1");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "db79e782-834c-4d8e-b85c-22cfb767de83");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "8de4ec4e-1414-4ef2-a4d8-7742567433c3");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "38ae0368-f114-4aad-ac2c-0dc29d7b887b");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f7ab6988-6b54-40ed-a000-cbf8d58229f2");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "da7a5462-2acd-4871-82a5-cca01aabb4df");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "06869f26-f974-440c-8ca9-fcfef9c4a74c");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7255b58d-0f18-433a-9bc9-88e585e10b15");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "cae89dea-1d63-4ca3-9fa1-dc069778676d");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "c5da67dc-7257-44ee-854f-e6cfcf271dd4");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "78488da3-60f3-429d-9c5a-bbbe36ab3b3e");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "073c46e7-776b-47c7-8461-e1898aedf122");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "62138066-a4cf-47b0-933b-4629e2b1f24a");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "9d8fddd7-97b9-4c17-838c-b9739318efed");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "de77627b-6ccb-453c-951b-8743b8256279");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "feaf9398-613e-4104-a9aa-a6ce3459dee9");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7cf8d357-b30f-4d81-801b-c25e9e29411a");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "4d641560-605a-49bb-bbe7-808ea78e81c8");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "1efe2790-2ec1-428a-acf0-770b098ed2a3");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "5a700410-77e4-4d3d-8f83-8d72b39c2be8");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "39b67c0f-993d-48ab-bb7b-7672adeb6871");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "4a6ae9ab-1748-4ab7-8384-08834efa66c1");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "1a984eb7-4174-4180-aa88-e9c32094a943");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "2442284d-dfe8-494d-ae57-6e22ad8c95f2");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "54cea739-4df8-4e73-be59-579bdc6bbacb");
        return this;
    }

    /**
     * Sets URI query parameters. The parameter name / values are expected to be unescaped
     * and may contain non ASCII characters.
     * <p>
     * Please note query parameters and custom query component are mutually exclusive. This method
     * will remove custom query if present.
     * </p>
     *
     * @since 4.3
     */
    public URIBuilder setParameters(final List<NameValuePair> nvps) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "e39dbe4a-e36e-4403-8fb7-dfb424b57b7e");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "d15d025d-99bb-4bba-84e6-a5013e9df2da");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f0ce0717-0b50-4deb-8f7c-2caeb94e677c");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "db4c6926-941e-4af7-b86f-530ad9054bdc");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "edd31a9c-dc8c-43f9-b67c-b8204d2f9467");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "d584f50b-33db-42dc-90be-d7abcf29af76");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "9569a4f2-fce2-40ca-b9c6-48a6a5282584");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7b9a8745-e4c7-4dfa-afb1-47118997497a");
        return this;
    }

    /**
     * Adds URI query parameters. The parameter name / values are expected to be unescaped
     * and may contain non ASCII characters.
     * <p>
     * Please note query parameters and custom query component are mutually exclusive. This method
     * will remove custom query if present.
     * </p>
     *
     * @since 4.3
     */
    public URIBuilder addParameters(final List<NameValuePair> nvps) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "03a43ebf-ca04-45c7-89bf-7380ea1663d1");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "9051bbf2-240d-429d-9c26-37824c787e30");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "e1779f3c-f0fd-4411-a151-2ed91bb0d08a");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f2048293-a5a9-4b10-ad93-a834957d3314");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "ed3a8151-29af-4eb7-9934-fafef0d3102c");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "fb62cc38-80c8-4025-b07f-65e87c109467");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "434a41d3-d783-448b-a4da-746fbbedcbcf");
        return this;
    }

    /**
     * Sets URI query parameters. The parameter name / values are expected to be unescaped
     * and may contain non ASCII characters.
     * <p>
     * Please note query parameters and custom query component are mutually exclusive. This method
     * will remove custom query if present.
     * </p>
     *
     * @since 4.3
     */
    public URIBuilder setParameters(final NameValuePair... nvps) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "c5357565-19c4-4aca-abb4-8dd822cb7e17");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f8ac9add-9e46-4c11-a312-0f91e4baa1f0");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3e8fbce4-c8aa-41f8-8546-fc999b9738e2");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "dee4e5aa-7d2a-43b2-84fe-ae28e764080a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f86581d0-e3b0-40d4-aaf5-6ec4d55ddeec");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "40b8289f-3af9-4429-960d-532523a89889");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "baed6a34-f638-4a4e-bc16-f45d9dae6b95");
        return this;
    }

    /**
     * Adds parameter to URI query. The parameter name and value are expected to be unescaped
     * and may contain non ASCII characters.
     * <p>
     * Please note query parameters and custom query component are mutually exclusive. This method
     * will remove custom query if present.
     * </p>
     */
    public URIBuilder addParameter(final String param, final String value) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "59734b0d-65c7-4c2f-9a2d-56b49b7b48d3");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "c14a54b9-d23e-47b3-a24b-9b9de3ad8898");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "283cdba8-3a92-4555-93f0-cdab4e353777");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3a27c044-204b-4a66-8a7a-7df6abd0050a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "36c88210-1c0b-4810-8f20-1fde07d10e8c");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "dde67fd3-d935-4df5-8197-475815df56d1");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "00653857-490f-42af-9ffe-a1a0395d07e9");
        return this;
    }

    /**
     * Sets parameter of URI query overriding existing value if set. The parameter name and value
     * are expected to be unescaped and may contain non ASCII characters.
     * <p>
     * Please note query parameters and custom query component are mutually exclusive. This method
     * will remove custom query if present.
     * </p>
     */
    public URIBuilder setParameter(final String param, final String value) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3e9da5e6-b23f-4bd0-b0fe-175086c10fc0");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "a1fb0f65-9bbe-4994-bf05-afc892acb379");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3a46c56b-4500-4d33-889e-28fff068b30e");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "6475d8e3-232c-40dc-be51-85281099f692");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7032eb7d-1d0b-49c2-9dd1-92dd0bec581c");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "9176e96b-e393-4796-89a9-47c56dd4340a");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "66f4289e-217c-445b-8d50-f846626ffa84");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "7b5654d3-dfaf-4f16-9472-6369bde64f66");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "cf93868c-ea61-4d3f-b0c0-6059deabd8a4");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "a727398e-7060-407c-b32b-a768a4592a16");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "b948be85-7f66-48ba-aa0b-fda21355ca43");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "8f1eb268-4493-46cd-880a-f47a73dbe4ef");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "1eb6be52-7e53-417f-919b-da80c375fa4b");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "5b46e558-785a-4a2d-894a-9704df6f7822");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "2fa23bae-d8ee-45d9-a746-13c2a2714d1d");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "552f18ed-ad09-4a3f-a852-f06b1f910af7");
        return this;
    }

    /**
     * Sets custom URI query. The value is expected to be unescaped and may contain non ASCII
     * characters.
     * <p>
     * Please note query parameters and custom query component are mutually exclusive. This method
     * will remove query parameters if present.
     * </p>
     *
     * @since 4.3
     */
    public URIBuilder setCustomQuery(final String query) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "8bfd2ddc-1b3a-49b7-be8e-05f3f363fe89");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "ff776b0d-9509-4776-8d93-0ab076256fd2");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f97e9bfe-2464-4e3a-9cab-8a7cc96a73fa");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "5c7b2649-8525-4a27-949b-29ca55a44df9");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "4b08924b-1212-4b94-a39f-7e45d9cb374c");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "9dfacdae-025d-4d20-ad95-4b0ffac2ee5b");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "e8b78afc-5420-4176-b3b0-070bfdc50c25");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "0dd61ca0-b31e-4a0b-b31c-60593afd0c91");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "0b30e08d-9e13-44e9-85da-2a8a91aed2e8");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "39a91d3f-f32c-4c73-95b6-00ef45618ba3");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f1588037-0643-4213-80ac-9e1a708a8d65");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "b75d6cfd-fde0-4033-94e6-7461a6b4d93b");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "6e4c7347-f032-4e86-abb6-1340f66fe77f");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "f3a78943-b0a6-47d5-904b-9cc315165f85");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "e1a61f74-a31d-4b9a-ab34-80a8e49dc055");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "3695e8a3-e366-41e5-b285-b5274c45d34b");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "22f9a6a1-7ebb-4c0b-9481-c2a624bb415f");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "6c97341b-069a-463d-86de-859a27dfa4a2");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "5bd19bf2-3dc0-4d39-886b-1fd752154c63");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_3_10.coverage", "a2c7419f-eff1-4ce3-88cb-433c8cb4e8f7");
        return buildString();
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
