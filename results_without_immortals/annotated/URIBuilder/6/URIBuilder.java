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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "c9387297-0e05-4aa0-8fa6-3663a2b12de8");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "bfae0264-b3c3-4221-aeaf-fa7d380e4320");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "3784c81f-bfae-44ec-80d6-298166a1a899");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ad11e9db-922d-478f-bf3d-6cd84991b526");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e1b3eb93-c08e-4e9e-8413-56da754cbbc8");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "48679c07-f906-4b53-938d-c2e2c8dd66a2");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e1a958f1-c3de-48f1-87ec-73f8f5bf5370");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "8aea4703-4ef7-49f2-b058-332ecddece48");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "9d39295a-f80d-498b-b5f0-d508d3e7ca21");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "016b1031-2a6b-4338-ab64-ca5aa971f152");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "1f2d12e7-de88-4a35-a08c-6bd1a44ca708");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "1e787f28-1096-4e21-8a7d-40dd74cad482");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "c49bba91-68e1-413b-9d85-5fb0a8f5dd7f");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "90392fcf-e07c-4dd6-8a81-9bf72d351eb7");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "63d5953b-c161-44e9-9c1a-22c2fd62f573");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "050d5c20-1a7e-4c6d-95e7-8eb9f32f2c49");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "11a01d7c-b976-4535-b60e-5114a246fbb3");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "8ab9f0e5-028f-4023-b0b1-ea4dcc333916");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "92eb3b7a-9c40-4579-8160-812717d67886");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "fdee2f3e-2932-4d10-a77e-444a4d46e6f4");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "7da4c75c-f960-466d-b5a4-c7ad3cd8c9dd");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ac5ffea0-aa1b-4fa1-bef5-bfa2f8779e91");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d30f919d-d03b-4954-a0c5-864b3be19666");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "11d09249-38bd-497d-854f-45cbae7e3dd3");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "fb82940d-9aa7-4770-9dd7-30ac736aaf3f");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "1c1197bd-7fd1-49b8-aa04-f00e559345b3");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ba7ed4d8-4d43-4c48-927c-1015f7de1e49");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "3b710ff5-f77e-4c02-a1d4-c0b0ddce3081");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "13dbdb8f-df89-4c26-85f1-660e9b67aff3");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "db4790ed-4ec2-4378-b2b0-b5e8bcda9bd3");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ae4a0dfb-fa09-4044-bf2e-c8de557545b1");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "4a67bb38-f7ca-4466-952e-9c043361278b");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "aff58947-388a-4781-80a2-705db5eff221");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "878d6e46-0e52-4329-a854-98c659f7a974");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "fa3bf975-8b5e-4ca3-b1d2-a754c06b5dc3");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e6b6544f-3489-4be5-b632-7d4b7004f331");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e5c2b13f-cca4-4b45-baab-e8bfe6d83df3");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2f7114f6-9c80-4c23-aefa-94cca8ee5a6d");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "16db1525-cca6-4895-9c3c-089eb2818b31");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2f9c6cab-269c-4600-b5f7-a588c90e5407");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d84d7e95-a2c5-4e40-8e20-1259e458c9fe");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ddc75e08-b9bc-45c0-a965-705ceb5b6f8e");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "b5db216f-75de-4d0c-9afe-3187e4e7793d");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "6056cf5e-e9fe-4b3b-b371-1006f3e26b04");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "37cc0950-ba19-4252-a360-a06db4215fc6");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "5238c4ce-eb43-4633-a785-eae857ae847a");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "8a2edf90-3269-411e-99d4-182984f55ec0");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "9296ddbb-6607-463b-9c1c-72dfb9264319");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "fe3cf7d1-1d7e-40fb-a3e7-96abd6386396");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ccc42d78-28d2-4065-8d65-6c2e61003306");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ace897d9-13d3-411c-a7cf-4e3690f45040");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "86b689e2-47b4-437e-b602-3f6defac3df9");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e7fddd07-5e16-4d62-bf4e-9efc97c04e7f");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "9924bb0a-a694-4e13-b136-9b66de76bf1d");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "b3438eb3-02bf-4de9-8fc6-96b73f45b45f");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "fefc1479-9c08-4ff4-9579-50587c507e72");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "9aac7660-9c09-4dd4-a4c7-2772b2b15fb1");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "4f99143d-d49e-4d52-8594-d280237e476e");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "0fed7809-f0da-4d24-b6f8-6ccb1281e816");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d386d401-048e-4b55-a6f8-00edb442c0d9");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "9f336fca-4d65-4f13-bd41-a726a3e5d337");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "294fe4bb-e96d-4fd1-b7c8-8e257d5b034a");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ff053ba3-2808-42a7-85ce-a51353db961b");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "43be544d-f7fb-4fe9-bc0f-2bd027a58872");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "a80942c0-ae20-4131-81e1-4a8f821ec25e");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "b26362df-bace-4df7-8d9d-c74eb4d65fc5");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "986f6d49-f653-4333-9832-ce79dc6439c6");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "be73f9af-cc5a-4e58-a201-2c87ddf0fe1c");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "b0ed270c-00b7-40a4-8c71-e48a736e1f49");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ba292d90-25fc-43db-aff5-c4cd9b72bf84");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "74c7ae9d-4f40-418a-ab1e-ee6bf6705b20");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "c4a8c7b9-75ee-4f9f-aeb3-dbca30d9adfc");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e3b87158-f443-423e-b816-8136c7559585");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "7b90d050-9b37-46ec-bbf3-c9cdf7778696");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "db9b81d4-2d1e-4b91-85e9-292a57f988e3");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2268989f-1a56-4fb0-a496-2e5e2b97dc11");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "10d91f4e-cf79-4f7d-95cb-83d496df25f7");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d9e05642-3305-47e1-ad06-4a992e4913bb");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ca21e8bb-bae8-40f2-9d80-c0fb8c38a891");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "c60219ca-4457-4a96-804d-7309e128a08a");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "c4251906-c2c6-4e9d-80a9-534fcf306b06");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "daf734cc-4570-45f4-bd17-094e0445bffc");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "7099654f-cacd-44ca-9b6c-cc45da134066");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "01350944-b54e-42e4-9deb-7a6bb335cbd0");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "8d8b7a2a-79e2-43b0-a923-8754623dcd69");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "ca113708-9442-487d-817d-43cd033edc5f");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e47d8562-0226-49d7-8e4c-a5127d9f0933");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "9b9f8107-c4f8-4bd3-bdda-726328b02207");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d0dd2dea-2ade-48ba-9cb9-e4b88db0bea0");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "0a72a0d9-f059-426c-bb75-ff51ff027726");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "312580c0-ce41-477f-8542-ecba516240ee");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "591448b2-82ac-4e7c-8ef2-50f92d1dcf51");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "47c55b6d-ff87-4734-94ac-572b1962f7b7");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "6d9b56b5-64c0-474f-a9a1-8f184426baab");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "51523029-d91d-4114-9c81-9fb96a262ebc");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "c53fba76-0140-41f7-bd66-24c89746b99e");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "821bdf82-5037-41c0-a137-a4507f3ffd62");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "a3300b84-fb40-44a7-9e07-3dfdda140275");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "47fd807e-400a-4522-a818-de0859b12aa8");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "03608e1a-3386-4fd4-be6c-6839420628c3");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "51a9797e-dcad-47e8-a6ab-7bee8a8b32c7");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "7f6dfd23-22ec-4857-aee6-545b43682b30");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2f974bee-39eb-4b26-8b36-2da326724323");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "cb564956-37b4-4b3f-b684-10517fdf03db");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "f586df14-f770-4d98-b840-e109ec554cfe");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d6250400-6359-4321-b40b-344000c663be");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2c29bb27-89d5-423e-a889-abe440977ca0");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "c04ba1af-70b7-4d95-9c8f-4ffd6cde602e");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "1dd4d306-ebe6-4180-9d46-d8bc04619178");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "fcdb712f-1a94-4823-8cad-939b70b713be");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "7fbb2b37-4206-48fb-8946-f373939082ab");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "e43f315e-f7ff-465d-bb81-0c8b950863b0");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2b27b346-9699-42b0-8a5d-89762433478f");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "b7ad6bcf-4424-4677-aab8-ea763f57ae6e");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "42fc7bcf-334d-4f01-a7d0-f186fbec78c2");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "895f6d64-f024-4331-a981-a534c059e572");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "07656f47-999e-4c46-88c9-25fb9cb4c61d");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "755e4bea-eecf-4519-be1b-9a572f53f51e");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "8eae502d-e696-4fe2-b3ff-571a8fb255ff");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "4702bee9-577c-48cd-8ccb-1e407667b068");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "b6c54c6d-6265-4d7a-8a68-f7706f4bb6e1");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "21571b56-c685-4e0f-8a3d-1162fe131978");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "7adc4b27-980c-432b-9548-16731679c4f2");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "0cedf208-87fc-440b-b291-284d218e11cf");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "33d84467-3583-45f1-a012-5e7522934069");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2c5bf259-8a95-4e98-8b17-1145308c741b");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d233eb90-38dd-41f8-834b-0c7b7d2fedd7");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "0d573634-99f8-473b-9252-d15a924af7eb");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "5c77c086-0292-4830-b4b0-3129193daa80");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "7fde5eb5-5cba-4e76-8c0c-749ab6317f03");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "636386f6-c287-4ab4-95cc-d86122bfea8f");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "f2d06578-aa57-4471-9c2b-70ea6ccff9ca");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "2934743b-8c25-493c-8630-d0d398985aa8");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "029788d4-f0c7-4ee5-a407-79233ed0767a");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "139d2b7d-3143-4a54-a523-49aa2c381797");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "15a45d87-9928-4d70-8970-225d954c9426");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "76745d65-175c-4f17-b4b3-1f4463adb828");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "6a00ab5d-ced8-4811-93b2-42ff8f74d7c3");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "44aabfb1-84ae-4d6b-b557-4874974345af");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "33500b0b-c1c2-4b58-a65b-4faf1df60d63");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "0d7e22ca-4346-4879-a7d7-ddb4ae7c08b8");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "3f90865b-3453-40a1-82b9-e4314fb8e3c8");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "1165ddfa-8040-4e63-8a31-4e30550dc597");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "428cc808-39c7-4f9a-80a3-a3dc4250740b");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "3001e744-fe2f-4303-a019-b81403d55793");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "21076a09-1913-4b01-88ea-0c9df78d79bf");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "43c8455e-4f41-41e8-80dc-3c2295897e89");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "bde34321-9666-46f6-a5c0-b8acc34e0176");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "3e059972-06da-4226-b45e-ee9eb96c8e67");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "b006875d-a7f0-4844-8719-2218aae9870b");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "1e9ba18a-bdc7-4909-8e46-1c525c6dd1e9");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "70980584-d001-4314-945d-2dfb6dfee149");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_6_10.coverage", "d5d66bde-4db4-4ff0-8203-e003207d3180");
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
