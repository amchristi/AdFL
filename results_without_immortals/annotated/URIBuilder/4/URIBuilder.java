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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "1d70898f-e3d2-4db1-b4a5-3bb4275b3bd1");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f3b47538-5550-40d8-9b66-7fc9272b6c7d");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "89ec0ead-c2c6-4624-b446-1619e87b1de5");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "952cfaf6-b0aa-47e9-802c-ccc9335633b4");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "1cee50c7-f7af-41f4-8634-c2b151a83bc6");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "a7809d84-7b73-482f-88ff-24237873354e");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "e7a61da5-e8bd-487b-ba77-227640e875dc");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "bb573ed5-2076-4bdd-be2d-d34743b48705");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "b6397a77-884b-4c8a-b642-ab1ea6df870e");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "3fc42e86-bf3e-445c-9f49-76167d0f8e11");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "0e12a0a1-76df-40a5-a3f6-ef7c79480244");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "e23d64a2-9531-4352-857e-e003f69f6e43");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "b3b0e525-20da-425a-ba86-b59ba504ec0f");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "e50a8f66-6ee6-4cde-b9ae-3dd927264b5d");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f11778d4-0f3f-4e46-81fd-215104f4bf0b");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "fe369613-fbfd-44ad-bfdf-0499969fb381");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "9f11c600-87e8-4857-bb2a-de0cd2134768");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f87e716a-5e91-47b5-8873-9fcf8e0979a9");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "e2c12c31-80d9-4a4a-ab90-8c3c46d53f89");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "933ed184-39c6-4241-8d2e-8ddffb8a3a30");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "ec9c1e2e-a1af-41ec-aa50-2aba504915d2");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "db212045-220a-4bc7-8e74-76ecb5710f46");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "45e26f25-6570-4a0d-8f59-02269fb88a3d");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "40de3175-597b-4417-b54c-38d8ef3cdb0d");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "90350328-73a1-4565-8914-e0c21fbf138f");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "e76629e2-0782-42b9-a9db-570f32c577da");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "ffc53b43-03d7-4d1e-8765-8d1c3ede0e72");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "90cac73b-c8ac-4145-93b0-e2e7c2f78cfc");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "9590789c-61ec-4487-9573-d695100582c0");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "cbfb4697-56ce-41f5-be7c-d06824288fd0");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "2283d1f0-f7dc-41f6-8749-058de5baa801");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "1173baf0-053a-410f-bfc1-aa09b932b7c6");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "fb57498b-71f6-4efc-8fd6-4ed71c97c6dc");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "8c366382-3c7d-48ce-9939-52c1ce8a2192");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "b5a542d8-a8a0-44e8-b602-c15ec25d051a");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f1ac2049-2e2c-4050-a882-c533b004c5bd");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "d95d70eb-ecce-4bf9-8b82-4e1e6dbac5f5");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "eb2cefb6-36f7-4142-8cac-3dbc3dffdedc");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "aac56720-33cb-48ea-8de8-54141f5e4b12");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "e7e1f6d3-c85e-4cd2-9c2e-d420e936ff70");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "6bf2d473-6303-495f-a6a5-a25bcce99f44");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "cff7de22-e10d-45ef-8f49-94ce63846376");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "eff7c879-74e8-49ae-bc8c-f95ade13d9c4");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c63674ac-e095-4979-ad5f-da4e4df21a37");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "ecf1311d-ce12-4ef3-adcb-53e259b8c933");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "9028b087-2498-45e8-8b3c-a5c928091201");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "b79cd2fb-7e82-4f85-9dbf-bb804f88f979");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "4794278f-64cc-4702-923a-76f2c9740bb2");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "bb37d37c-f765-4981-b268-1e4bb135114e");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "bb92502b-a183-4e0c-8253-7556a03d4632");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "754031eb-89bb-4e45-88ec-5a28d025f2c2");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "d434c608-f23c-482a-9222-1a13797b6fbe");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "55b3a1a3-f876-4282-a146-421175ea8263");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c822fd60-3f2e-487e-9b67-b6f1a5f559fe");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f2a9707c-5822-4050-85fe-1402a154c273");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "3fb6340e-c6db-4826-8831-65b31cb36062");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "064bccf5-c430-49e6-a14b-749854c43143");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c913f93a-66ff-4670-917c-e2940589498b");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "2b3ad962-7a10-4d0e-b191-d21793a71a86");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "80c9bd3d-6531-4fe1-9734-152924b8ffd3");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "86b1f855-cc7e-4bfc-9efe-cfe980cd998d");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "4edb1f4d-cbf3-43f7-a491-a705cb028c62");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "8cfe7fca-5022-4764-b0b5-4643ab07d312");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "7208b27d-bf8f-43d4-a39d-1aa7a4d15e36");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "4f69def4-bb90-49e0-9022-29daa8be126c");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "a31f7b53-536c-484c-b571-5904e6f5f3bd");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "463c5a16-7a34-4225-994c-6fb49475d183");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "92c132d9-665c-4972-9539-0ab16c2b3d37");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "77ceeec7-2479-482a-ad72-92644c7d49a9");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "812d7502-5bad-41e9-a061-722362c8dcde");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "2e6b664b-5fe4-4233-bcab-cf400e9d434f");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f62bcc15-cc9f-4c80-9c84-ff90580b65cd");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "57a895d2-5874-4350-a86c-0f7c38c0aad4");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "919c4edd-55c5-47ed-b202-cafdf70139ce");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "32fa0577-a0f9-4a91-82ff-91ccf68a022e");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "a93899e4-4060-4542-9a2c-92c4a4ed92cf");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "998f9358-d61f-4ea2-8a1b-0668500b420a");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "61b153a1-9c17-4326-a3ae-2b0ff108f29d");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "3f6c1b1a-86c5-4cc6-9c55-307d5d9c2587");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "244dc695-38c7-404d-9040-756ffbf81cb6");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "10462570-7926-4934-a78a-e72efa1f8776");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f87b98ff-aef1-4b6f-b4ed-26868cb635f6");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c2945c4c-49ad-48b5-9a8a-5d4fc98dce1e");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "457aa9a6-552d-4bed-a2fb-7e1253a6ca88");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "77f41eb4-1b13-4732-98f8-144cc1c7e779");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "330adf5b-81a8-4431-8f93-fb92049e4253");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "2b71b29d-f325-44dc-ab6f-0324d0570f19");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "a33561eb-dd30-4a9f-a90a-9ca09fa4fb81");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "39b4226b-fdb9-4f91-93fb-0801cad9ea7c");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "417757c3-e10e-442f-8d41-5410f629135a");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "dad79116-503a-4138-bd9b-677b902599d3");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f5528cd2-9017-42e1-9f91-0aa3db9b94c6");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "31487692-db17-45ca-b7d0-b6c372456291");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "41d89a60-5261-4698-b8ce-56dae94ce757");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "0fb03052-3df7-40b6-a816-715d0e50e787");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "739faeb0-d119-4a03-97ab-6eae113927db");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "855ea0ab-240a-47ef-ba67-fb558488be2e");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "ffde5e29-0da7-44e7-a6d1-c641d1677e82");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "fcc8ec1c-b50e-482f-b499-d84eb913246d");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c006817c-c1a4-4a75-a4a1-ab7d1f400c14");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "039b073b-f18a-46df-95be-53f33acd83ae");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "027fbee3-2532-45b6-8290-970b6142b979");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "efad5122-3ca3-48d3-9bcb-c4ff5826fe93");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "2f8d0807-566d-4897-8059-fe698d1ee38d");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "cb949bb0-dc09-4ab9-b82a-0c023f389821");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "8870abd8-4242-4c79-acb6-95e2a5e9922b");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "bd392b61-111e-4fdb-b896-495d9ae8108a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "70cd9749-f9a7-4c8b-91b0-36dda164c41c");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f9febc95-ffa2-41b8-8d2f-b115d3729513");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c17b087e-49b0-4231-8b7b-e500b243da6d");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "042b1f04-5089-40d5-bd60-8ec67f09db75");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "2f7574d0-3ca3-4cca-8633-59929b69b4cd");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "5bdc33fd-d4ed-4834-bf6f-bb16c2fb6db2");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "b1baaf68-cd98-448f-a58a-c730f649d5cd");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f30e9971-65bd-494f-80f8-de33b2c1908a");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "9657e686-1b35-4042-bd1d-c555e100c025");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "782c937a-5d9b-4e3f-9401-410067601ed2");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "936efedf-77e3-4c3d-814d-25f383a500cb");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "25ba2eae-55c3-4637-aabd-afa52729d379");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "de9b64fe-a9ee-4ee7-8520-841e1c30c44e");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "bfbebdef-ab6a-472f-9d09-a45131e9c13b");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "94034607-bbb3-4a17-8828-4d9585f70adb");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "334b090a-53d0-402b-8647-284728312ce2");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "6e8f5618-bb3b-4abf-a4a9-bec0834ad3c4");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "44a89b66-ca54-4ce2-8af7-ca8b8415cc4e");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "7ca51892-7d57-44ba-9cde-ce5d64dbd38c");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "23ac05a3-7f4e-475b-9190-cd2d4c18be52");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "bdbbaa59-ce9b-4e44-8900-83b0d92600fa");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c37e818b-d60e-4265-ae56-660a291aba0e");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "d80ecac0-376a-4262-9533-d9541563167b");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "8aced7b4-5155-4855-b9c3-f9cd4a90623a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "35811658-cae2-4ece-90ea-efc187fe2563");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "28ae50e8-2bf4-4f47-9f98-4ef59720ffbb");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "7283c063-0697-4399-a899-58bd7e6c71b9");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "88c375a1-4706-4190-a366-a771ac29911e");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "f7e1991f-c7a1-4c12-90c2-d12ce598902b");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "9252645f-8950-4c71-9f9c-c8a08f3967c9");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "0ee1e019-1fc5-4e47-8330-b2be23f786d8");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "8900c252-3135-409b-9916-1ee8b8ec10c6");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "9f6491cd-2621-46fd-a9e2-f32edb3759e2");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "19f9399f-898d-4cc1-9fe7-d8fe828e2325");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "3e1d0726-74e3-4db6-b0f9-356e97c4e0d6");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "8b4c9b80-866a-4e90-ae6a-9bd5614a386e");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "3767e5eb-4f58-487d-9e78-339be7eada09");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "6ac798fc-12c1-49dd-80a9-7efeae2a35da");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "a4f2be66-25c3-41fb-818f-1f4233c76894");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "00b7b4de-6bc2-40ba-a423-1ab383f6588d");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "76f0ea29-badf-4243-bda9-ba00b71fd9f4");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "a266e3eb-c642-4b6c-bab8-74fa5cda910f");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "54d2c856-95c2-4db3-9d4d-00e897e6ea6d");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "c8c88659-c932-40dc-9211-b465e2f26727");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "e94a902e-60f9-4eb9-973a-de2c411d81eb");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_4_10.coverage", "5999386d-ea40-455a-98dc-d8cf6f0881ee");
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
