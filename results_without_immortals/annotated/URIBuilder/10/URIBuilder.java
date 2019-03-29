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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "89e73b60-0008-4117-9f8d-652b03d98c10");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5f27bff7-2df8-4ad3-a494-333f8d8485fc");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "eebdc29e-0972-4769-9f48-06308b312c38");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "4b905e1c-44e9-4c43-a50c-abac2d0126d0");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "bb917aac-b8ae-4849-8cab-1ae7ba2b757b");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "42d3db17-e273-4417-bac7-01809688f307");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "cf41fc4d-e25b-40e4-bba1-0f05266262e8");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "22368651-77f9-4031-be01-7dd0376f5052");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "9355e2c2-42a4-4d2d-838b-135e510f671b");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f39237dc-6e85-4a48-8773-339bc0f845a3");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "993c8a8f-6273-407e-b134-3f7b0a0ee93f");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e1c205cf-a1b6-479e-9db0-1943e92c0b45");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "2fa9250d-83cb-4ab0-87a1-24399816a3ea");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e801bc1f-6189-4d83-a726-5f5a71879151");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "56bef50e-8f20-474c-80dc-d7c1d194d429");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "13c6fdb7-3929-4715-b810-0136b046e2f5");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "700e0c73-3a7b-4670-acd6-84c3765e656c");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "93fcbbc1-8520-4f0a-b1af-5515a4d7181a");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "38b47bb8-91d6-4283-8023-45cc7f9118e4");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "8635d59a-ef85-4ceb-8ce2-44075d8a4ff4");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "2e2f9564-ff62-4412-8a85-205a18f0e8e5");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "8d5fbc8b-70b4-4413-a290-9d50c6d2b4d4");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "2a5dfa9f-110d-49e7-aa59-f48648da6045");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ca81f40e-3397-40e9-b261-ee3af4aca2db");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "7535b31d-ceff-40ef-b499-3fb22aacf85a");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "69642da0-cada-4015-bd55-8751fc042ef1");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "951c6439-7c2e-4857-8e8a-95aaf1612836");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "23fea316-d79e-44cb-9f64-ef7fb8e62817");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "034b6f91-0b3a-4dab-a99a-b6c00c91071d");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e9c1faa1-0a8e-4d58-90cb-3eff31b23d13");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "bec8bcbf-baff-4619-b0c2-75c1ed01d34c");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ef1b576d-23e3-469a-a76a-8d1451d17006");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "81e52d1a-2972-490c-85c2-1aaa703e3dc2");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "6bfcfcc7-9fae-444f-9f1d-612a7d7d12a3");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "dffc33b2-105b-4c1d-b6e7-deaaa88a1cd3");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "c8b84a40-eccb-4086-be38-390529d9a0d7");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "fc58c34b-0670-45a5-a3a2-d2b798c05813");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "eb49b841-9f03-47d5-a8fd-9713defdc799");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ca0609cd-5fc9-4254-8423-6fc97382c6b3");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f5b28ee9-2c35-45a7-b3e6-70095c21c301");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "66b2ba88-2674-45b2-9ff2-9044b813d56d");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "56db612f-40cc-48a4-bad5-084df87b6d82");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "a0522936-fe92-4737-873f-0bd09a7248ed");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "a87fa72d-9689-4c77-b9b0-c179452337d2");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "b3ea076a-f240-45a9-8181-31480abeebb2");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "58501fff-421e-400b-aa19-d5556cc9fbe3");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "8681cee7-76bb-4bfe-993c-5f453929d076");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ccd7c0dd-3ee6-4eab-ad82-25a05b53c254");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "854183e8-e62c-4ab6-97d0-bd88667f3464");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5a46f40f-9e68-4104-a61e-0b29e3f8096c");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "41abc0e8-d12c-4b68-8050-bc4d70806d8e");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "2044b377-ad24-4c7b-9607-04c507198d8b");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e3f46f97-01f3-4f35-8ba7-15af09e48207");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "0b7225d8-0678-4ab3-a399-7b133ef32088");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "05d22a5a-bbef-4b32-afb3-ebd65cf475bc");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "63bb9630-8917-47fd-b935-3f7b3a752496");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "2d042ef0-9bef-4e71-aefc-4e6d149f989a");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e595f0de-5736-4fbd-9746-de40433adafa");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "09113cf2-1282-41df-b33d-1e79ff62d198");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5b984ec1-2d46-4489-b1d5-091f1d0c655d");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5baf1db7-9834-4df3-897c-fd076ee13f5f");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "b7d96c50-76e2-45ee-a854-3fd2b53591d1");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "8af4b712-de15-4501-abaf-170e76c06352");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "86f506e9-702e-4a8d-afcf-f96d1eeea51b");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "67cc2699-ff3c-4f95-a338-8b08486a8c2d");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e89560b1-4eb3-49ae-9287-678635b7584a");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "8e1110e3-2d0f-4e71-ab70-9384dcd11207");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "44e88fdc-c868-4ae9-9ac0-711103bd0e42");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "628cc2b2-0f89-46f6-b58e-c0449741d989");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f327618b-d5f0-47b0-bf78-d82babfeaa01");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "c1003481-ebff-4ab0-b879-551f87e52810");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ed662519-792e-47c5-81c7-58c489519b73");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e5514747-ae39-4428-981e-830292c571f2");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f0ec0f98-f7b5-496d-89df-e355d27940e5");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "6f6624aa-9b6e-4f57-89b9-cdca2468ea29");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "484f25b3-6a56-4094-a7fd-c7bc1035ffdc");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "d622264d-24b3-485a-90a5-56ca078fcdd3");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "75ff8b99-a32d-417f-ab58-008783839d02");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "0a3897f6-5780-40fe-9fdf-a0803da5e24e");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ca9a07fd-9064-4b69-a5f3-024a769d17b3");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f23fe7c9-5720-4e0e-afb5-7e41e425b814");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5a0224dd-927a-4bca-bbb3-bd4e50da801f");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "1a7277ef-e6ec-4bed-8dfe-02d13eca807e");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ecb7b72b-d0bd-490a-b5a6-40e89103a191");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "b40d2620-3894-4d50-a2d1-b5b57e107765");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e7c3317a-ba3e-4719-8410-879edbcd3692");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ac956a6c-d6d7-4c8f-8d43-1b12692f2c0a");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "912b48fd-b663-43be-9dfc-16687832ec60");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e5fe6d74-30b8-4486-9585-66ded1d9e855");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e577498d-815e-40b8-bb8e-6ecb6925efcf");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "80da5b48-e884-4f1e-85f8-538a66363c5a");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "33817726-a30a-4e2e-9166-e8ff26dd6ffa");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5ac3e7df-c89a-477e-ac46-74fe849f4a38");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "77a6c60b-e620-4bf2-a008-997017cb259e");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ae487b46-b841-4e3b-b52e-0dd81477f967");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f29c48f7-aefa-439c-8e30-a8a7aa3e7b95");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "a5dbca59-477d-4180-b284-698f5f663d43");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "d8bca097-f3d7-4ee3-8792-94e6e222dfd6");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "eb415488-ea6f-4958-9fa3-102bad4de091");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "0b0104a9-ef40-49af-a9ed-7ee1a5af9901");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "17b14a7c-8091-48cf-a179-b8d16bd37e51");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5337cdf7-18a5-4ead-8832-fc8dad28a928");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "c15c6e68-8c4b-47a8-a3a4-c4e10310d351");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "3b520213-a8fd-4746-890c-4ccdc5d4d05b");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "9f8e5e4e-d262-474d-a655-ea86e6e4b74b");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "6fc37087-8641-4baf-86db-8fdbeb7bb2e0");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "53553ec4-664a-4f19-856d-a7c70adb5160");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "797b94c5-e27b-42cd-a295-8ed82fde3887");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "d6950859-140b-4c58-ba87-25cb1742db0b");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "c16bd4f7-6613-4c4c-988b-358d01cd0080");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "dadbdee0-037e-4581-a543-181c1e134cff");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "da475083-b06e-4ced-810f-a94d9bad35c7");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "7a03bd0f-4b98-4673-af2f-18b1ab63e8e9");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ead42f97-c259-442f-873c-7319352af09f");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "e61603ff-2b5d-43fd-b155-ebd78ccdede2");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "c6aebf00-768d-4f57-b6c2-4d8ececdb568");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5a8d1fcf-cfeb-4218-a4fa-f7ff25a3de5a");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "95c2ca0f-08ef-485f-ab83-96c6bd2e557c");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "7502da5e-7176-414c-a714-0e45e6afeb11");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "31439863-45c0-4157-aaf3-1142e8f4ed24");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "39e15793-726f-4184-a791-6228261e6002");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "9c474096-2092-47b2-a274-af8b9f08753d");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ab4ac01d-5faa-4614-83e1-843789a3b1c5");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "c7c6a7c8-92e5-47d9-8cfb-8930922d5831");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "63aef488-6e54-4622-840b-d4e4b6f6d9bb");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f7eac4e9-1a48-42dc-bc08-79853e75caad");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5f647cf1-e653-4e21-8a61-dd23f09d9e26");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "99f73ee4-058d-4174-a4d1-02e5d3c534c0");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "7ac045d2-390d-4eda-a829-6f830852c02d");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "6608327b-f2b9-4ef6-87b4-2227b72bb7ce");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ebc07ebd-5153-4589-b73e-7288d9d2f79d");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "2d90d466-2360-40e6-a1d5-b6e12ea0d089");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "5ac3b3ca-3c7b-421e-b16f-1e218d1d59a6");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "144ea42c-d712-48b1-a8d8-6af9d9da7d74");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "118971fa-8369-465d-aa4d-198e756dc642");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "fdf50d5c-a659-4bca-b6ea-99608f9e8074");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "f41ad096-56b2-44a1-a8af-5fd5b9ccc729");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "81a20d6a-c95d-48b0-97a3-695caf1a5036");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "0ed0445d-fc1e-4a89-998b-083bfa8dfd14");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "515c2a1d-e9a7-4fa3-b649-11bc1b11e10f");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "8ece8aad-55b6-4d4d-bb4b-f4003f7d1c71");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "66fbd6eb-19ee-47b4-853d-be1094468da0");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "ceae21b5-5551-4d87-a844-62899970ed57");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "779555fc-9393-4eaf-9580-deee24f05e5a");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "853de48b-ac23-46d6-ac6d-af78393fde1c");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "9a27d5d3-e2a7-4b5f-aca8-76d65a8ca7dc");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "9a817854-817b-4074-a2c8-f114da9f1920");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "d0f2ff3f-8755-432d-91e9-f8e1216422ee");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "45516b97-a227-460f-84de-a33208062751");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "88545085-e1c9-461e-902a-43ccf3490ad3");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "356c7503-228b-4f2b-9931-d842586ac288");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "07065143-1e14-4df9-b673-c70935380b4d");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_10_10.coverage", "d5c74070-5cf1-4fd0-b759-0c770181267e");
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
