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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "74196ebc-c8ca-4088-93ba-6386b574b5a9");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "0bca1c4f-3c0e-4b05-9933-ec6f5920c74c");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7434c248-f758-4110-bd7f-265b76d892e7");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "668e5f1b-39e8-45b3-b7d3-05ec207e24d6");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "2a256ffe-4e19-411c-a1ed-6ba69351c2dd");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "405d7040-a027-4cde-b8a8-4f301553ca33");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "40c9e8b8-339a-488d-bcd8-f25f65b2085d");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "2b623228-8c31-4e54-aaf7-658113283c11");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "ab2d50f5-9ad8-48b8-804d-239bdd3323ef");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "1ba6fd41-c617-49f8-aea5-faada07429da");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "0b0f08b6-9c43-41fb-9c02-6737aa1e6973");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "97a86e8a-27fc-4789-9a82-3975534378b4");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "d1860066-c33e-4374-9d0a-ac43edd690f5");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "9b4b890e-bbd2-48c4-b9ce-7954ed9995fc");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "acef25c2-e212-44a7-8879-1a1498bf2dc5");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "0937346d-ef38-4068-9e62-db4afa2cd072");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "4635d047-4268-4b9f-8ddd-4cdbf24343b9");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "c2332f47-15f5-48ff-9e9b-ae89847375d1");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "bfc86e06-6c2f-481e-a2d3-5e075b8125a5");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "801a6eb0-1802-430a-b235-4bc704a354f5");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "b29a1910-3750-4974-91d4-5e163fa7a6f2");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "67d79637-3b84-4a7a-8462-4a074b936d01");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "ef496ee6-93bc-4374-a8cb-7d012e7a50ca");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7428d6ca-24ef-4813-851a-a120bb1b6c13");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "abf0055d-bd4e-4e6f-a126-e5755986b896");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "8c39b4e6-7ad2-4595-be8f-0ef0eaef6e41");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "18d5697d-8a9e-4afa-9b1b-4092b0a58feb");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "3f9cb529-4424-4388-87d3-f91a9e2d63bf");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "94f93af5-ee6f-49f3-b8ab-f7832e1f2e08");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "4a4dc1f8-69b9-46dd-b316-5ec1311c5eff");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "b25ac643-78fe-47de-87ad-b83699a9cdb3");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "cc13f3ce-b94c-4e5a-be3a-44436dd6d672");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "88eb55de-a888-42ba-ae6d-fe08eab7b19b");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "23182392-f8eb-46ec-8e36-ed3f00552f2d");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "5a2ed6e4-a854-426c-8e5c-96df894ff73b");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "47c441d0-1f3a-4ab4-b49a-6586a1696143");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "cefedcc0-623d-4683-b63e-f2425d6e18d8");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "936fddd0-8c2e-4651-aa74-a6f1ce1850aa");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "d4ca4bb7-e533-4b05-be2a-075bef85a3b6");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "42b9228f-65cc-4669-afe4-6c66cc36b217");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "18736981-b29c-4c14-8395-822f0960d06c");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "ba3d7a0b-9eec-4585-8e6a-6b5f229ef510");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "80062648-e3d5-44ed-a32d-1367eee23743");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7549f152-2a2e-41c5-bcda-95bf1595cadd");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "c0668a01-f199-4de4-878c-c470160a90c3");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "c26fa9ab-f56e-4360-8206-e7daaa83f5bb");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "b1444197-9062-472c-9a74-7d2a9bb74627");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "f96d8d18-90a8-4200-87e0-8308821ce993");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "a08356c3-99ab-4728-9c89-a5b467cca6f9");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "d118dc1a-c33e-403b-a315-4424eb476dd5");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "a5ef85ea-aadf-4009-8ca0-1ab7ce6836b0");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "e535c9f6-e06b-49f1-be56-80b109e7b727");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "1fd7859d-13eb-4de1-b064-5badf15f650f");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "af3621c9-1e08-4ade-9f02-41b1f0e0bd9e");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "252f8a43-6940-49de-b37b-38e9115621b6");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "af3b53ec-27a4-4b20-a3fc-41bed86d4e89");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "a1354682-adeb-4ea1-8c43-3ea1c9d6357b");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "c042db50-08c7-48cd-942d-33ff753f6c18");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "37bada05-4cb7-4687-8c8a-43c2e20c5ef1");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7e35567c-08a8-47a9-84ff-ba836b6f7d46");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "44e05ff2-7f46-464a-a81f-eb5eb5f56047");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "bcd51734-bfec-4f82-99c9-4ddfeb640155");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "18988840-633e-44c7-bbb6-a2fe3c4a53a6");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "4576a525-f78a-4a3c-9a19-ff13cdd581c7");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "90567f08-0dd6-48ee-88e8-fa9cf00e14e0");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "82a0d759-2e45-457e-a1dc-c07220d9bc3c");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "89783c20-05f9-4354-a588-a715c701779b");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "9ba363ec-0fcb-4b79-aef3-9a383ba9d983");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "06df9278-5e34-4f1b-a925-0e92d6341041");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "cf4ebb7b-2a64-4faa-8577-0fc655db615c");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "925f306b-6d38-421b-8784-460cb6dfda4a");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "0d585b85-d6bc-492f-b0b4-091097a1c768");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "179b045b-d050-459d-b213-ca4fe605de4c");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "34d5adde-40c8-4c04-beff-43440ccc074e");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "d5c37863-0ea1-4c8d-a8af-d3e3781b23c1");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "0d5118f0-3cfb-4c81-9e5c-b1e802c72f09");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7c289ff9-2585-42ab-8872-105fb6b008cd");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "1ec7706d-07f4-4e82-b58f-bcd5de4daa8e");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "24383b97-f98d-478b-bfda-8817dc5abe4d");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "355e82d9-2c4d-4505-842c-6529cec5ec2c");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "fc680e71-c174-434b-95da-1552406b8fc9");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "36343548-6d12-47b8-93e9-af269949fbd2");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "ae9e768b-8bda-49d0-970b-75202d89f6b6");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "08c79983-44a2-401c-a52b-bec2c8090a35");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "df8b374c-8e48-4b89-90c8-d392dc3664f8");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "c65868f5-4f1d-4e3d-ba6a-856dac6017bf");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "3891c5d2-8def-4d2c-bd71-7e94cecb9284");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "2ebf16f5-6da3-4508-a279-efea3f45ec0d");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "ebcd47fb-e705-47b4-9760-2f3d5fc7680d");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "b4805d69-b596-474a-943f-8b2f8bf51b03");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "040090d5-8f99-4a4e-a769-57deb89f9c89");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "ae8e76c5-9e03-4ae5-955b-c171e3e78ade");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "13c075ce-3dd3-41d9-b1d9-1381ec00ef59");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "c100508d-c8f1-4015-95da-b33631ff200c");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "959e0518-0e1b-491e-b0bc-cd83da0c09b4");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "a464d927-df64-4163-a890-5d10c67bb447");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "099b5406-3633-45fa-918d-e54454fbfe70");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "44f06c6d-1956-4ea3-bfd5-c7bed8e1523a");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "858d50a1-5482-417e-b9bd-b0fb66e9de3a");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "29feaf72-0412-46fb-972d-e87c3ec6c8c5");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "21b2b26f-b5fc-48a7-932a-65cfffba2fab");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "6b4ebc7b-aaf4-438e-bc9d-2a188e4db9a9");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "29dcf5c5-220c-4f77-b60e-434089524e36");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "16e2e20b-a50f-452d-8b76-8a5fd7f25512");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "6f578542-255c-439a-ac77-ec4b2737b3e2");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "2be6c149-1ce1-44c9-8cbb-cdbfca4f5ebb");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "21e70fa5-0b00-4bf3-9792-65ac578e9245");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "e2977204-ca90-4b1a-9976-9a66ad6e09a3");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "d96dc822-dfb8-46c7-a17b-200788fe178b");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "504fc742-6a09-47ce-941e-baa93d59d09a");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "40194043-8205-4042-8b48-dfed61d253e0");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "292df83b-5c6a-4d34-82e9-5753530c6991");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "238b1a7a-bf83-4803-b6ae-bc0cd768804c");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "781d94e1-39ce-452c-8e36-b9e05ca9fa94");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "1cb100f1-4e7b-40e7-abe2-472fe915de63");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "c86803df-50ef-42bc-9848-cd877d805fa7");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "02ada5e5-247a-4749-b3cd-840c17861993");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "e99b7222-1c5f-4c38-bb8f-ae774d391412");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "238bcb31-7367-49f9-a57f-514aee84a1a9");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7e229417-07f6-4bd5-8bca-6d756836da38");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "92572f28-01c2-45f4-823d-1c83e46ef215");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "fbb4b0e9-e8b7-4a55-bcba-ea9b9da1a045");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "f50ef917-bb0b-4882-b62d-e8c29d53fc7e");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7eea26ef-7699-4911-b2a3-cf6c44494f4c");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "79b12b51-0b82-4ee4-ae00-016ac52a0daa");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "10bf06c8-90bb-46d8-b8f4-4d038f98cb01");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "52db71a7-ca42-460a-a620-70e49df5028f");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "3e1ac913-9f52-4eec-9acb-93eb2665345c");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "3081ecc2-25d7-4275-81ec-61c4113d223f");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "dbe8e316-dd55-4159-bc18-a6ea4fffae0f");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "ec61083e-b828-4b40-b77f-7f9cb820f5f1");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "2a25fc0b-52de-48bf-8160-8ffab6d60660");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "a435be7a-8717-4173-9a45-34527c562543");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "d14090b6-5e9f-4584-9d6f-7c69d55722af");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "09a1919e-6e63-4aad-a55b-f438829ac69f");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "83af4086-6f5b-42a9-a04c-e17ecf3805f7");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "74377e50-dbf5-4ec4-a0fc-b9f00353fcc4");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "86bfc60d-d7f0-4d78-b3b4-7d700d23d871");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "34f2bf55-28f0-4d2d-b3cc-b9ca0503c74b");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "8c27e6ea-df06-489b-b636-ea04ab12fe5b");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "4343d905-2ce1-4820-97c8-24006ae41248");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "b540b78f-c1a4-4446-9f9c-2eb28a1591e5");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "6374eff5-7d49-4a72-add1-38f07f16189c");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "43ef8664-8f0c-42cf-9882-0f61c275f746");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "7e7c0368-e46b-4293-8354-6f1c490998b9");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "0e55cd22-6c65-4310-810a-883e773b9f70");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "3e68c336-3641-4845-82ce-07c6230ccf0c");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "36afbf3b-563f-4316-8268-0fb81ebc5a94");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "a33323d8-b56d-4a52-9a4a-0eabde21cd58");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "eabbb3f1-3b6c-43a9-ac81-5a474dfa2be5");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "dc72173a-9f9f-46ca-89b0-d12d5deaeed7");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "370455ad-1d1f-4609-859d-3e44797cddc2");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_1_10.coverage", "8c7043e6-6b34-4263-a0d4-31a313e725b5");
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
