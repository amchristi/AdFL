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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "306fb1fb-6551-4c50-965b-fadedb16a13c");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "4b887031-fcb2-4545-be0e-027a997d4398");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0d7f2f81-88c6-41c2-a5fa-cb218d7b4793");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "98df069d-5fb0-4055-8ef0-82e2b700b492");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "c51fc095-441b-4778-a7b1-dc9840213f65");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "2ffc5d27-3764-4084-90c7-eca06aa3ba3d");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ae787bf7-f5b7-4bc1-9432-9f0be61db338");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0363adc1-a0aa-445e-812b-5e6eb416510a");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "7f61a7e1-dd36-4dd8-91d8-e0aaeffc078c");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "963456ee-4049-4e78-8246-68b00e84939a");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "58cf7460-dfdd-4dcd-ae27-ac4888a62192");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a92a4d29-b691-40ae-a947-6d75c7d887a5");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "5ba48ba1-42c1-45fa-a7ae-4b2ccc31bf11");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "b8c8876d-c5a2-4af8-a1dc-bb3e95d22744");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "19da83eb-508c-4cf3-b078-fbd8d15bd0d9");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "5b542d60-2f9e-4ba3-9575-1e89509a364c");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "e66089a1-643c-4e1d-94aa-8495e7edae6f");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "67cd4519-249b-456b-9093-bad400709b91");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "4babee20-9b86-47b0-8b76-e150fd093037");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ef84d20d-b871-4ab5-86f0-74761e47acea");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "15ecba5a-6840-4a95-a175-1a64fbca98fd");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "79da30d8-e469-476e-bc38-8d39a3fc933c");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "76353802-2e93-412f-8764-d6fd9b01e670");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0a9b4649-227e-4953-9406-e47402a1acde");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "bebcd12f-6fa8-46bd-8f7e-05320a9a7d3c");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "344ad605-b61f-431a-9b96-42524a47c12c");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d7b14598-eeb8-42f7-8e37-89f8db6ed0b3");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "425fe7e5-5f44-4fe5-8f57-58ca6456a4dc");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "5e4e1c82-65c7-4429-8062-f161fa4026b3");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "9be3db04-f18d-4f52-ab75-87d582285f11");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a9569301-7602-4806-8c6f-22be06727ae3");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "73b8e037-962a-44d5-adb5-aa644e700c03");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "e1d4784f-9c1d-4a7d-a442-3343188717bf");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "78762ea5-8d76-48cb-ba02-2a3b908031f9");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0dffce88-f13c-4d36-8046-a8d9e92b1ffd");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "237631b3-1e6f-4483-b097-e5d4560b8449");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "945c9f55-943d-4fc0-b069-49688e90a28f");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "61f84b2f-3783-4fbf-8a55-73e0d80f5d43");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "3db2b3ac-3972-4a10-bb21-a9331d106727");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "f553a621-fb84-4b4f-9178-c6412e865ba3");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d89e7c1f-1808-4426-bea1-aa380285668b");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "7a0aec90-03d9-45ec-806a-3ddb4adeb30c");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "b8064f5c-378a-45dd-ab2f-ea24bd3c4354");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0170d973-7930-4134-aeb1-4c0c8309cce7");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a760355e-74c9-456c-b4a0-c07ed96d8799");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "12560561-2504-49aa-a148-df1b277670f6");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "c2cbefaf-5ff7-4c7c-a41f-ae607b39f62f");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "536c9ecc-666f-4b79-ab59-eb31f781aa2b");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a512965b-4b52-47de-9c8d-a62b4530105b");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "92b7f212-2a1d-48d5-9c02-cd033bd442dc");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "83cdd930-4b63-4ae2-ac8b-3666c54c6487");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "84a36791-4488-4bdf-b545-9b6d356fd5bc");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "fd047328-a868-4054-9ed2-d18985aeeca4");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "2268c4e9-7164-400b-839f-08c1efad6cc0");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "11a65166-3116-4aac-a962-4774b12cca04");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "17bf26b3-be5e-42e4-917b-9cedce2a1abf");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "e5e852dc-3140-4e9d-a523-877761bb2b3a");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "65cacd29-d5d5-48ac-a425-769c08fcbed8");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a772ed9a-9b6f-4c76-9f2c-d8ec6a2e6cd9");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "78481241-4f8e-491a-9d3b-8f8bd875b6a4");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "26ca0aa3-d076-4103-883c-e977d31a3a0b");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "3f5c0d96-3019-4e29-8b36-8a52a5a6687a");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "de400829-7624-45f3-a874-0039a4733fd9");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "4e667983-fa65-4bdc-9598-a3694c6f510e");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "db5f3f84-3b27-4615-b639-20394e9e5eed");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "f0e61d49-158d-42b6-b3ff-eceec7baf995");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "33500374-2601-497c-8154-c5d63e726db7");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a41e170a-5558-49dc-b6b9-03c34c8f97e7");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a302b71d-332b-4ad2-a86d-ce840d936af2");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "49efbd83-964f-4c93-97cc-8cef5537bc19");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d81017f4-c86d-41e1-aac9-c2a380dff075");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "efa79f23-8e1a-4d35-bbe8-437464c94035");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0da22e09-45a2-470c-8a11-e791f74b09f8");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "20b1a9a5-ebb7-4eaa-9d07-47bc2338d697");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a5ef7055-cccb-418b-9891-c0a5d6f00cf0");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "34659221-3ed7-4cdf-b936-28f2f514614c");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d3c1c519-bafc-4f16-bfdd-ce0c3d7d758d");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d752b7c4-f702-4f1f-8ea6-5ac27bc99703");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "993c77c1-b87c-4235-b0b6-9da8c571a1e4");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "9ac29ad7-cec7-4390-9ce2-c55bcb187a01");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ba60dbed-364a-4b3d-bae3-f2194bc7486c");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ca7829ee-f87d-4595-ba33-5341e41f668b");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "1edbf72b-9fb5-4a91-a491-f10c4f84f848");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ab407b95-dd1e-4c8f-ab0c-4bf9292ed5c3");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "910a56c1-daba-466d-aaf3-f254fa76da9d");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "5293df52-fdf3-45ce-a73d-297ae3c44f77");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "533ce558-fbb3-4259-a4d7-33abbde2e146");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "abe9beb4-9a7a-457d-a375-19283cc4d4e0");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "4f3c1b99-a601-432b-bf81-91cdeba8ce64");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "60da9701-6259-4f3e-a142-70d56f6007e9");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "89b3485e-114a-4727-a664-234bc01643da");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "79fea92a-2b46-482e-ba5c-bc88180fedf1");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "c548bc86-ceaa-4da9-ae39-15722c87ed0d");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "e363cb64-d651-4f8f-a849-d91526d48162");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "91c7696d-69c6-46fb-8b25-0a402794d774");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "181b1da7-64c0-4773-a06c-18394b26cb29");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "96cf5699-0474-42c6-bacf-924ff5cbb678");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ea654081-ff3b-4375-87b2-2108bd5aada5");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "3ea107a6-a73d-4a11-b29e-cf5d35397570");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "cef77029-3291-4c27-9479-eb2be4da5e0b");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "7a5ce15a-6665-4002-9edf-141a9346cbba");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "c550a7c3-2e3d-4214-87c6-c5f3a1d273b5");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "43627afe-336e-4fe9-aac2-f3df5993d190");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "dff6fa1e-9b9d-41a0-9de0-5999f40ad8f2");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "5ee391af-2f9a-49d9-969a-5d0de56f6ae5");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0a5a0cc5-5ab3-47c2-a32a-0ecc699aa74b");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "6917e8c2-5fab-45ad-a61e-66fddfb16a76");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "13188b4e-92ec-4288-a2c2-83065b06d5e7");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "80edc26c-99ca-4d1a-94fd-7aaf22205504");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "53434131-3126-4a34-85f6-7e33d83380d2");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "854023da-ee77-4d3a-91db-c914e5142afc");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "431d8072-427e-477b-b476-d485cf647567");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "40c8dcac-8f1d-49a5-abc1-59a3398410ec");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "99efce61-f2b4-4599-8be1-9137cb213362");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "adf44924-eab4-4774-988d-55708fd8b0bb");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "027c1402-bd08-436e-89c1-7b6eff802cd8");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "a767d73b-4f71-43a8-ac07-1e2dbaa799b4");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "7ea1bae1-dc76-4b5f-aa82-8672cfe1c930");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "08369acd-9bb5-476e-9b9c-6df02f6d33b6");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ae60f7ff-f004-4a0e-8fa5-66d255b663f5");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "cb1b65f5-fb41-4167-bb11-ae080134da98");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "7b577214-04f4-48f5-a00d-2f2d25a448e7");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0c5d0a7f-b838-40c0-84a0-2118e1a62372");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d9d8478e-365b-49c7-a60c-c2590080a4b2");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ca346bb5-e9ff-48fc-8ddb-8a6818b1ea1a");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d52373bb-a410-4f38-a373-39bd67525d3a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "200d02a3-b226-4388-b37c-fe6f80b82f76");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "71d824ec-770c-434d-a43e-db3e26762a53");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "05d7fdc4-2386-430b-9e01-db8b93528353");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "903901de-3ddc-4117-a748-09d7952439e9");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "26e3efc9-4105-4529-ad89-e3633f17624a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "c131038d-ae69-4981-ae68-02fae411aaf9");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ceedd10f-414a-4468-befc-f3e6520e999b");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "78deb095-3f74-457d-8f75-766011e47f51");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "73ce3d7e-72ef-4993-902b-cd792fcb6de0");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "90e30354-7e32-4466-b711-16b438a5367e");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "ddc43695-e477-4cf9-9350-71bd06dbe3a6");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "b36df50e-def9-4ee7-b67f-3505e41d4639");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "92887e3f-ba07-4a4b-bd74-c09cd17b02c8");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "cddb197a-eab0-4791-b9a1-9e87add56534");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "cbcba29b-28eb-4a49-8561-88878f7bb441");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "78f2a447-4717-474c-b0a8-efd7b665eea9");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "e853eced-3650-4230-abcc-84be9fdf5891");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "4eec0e83-6415-4337-9e54-322adbd088d0");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "d555dfbb-7e9c-487f-8d12-187471fb31d4");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "9aca4173-9004-4e9c-b921-405fcb5ba87b");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "325badc4-3f18-49cb-b926-ff067377bd96");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "65bf4545-04cf-4f5f-9764-96deb246f366");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "f5ca87aa-772e-4ec5-b7ef-632f8a973343");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "e95155a3-ed5e-46d0-89fc-c01963e1d590");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "41aa0828-56b2-4918-a0c4-21c0007a7338");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "0cc12f77-b93a-428d-966f-838cb0cddb37");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_2_10.coverage", "45f0e3a0-5aaa-4b96-884c-ada8bddfc841");
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
