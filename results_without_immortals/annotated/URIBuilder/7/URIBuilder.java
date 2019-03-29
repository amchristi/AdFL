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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "3bc852c2-72fa-4c69-b0bf-9ac7b7300452");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "3892564c-09fe-449e-acda-1f882c753db4");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "dcaea802-ccac-47a9-a5ed-11bdcbad8c73");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b4858857-bd8f-4c8b-a8e5-5d8cfcede4d1");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "19b2f6e9-7211-4ab2-afd7-9ecb83b37124");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "16ce8984-287e-42fd-af6d-b0403d10a231");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b2e5739c-a6e0-4926-bd7d-8baeb5636d64");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b8a09086-bb69-4e99-b69e-0616af5646c8");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6a09f017-86f4-43b0-bf98-c94a5561df7e");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "eaa8fdfd-c842-4af9-8199-42c2d2ba0f4a");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "2d238e4e-16a2-4e81-9f6b-3450428866d8");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "54f5a397-5021-4194-a4e5-5afc22e89716");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "c6cdfd03-f293-4e7d-9bc0-6822a4963a8f");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a98e9c53-764b-4f49-bc53-f63eed353922");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "781ceb78-755c-4d29-998b-39500891d7fc");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "bae15063-61c6-4793-9cde-abc9e428475d");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "3fc81e64-97bd-468d-b79e-bb55f09cd2c3");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "52fc9e11-7ab9-40b5-a79e-b59b04dad99c");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "1915c052-430c-46f8-8e6a-225423dc4512");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "1bd1f795-b113-4b78-9e40-7eab7653c9b3");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "2db5a87d-9798-4984-891c-8907fcafa1ee");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "1cd93f4e-fca7-44f6-b90a-f10d53c94ab3");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "c21ea12c-0d82-45e7-8958-6bde7b4e16c9");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "d23c6461-d04a-4df4-a350-ed8b17a41c7f");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "8a59cc67-b433-4b05-942c-bf662fdab532");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "9a74d0c5-7faf-4594-b443-9335115bfe78");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "033143a6-d2f6-4af6-a3a2-9cfabe8840af");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b72fcd63-df4e-4261-b2a2-061301a0fc69");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "9ae50696-19bf-4723-953f-f88a46f49ac6");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b6f95b4f-2697-4614-b580-941bfc0ad9c4");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a2826768-d462-4f6b-bc28-1228b77018ff");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a03b1d5d-cbb3-4cbc-8274-12ceef94f646");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "1b4c4410-bd8d-4805-b1f5-25a01cf8d27d");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "290981fb-4d05-4164-a189-384da9db7e9b");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "2957d97c-9a99-4284-9f88-3989b7517756");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "490f3f0b-2036-4d0e-bb15-4507e6c7011e");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "fe736931-46b2-4527-a7a6-2d5844c2d05d");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "f95ead93-93fa-484d-830b-df33d9b687e1");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "d611b686-1a44-4ac5-9b8b-d50926e5cc60");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "f13adbf1-1f77-417a-bca0-dee3fb2cc20e");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "fe0a5dc9-fe7e-47a1-8d0e-74e68b32e3e5");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "4a4ff71e-e075-4b08-8c23-88363636f639");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "16d6ae33-475f-4eea-9411-df0c2d855510");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "3563aae9-62b8-4342-81e5-b33512cc6703");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "0f866b1e-0864-4920-939a-02829e9435f4");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "7a933b8b-60e3-4a44-a28d-ea08f7abcf81");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6d86f016-9022-4810-bcc0-3a7b0ef65b52");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "943c8bdc-83ed-40c8-8f97-d9d58733b9aa");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "7476ed21-ba6b-4925-ade4-d03da30e39e2");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b239753b-d6fd-40ea-a39b-fd9560e0315e");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "dfcb3951-8cab-475c-9be5-093ce661be40");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "3f460711-f2a6-41be-a146-7e9d48bd8908");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "9a1ae336-bb06-4238-a3ef-dd42c26b8474");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "bb1b06d2-f24f-41ab-a467-4db1e7a23b8a");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b98f5af1-9816-43b0-9761-be7ff0797131");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "9aaf268c-b3bc-4c9a-b417-26ef73cf5a3d");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "8ac4d35e-5140-4ffe-9e64-914c874afbdc");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "1f63bb80-2b86-43f2-819e-6d7306bad88c");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "acb9dbe4-019f-43b1-a035-8b12a7120ae0");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a41fcbcd-2682-42b4-a369-00f3b98b68f8");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "af4401b6-aec2-4316-adbc-733577634dae");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "13d5b40e-32bd-4ef6-8c33-794090de4a23");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "40e92fd4-0c95-4486-9676-02817905161c");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "06d984a6-9b73-422d-9582-6bd4c9476a25");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "3d270077-4177-42f3-a6eb-d92db19ea8e4");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6c3b20ac-e9ed-4c6f-8786-e0dd048a8eee");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "9061a447-d1c0-45f8-a304-bc668d55bf75");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6a6ffce6-fa7f-412b-9b8c-c9f6c63b198c");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "c8f9bb84-37fe-46c9-8f4a-3cc515a20684");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "be1d3fdc-163e-46aa-9c58-20fdb98881ea");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6288e507-3463-4807-bfe6-076910ce71f9");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6be2790e-8efd-4273-bcf6-8cfde32a0062");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6bcdeb82-67a6-407c-b36b-a320909d30cc");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b5bf0e9c-e68a-4029-9381-6d2f19d0e19b");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "8c13ecc7-4c38-4cec-a50a-10ad0a965653");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "e50130ca-97e1-4e1c-8ef7-ea2e44558c14");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "af104f5c-3d84-416d-aa54-abb93b959989");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "4aa1a017-3f37-4f9a-9098-74ab775c94ae");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "221e1701-620c-47dc-874c-e6b580b9067e");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "3cee06a2-8d91-4cab-ab99-3730d1100c10");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "4c0d9f38-cf4b-45e7-b930-4b707b1ba766");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "4bc825b8-c44c-40ee-abb3-3900f59b4ba7");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "ce926030-9f84-40b9-9668-cb4919749ab3");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "bc2b497d-6b1e-4bc2-b0e8-8090906571f7");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "5330374f-c5f6-4af8-9bb5-b2e16658b47a");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "67761034-e3d4-475b-9328-58ce41855d26");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "f29e38a1-0544-470e-a4eb-991f93e3a9f3");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "7d073ca9-90aa-41a9-8d34-67580a932633");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "ba54b3d6-70ed-450d-901a-3fab43995137");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "7d51db0a-8954-4283-a8ca-82e353ccfe5c");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "2be719d4-9e84-4461-9fbd-bb12216e9b2b");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "95678fb0-8706-4972-bd0b-0908e0b5bec4");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "63a72dd5-401b-4f10-9f59-26ec6bd7c64a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "03c12ccc-daea-41d0-8954-8266e873ba1f");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "5870d49b-5b7a-4c98-89f4-03720d79a462");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "ec71cc13-3f12-43c7-be6a-7eb0b7178c05");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "ee7d7b2e-0163-4460-8b43-aae9633c27ca");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "c3007560-ee7f-4d72-a026-2b84ec7a4137");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "b4ee6a00-8b8a-49ba-8720-44b1882f1298");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "5e10b7c1-647b-4667-b9dc-a831537d29fc");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a3f51b5e-4a9b-4e7d-8f3c-9554e022f032");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a7b51e77-d5dc-42e6-8bf7-876218f454e3");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "63485f3f-151c-425d-9f25-4834ff130042");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "cee2197f-a416-4d49-89b4-15ae4e0cca3c");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "0fe3a447-8213-4269-9d9e-e8a974741598");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "f0e457dc-940a-46a7-b31b-90fbc0ae4db3");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "88765349-406d-468a-879e-90b6ca5f81d6");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "671ea04b-3f32-4ef6-8b5a-457e8d879948");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "54f682a7-a676-4884-9c0d-426c73025c90");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "59b7d338-be5e-46da-bd75-8e8fd5e57295");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "2e7c059d-c3da-473c-87ae-0255be203a5b");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "c35b45ca-1712-4249-a98a-7afd12344e26");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "8764289a-da17-449a-aeb1-bd6c91138ac7");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "1e5348ae-7b52-46dd-9c74-6546fe201867");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "082cc250-5cb6-41fd-9d6a-3ed91e30ebfb");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "028982fe-bd6c-4399-a64f-d96d3be47012");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "ddc702df-3e76-4c7e-808a-7d4f215f5f2b");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "9c38abc1-e063-45cf-a538-79cf3b8b6a3c");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "7d0743c7-fa69-483a-a5d2-132251f08c6b");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "66786268-45c5-4820-b7e5-df78540964d9");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "6267d2f0-bccb-4e3f-b46a-f5cc7b21c607");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "cb96eee7-30fa-46de-ac19-677ab1abcf57");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "ebdfed1f-93ff-4322-b7f5-62c5bc78abc1");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "fe4ae6b3-9cf0-4900-bdfe-1329e86867a7");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a5dc9829-4cc6-4eeb-95bc-2c80620bd69b");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "bc1dfda6-f662-4000-9a28-f158a3402298");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "35433a1c-da91-4eb0-81ad-5202edafca25");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "7efa5217-4902-4ef9-a9a0-d1a6867c67ca");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "1bd78f72-8650-471e-bc37-6dbcbbcbd8f1");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "0f5a4008-419f-4e20-81fe-ee503fea51b4");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "9fd03564-95e7-435f-82be-b9efe8e1218b");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "cfd7fb33-d65b-49a6-afb8-b8448d7cdbf1");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "e655a46f-c59a-4dab-bb39-49a91c340f1d");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "7b8d3cdf-023b-4a6f-b854-6c2a4327b646");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "a08c9895-0be5-4af9-b46b-82ae6e4dd530");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "624b507a-e485-4300-b823-55e2a886d377");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "180539cc-54fd-4a58-b0ce-bdbfd96a1d5c");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "8704c6da-8e29-4af6-bdf0-0e61b97d93ff");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "95d28d81-ecf1-4841-a6cf-a6b664241692");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "46eb7d6f-257c-4958-92b4-033f0e95fae1");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "4b15751a-38e9-44b6-b916-9f41df617396");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "510ed3a4-e0c4-4800-9736-39b491f936a5");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "dd88758f-ee5b-4684-aa63-3be394a9d415");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "fc426923-d7ca-4ff3-bdd4-6f99b90d471a");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "c5aa17cc-634c-41cb-86c2-01e48eb4028a");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "2e65e826-898e-4bb4-bf0c-24d30a4d5bb3");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "8f55e206-537b-422c-a530-7d9c082f1821");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "e101abcb-b37a-452b-aeb9-77e04ad32a88");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "e0c2a604-f5ca-4e69-a74f-535fd166c358");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "814695ce-3678-4b71-93ed-7a0369176169");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "138e5f0d-43b3-450d-8480-dddebbc969fe");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "779a1460-d27e-4f40-a063-f6d60a1380e7");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_7_10.coverage", "4e85774c-10b7-42dc-abb6-cd2b0d8f255e");
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
