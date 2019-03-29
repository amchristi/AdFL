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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "9a0c8613-2c01-4a30-8395-a88c6ac3f577");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "814ce1d0-0921-4581-88b9-481387340358");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "572d3192-3ebd-4a01-81c9-5f25946fc396");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "5a09a54b-c568-47ec-9207-e71c9cbcced6");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "91c8cbb2-150b-40c9-b2a5-69cdc83d0fe0");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "bbe51b7a-3919-472b-a029-107c32aedaa3");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "5ae25701-75ff-4244-8355-a1514e968cdd");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "2fbd2aeb-25bf-405a-aba8-d75469e66be2");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "34851f6d-748d-43b4-961c-7b30cf64124e");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "3829653e-aa27-4d11-9777-956e47505656");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "ab3c6ac5-53d6-4c19-b2f9-f3fcc1f9becb");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "6df142e4-7293-411e-a408-da8b65afca86");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "88babb3c-86f4-44ba-85c0-a4e00dabaa6f");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a8bd209c-7400-41bf-b723-d49f7d10b9a9");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "232418e7-c6ef-4940-a266-c2b98efa5575");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "103f4cb8-d920-4aa5-9735-24c7b663e739");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "6bbe644f-7e2d-43f4-b6d6-cfa41b86d97d");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a0b88c63-1e81-49c1-928e-2e5770f51cc8");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d32f4512-59e4-4c42-be89-c3b32c7df520");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "9fbd5d09-aa0f-4147-a7a7-ce247b253cd8");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "4f543f25-bfcf-4ca8-a796-3fb29bf85c11");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "db0e8abf-c6f0-41a4-92cd-5e96094d6928");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "1f288aff-d7f8-4dda-b6b9-e881eb647e65");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "fb06bb7e-0e79-4406-ad8c-c4d05d69338f");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "66fb1dda-9769-4046-b04c-d5fc11faaf3e");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "778bb4e4-85df-4923-9553-94bf204d7f4c");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "0d6f3cf6-97d6-4722-8b5b-5690beebf470");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "02703926-5e7e-4009-b988-6fb561e097e6");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "01184e6a-0cfa-4165-b450-391d284fae21");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "bf28a61b-010e-4ea8-8d8d-e4d17a72e618");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "4ae41a7c-0ef6-4f3f-8f2d-d48fbb8b134a");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "fcd92013-3682-4e03-935e-8479971b2bde");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "b8d26a97-b78d-4090-a586-141eb3d0237f");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c81d8ba6-9bec-4479-9993-1795bc9cf2d9");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "cfb3a135-acf3-40a9-846b-19fcb0e4ee05");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "b3fe549b-7c8d-42c1-a742-7e8a7f5af42b");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "e3544e0e-d3fd-4232-990a-6d3f3d334eab");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c96e7ae8-939b-4da6-a938-4cddd208059a");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "3b6a2373-82ff-48bb-a544-44c4e9897b37");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "142f7395-d4d5-4545-a0a3-a5bb96686aac");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "1c3888ae-c09e-48c5-8ca7-a8bc41b2034a");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "73650d6a-f84c-49cb-b540-daa3403b40f2");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "76000b21-2346-428f-9d2a-b0a55eda4569");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "dd371bc2-faaf-41ab-9f35-d0faf164e51b");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "324cead0-4128-4702-9645-b64844052299");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "df848857-aa43-4b6a-a507-a21115edb906");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "07094f3c-5f06-4f13-99ea-c0317da7b096");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "928e81ae-ffca-4a1e-96ef-44d85132c56c");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "95589766-de83-4a4d-9b38-653fc3846d15");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d9f53bef-5883-4ae3-a452-1f845d71d238");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c4055d4c-894a-485d-8ced-a0407ea92aa8");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "58682e75-fbd7-4403-a619-61945c6b5ebf");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "b1cb5130-5691-4959-a963-19ba90379f3e");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "eeb3b9fe-6457-4fec-b20f-b069e2b405cc");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "8d653ea3-a58a-45fc-a9de-28cb413e1f40");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "6d6c8d21-37a9-4c3e-a066-e0833f9140f3");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "34eccb6b-f8d7-4204-8f6c-e19721d462cb");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "f92cd484-eee0-4e47-aa38-d4b70d8e8c46");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "da699021-4d59-4ef4-b875-129c135128a5");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "5154e467-1c5a-4968-91c6-4a0a8b71c6c5");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "bd3e3e02-c737-4e1c-b8ef-f0395c0b31c7");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "3fa0e3c9-f1b8-4571-89cb-d9fc11b9eba2");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "f1b592d9-c40b-4670-98fb-c323870c9405");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "6617c19d-564d-4bf0-ba08-c355430d3f92");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "0584629a-0c2e-4e18-bb11-e20db4a539ac");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "14609500-3947-44d1-88cc-23d437acb9ed");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "66cf1a8d-685e-41f4-8dc4-3ff37cc23eee");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a1c90857-b01f-4613-91dd-20486f704c55");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "742d7f1b-60ef-4364-acdd-5e6ac2069eaf");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "0a2ac7f0-8b26-48df-b0c5-39826eebaae9");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c126adcc-22b2-4102-8604-8504d89614c1");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "29150045-27e8-4d13-bf79-c379c014e5f0");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "96ed6f3e-a121-4806-bd02-2bef79d9abc2");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "72b75647-c23c-4a6a-b823-be0c4a8b1352");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "403c0dae-26ec-4474-97f0-ec4c7de9ac57");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d1684b37-08e5-4cfc-8fdd-875622086484");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "bda65bdf-7860-4753-91d9-31ebd747ae84");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "7977059d-4977-4627-8c80-7bd06ad4f1a8");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "cea9e3f7-8ddd-4645-957d-c6440226184b");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d4a3c874-11e1-4baa-9f92-1c8976578c63");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "5c8ffafa-a67d-4ec7-b119-b8516d92994a");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c93e9f03-be00-4944-97fe-4530927122fc");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "17ba8210-d001-4dff-b323-1976a801aef6");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "8b2bc550-f9a9-4121-899c-e36b474ea8f8");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a5b40b69-6f0c-4263-baaa-df8d2798ed74");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "e7988b40-882c-464f-ad6e-fcc13fbfcd5a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "506b5d03-b45d-4103-9caa-8515c3e79624");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "96c1feb8-b955-436f-894f-5e5333d186dc");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "b007a52a-e6c6-4b6d-a3de-4f74a5a350de");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "31cb498a-4210-4038-8baa-c81848ce17c7");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "dd5507f1-48fd-4a95-bb1d-6e5ac1530fa2");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "8e80aed3-0ac8-4a91-bc2e-9caa08921285");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "75d605e4-922b-49d6-890b-7abfe172133b");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "0a3f6094-353e-42b7-a8c0-48b6ce66e97d");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "4343d083-7eaf-4024-a976-2e83bea72dda");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "143da703-494d-4514-8590-0bf521a3d464");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "390eeaa6-c2b9-451b-a4b4-af070814ee3d");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "3066bb14-7206-4a2a-b66a-b7a24a08137a");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a726dadb-edc6-4b25-8cda-a958fa9ea4cc");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "b097d714-ae8c-4233-baea-7f9ae062d022");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "017d356e-94bd-4625-9dd9-d386fe62ac8a");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "7cc0bd90-e9b2-46de-8f10-3af0668ce93c");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d7a4bade-aa26-4bb5-9c2b-bca21af8ec24");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "92702971-b806-445f-803d-877873da15db");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d9f2468d-6641-4bec-bcea-9ab8f74f206e");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "bb03c5e8-5bec-43f2-99dd-6cb8e108ef57");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "2af5123e-eb84-4de5-9471-802337836667");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "8585a00d-91ad-43b7-86c5-33797542ab37");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "437d4f35-0321-4950-90ec-7afd5715d83c");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "9692ecf0-6619-4881-b9ba-fb45f2cff22f");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "5b8d2e7e-04b0-4b17-89ff-fa34da516b8c");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "091d04c2-e92b-4e72-94bb-4f05cf7987b9");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "53867c8b-6aa2-4eca-b560-16e26b141a4c");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c3318c56-e0c4-43e8-9f36-763a7095ef44");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d4d530ab-0c52-43df-997e-b320fdacbb10");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "46c11f79-6815-467e-b30f-d33bfcd4dc42");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "faff9c2b-32c5-4cf3-8a48-6ad160a1401e");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "63c89a23-bf4b-4d08-8e04-cfd6f2e12412");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "b84c8c93-7ed4-4eb7-b1b0-e8fcc41c4c62");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "8b70784a-dd66-47a0-bea6-5fa1a3f60a42");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "8f7d22df-7eb4-414b-a141-80a27798f226");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "771c734a-57e4-4836-b928-55a7d5e7ee31");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a39b073a-0b01-43cd-905c-62e612119fdf");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "31523110-46d1-41c6-a7b1-4107ac7b5292");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "9237146a-21ea-4f86-93ae-ca96c4dd1815");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "4805e11f-b7a9-471a-9800-b7cd1d7c97a5");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "de0e565a-7128-421b-b816-cc4f3726e065");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "35fbcd30-1b14-4da6-a3db-ade96b1504ec");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "e2f0bab2-568e-4abd-9a78-b6c753a16140");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "ac6e8973-5b05-4414-83c3-82884e91b174");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "f851bde4-5de7-4e3b-9818-4a92df82eb5c");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "d7705def-2674-409c-9848-fe985edc836d");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "23953e1f-1ced-495f-a908-7d2cccb49125");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "866d3247-f2e1-4b82-bd95-5ec33a76e26c");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a05377a4-83bb-420a-8c34-c1f87f73782a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "78b25785-1dcd-429f-9779-b5e1cf1c4a11");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c6f5e41f-f60d-4527-be15-792d264e87e8");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "588efa1e-2564-4c78-9244-37e42931f84e");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "a29a29ff-8812-4790-9181-8ed748c2b16c");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "c137683e-c243-4d14-8395-d401d72be920");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "30ff6cd1-6826-4d03-a4bb-7aa1db4a63a9");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "ef2c35dd-4deb-4f25-bc67-fbdda1034418");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "85648ee6-963e-450f-bbe9-4f551aa27baf");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "4ed06801-6f97-4e13-b44e-6fbd99f9896f");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "ddc82045-33a5-43e1-9d78-4836442c2ba7");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "521b277c-3642-4490-a6bc-336c9ffd9c37");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "cb9e95cc-9f25-4404-9f1d-fe826d90602c");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "f3a7064f-a991-47a6-9477-8e3a7eb223cb");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "850546ae-4f5b-43fb-ba38-b593a472edc6");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "400e8a98-403a-408e-b4dd-eddb073c4cf8");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "9e94e402-c749-4998-af1c-9787834a7b1d");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "1a0466f5-fb7c-4add-9559-fe6c30750bfa");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_9_10.coverage", "561175e4-6447-4657-bc12-edef2ddb6d0f");
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
