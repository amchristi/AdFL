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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "473e00a5-98d6-495e-bed3-ae66e7a7f222");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "ed651b99-e659-4729-bcad-d0021f4673a2");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7113b260-93ab-4acb-8ac4-a85c2bc8e2de");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "5cef3930-0705-4a89-b62f-68a283ab8651");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e4fcf2a3-dc94-40d0-970c-3d2a24410e43");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "16f508b8-dbde-4d4e-af09-619e34667c4b");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "db450861-28ff-4023-a403-c18f2aa1aa10");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "32b34fe4-f605-4fca-b306-d218ef154cc1");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "c7033a91-fbf9-4589-bed8-7e0f0b0f5589");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "4ab03739-5fe0-4987-9fa0-3226dcb2baf0");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "ad0d9fee-b9fb-442f-9692-6c822fd2584f");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "148e0add-8090-4f35-a887-58b6e0b414ec");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "49a6d437-64b7-4588-adec-b7f8c1042a6c");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "9231f5a3-d863-4274-8ad2-884b42f647dd");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "95d3d72f-d528-44b0-ae52-a727b4e19c03");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "0e0d937b-6abd-4340-8abe-05db09747f67");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "af6fd11c-6436-432e-a58e-e6449ed07a28");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "1f2c4651-864b-4729-9679-e13dfe174be2");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "48089646-6a0d-4741-888c-818dc760a1aa");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "71c3a370-cdad-48cb-8fb9-1dd27eef91d0");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "9c41b167-707c-4891-a883-9a0eb8be30fb");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "46041caf-ef59-4f69-952a-54b2a6db3bea");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7e7c2711-50e4-4ce0-9fc5-547b7b9a4ae6");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a9f7b9fa-06a0-4fc1-ac6e-9c4a0b61099c");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "5b860d9b-b110-47f0-9101-0a99e671ea38");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "ba1d688d-f774-42ca-8ae7-01ee50ae66e2");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "02b5fab7-faa1-462a-96f6-669ba59585cb");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "9d3f11e6-f065-4370-b468-3cdd450ead60");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "eab33b37-50b7-4a36-a227-9386a485af0c");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "03a5d3fc-0455-4bcc-887f-bd3f0a4a8e0c");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e974bdc2-2ccb-4e6c-8730-21913731710f");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7f4b8a4d-14b4-4a3b-8cc3-751d450cbf33");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e98d87c4-c0c3-4018-bf15-928eee0ec616");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "13a738e3-9c2e-42b3-938b-72677b1ad217");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e0bc799c-195f-4db9-8b7e-2ef64498a117");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "2c176fcb-ae67-49d2-9746-3856d0802095");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "36781681-eed7-456f-8498-195bd9cf9979");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "f18a4177-21b4-4954-90a7-e4fafe5994a5");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "84388974-7d35-4c82-ab3e-edcdb558b9d2");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "909d7c2e-0761-4640-bfa3-c55b8426f8bb");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "89295ecd-7ed1-4608-97a4-0ec60fa57f39");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "c4dbc88e-66af-46bc-8fc7-f62b28da031a");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "267417c9-813e-4070-ba74-801f88bdb640");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d8dc5eea-3e73-4429-a363-1036cd39befd");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d6863558-7748-4ae1-8a3f-e1a16184b335");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "272cbfc9-1492-4f49-8405-18d789864e90");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "90ae18f1-7087-4125-a1e2-844b66093f91");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "10f1a568-08a7-4e16-943b-4aa27ecaedf0");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "aecc272e-e52f-444b-9e84-eacf30d446b8");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "ce0dbe7a-1feb-4c28-891f-ebd1694341af");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7c3768d0-2531-4b82-a299-f6cf65f0a835");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "80638efa-8b2e-4fe8-9f98-49e8c85415d2");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "73fcbde0-be03-4ff3-ace0-e50fbb03c18d");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a6840490-8226-481b-8402-a9277cb73eae");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "ce06ad1e-f8ab-475d-91b0-a42a44bce847");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "f084b5e7-99c3-4de7-85e3-3341e13e1af7");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "0e2770bd-4272-4526-b254-9b2d47921df5");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "69abab78-9b38-423b-8348-487f5ebe02f4");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "968d125a-f4c7-4694-900e-73b540c14b1a");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "2f833bce-55f9-4d97-a0d3-304e07a7754f");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "78929863-36ab-45f6-af7f-380548dfe2d9");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e23df1d3-b899-47d4-8dea-e698e96c82f3");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "b915d0cc-c48f-4f2e-822b-5b54591de988");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a2e8e95a-6b8e-428b-a863-5ebb293afa1f");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "dc9a9583-81a7-44b9-bd30-80bb908d1deb");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "3a987ebf-9e47-4578-af51-5a0eedaec56b");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "71bdcf41-5689-428c-b66a-667eae2c5bc2");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "40948611-e8c4-4e09-98f4-32bf0e475fa8");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7ba7b416-f7c9-4d8d-a08f-5cc84e93f516");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "cd7b713c-630a-4c7f-8fd5-408439336c11");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "9b0dc8aa-9cd5-42f1-927f-06af60aeba64");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a35720e2-7e08-4411-a6c6-8d76165a5d4d");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a8555d36-f90e-4239-a0ce-647ebfe1933c");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "919eedd8-dfbf-4f1c-b295-ccaac675f179");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "b8540006-328a-4512-987d-352a9b2e70a3");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "9158b9f8-908f-49ce-9232-2c6bf04e3c18");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a71a3334-9aa7-4826-86df-cb10342d7bb9");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "91e2b7fc-4353-470d-a2f2-c421668b6bcb");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "fb55fdb7-07f4-4fe3-87e4-393772adb094");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "caa648b4-fb5c-46d2-8d12-78176ec283ae");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "93611cb2-5191-4501-93fd-ae4e2054a8b0");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "50b7c72f-da2c-42b1-8d2f-d3b9bc869d01");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "37857130-65ac-4ab1-b83f-f2ab48f72081");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "3277f1bb-231d-4bc3-b390-3905720de9f3");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "fd01487d-5a2e-4c8d-92c6-d6f96fd93b5d");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "65464e2b-6bee-4c88-a972-ec20f7e72d51");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "c561c1d5-2c27-41dc-9a9a-3d624afcab51");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "91db8975-0728-43d5-ae17-a1ceba010cfd");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "72089cf0-924a-447b-ac1d-74f8ea9121cf");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d0980896-fb84-444b-a0fe-ca18bb2dff55");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "f11170e1-ab67-43c5-b09e-abcd30303b19");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "86187f0d-bed0-45ca-97c5-e7a81f850890");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d8fa6b41-3cae-4eda-9b86-5931c14ccde2");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "91bdcf94-f570-4a8f-8f28-be11174e06e8");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "76c734bf-41e9-4707-9e87-bebefa8bdd82");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "cdf0905a-daeb-423b-a0ba-37e07f7071bd");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "90d70b41-0be9-4c5b-bffc-8db8170648d8");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "4de340b7-d02e-413e-b5cf-eadc24dc52ac");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "92e8287b-05c1-4e57-af03-c610ea6de5b7");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "122216b2-35dc-4a20-8c47-51fdf908d4aa");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "27c2e288-41af-455f-b3b1-0e639eab105f");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "c738bb3b-15fb-4c62-8268-e91a41c18efa");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "3aab4703-65b9-49c5-bb9d-475b462fca26");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "92e9474a-7e1b-4248-a539-de4c3713aa96");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e54a200a-a767-444f-bf0c-1399c35d6c72");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "ccaec00d-fe73-418b-ac46-cf765cb9c058");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "13682255-0a90-435f-9ed7-2018fee24025");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "226085a8-8f2b-420b-b566-225f9c71d614");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "da3b9fdb-81a5-4b57-8a51-c9a47ab8f9f0");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d118572c-dc21-4c71-a643-3049566f3350");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a2546641-4df5-44a3-848d-92f058d4e5a3");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "854ef3e2-9156-41ea-b45c-99a9cbb789a9");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "371ded56-9664-4842-b52e-a563e6a5a26c");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "73cd7580-72c1-436a-8923-4f8bbdd6051e");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "24921fcf-3071-4578-b65e-ecb880b4d439");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "271690fc-e2eb-40fb-9016-6c2c75fec788");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "1843c723-d4b5-4299-8623-30fa8d6f6832");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "66ac4ce7-9391-496b-ad9c-7e60a607f3c6");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "ca906016-78a4-4215-956e-2e74e9e61c7c");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "75100c3b-2ca4-4eda-b035-d9c396be8fac");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d9aeb3a2-7a7a-4708-aa3e-8cbeab21bd88");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "8f9e26e7-bd46-41c7-a8b0-279b57cc11d2");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "2944c749-083c-4c88-b9c4-9460d67d9180");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "edcfe6e6-ba3f-4461-a29a-ed661b2c71c9");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "211793dd-90b5-4aa6-9016-62dde8e0bebb");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "1fa8ee4d-7424-4e0d-aa79-3dbeb463759b");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "26f730bf-1e42-4af8-9fc7-16b86902aab7");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7fa4d60e-805b-467e-a42a-c6098051e934");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "9f191313-da0f-48f3-b29d-e0de6f379217");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7dbb8d43-e916-4fea-b31b-f0ecbe7008e8");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "f0341894-e8d3-4260-9ba2-2ba59002c247");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e1605b6d-c498-4ec4-97eb-7792592ec2e0");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "8b817d26-831c-4838-a9df-7e665c890a6b");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d7220de2-a7a0-4837-83ef-1fe46a68fece");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "e6f2c176-b9dc-45b4-b483-d0f685852280");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "afd6854a-b725-4421-a100-0a3e957e26a6");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "56a0c220-bc29-4233-8869-24c6add4ba0d");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "35b30805-8f4f-4471-8f1b-8dace81afe67");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "6e9f8833-acff-479a-828a-f4608d043df8");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "180b5bc8-5e2e-48f3-bf59-68c601af8f3a");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "c2812687-c0bf-4f8f-b6dc-c1c20ee6a5d1");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "5aa035da-de01-4ae3-8a48-5abf1038af9d");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "7bcde274-89de-4dda-9685-3e2216535034");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "aa7f69c3-bf8b-4aad-9775-49429c73f7c7");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "da99b063-84d6-4981-ac7a-095cd2372ebd");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "c862bfaf-4b39-45d4-ae1d-739bb3007726");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "d9080c4b-3b12-43c9-b863-ea08c8d8fe1c");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "26b32133-7dec-4705-9304-1daa5d6591b7");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "6300ca4e-d2b7-48fb-bfbd-fb802167eeaf");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "33e55ea1-d4bb-432c-9e7d-b551752161cd");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "124a2320-6533-400b-97f1-f7af9eaef0b7");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "9cbb4001-fc7a-4517-b5f5-bae3465a603f");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_5_10.coverage", "a0fe34d9-4a9e-430d-ad6b-868054325c80");
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
