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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "c9ceb1e0-f11c-453c-acfb-720911878f75");
        this.charset = charset;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ecdef308-db6d-4438-b0bc-63e65015ee3b");
        return this;
    }

    /**
     * @since 4.4
     */
    public Charset getCharset() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "2c5c9208-32ea-41fa-b9e6-1d00cd67d8c8");
        return charset;
    }

    private List<NameValuePair> parseQuery(final String query, final Charset charset) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "73d075f3-7a91-45ae-97c5-cba907b9b109");
        if (query != null && !query.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "f831d7b3-8805-41ac-9b81-c1ca46ad1ca3");
            return URLEncodedUtils.parse(query, charset);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1a31a90a-e43b-4a91-a695-a4353502ef0e");
        return null;
    }

    /**
     * Builds a {@link URI} instance.
     */
    public URI build() throws URISyntaxException {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ff16ab93-6994-4635-96fe-b6b47bc7a821");
        return new URI(buildString());
    }

    private String buildString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "56645ea9-84e9-4c44-9b70-790d425a211f");
        final StringBuilder sb = new StringBuilder();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "02c095cf-3e39-4054-88bc-6cf6175c3c79");
        if (this.scheme != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "4c7e40ca-4189-46a7-ac92-5b54761cd2e4");
            sb.append(this.scheme).append(':');
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "233d1970-edb4-4ef1-8c67-bbb93c1f4b09");
        if (this.encodedSchemeSpecificPart != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "20473cd4-5f88-46ad-8c63-1a7803061948");
            sb.append(this.encodedSchemeSpecificPart);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "84e9d745-de9f-4494-9adc-5c5887b9aebb");
            if (this.encodedAuthority != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "8e6f291b-77db-4ab9-b286-0f1b63daa8dd");
                sb.append("//").append(this.encodedAuthority);
            } else if (this.host != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9f68586c-8faf-4175-a080-8145f8079230");
                sb.append("//");
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "666950ea-bc6b-48b9-8a25-a91ea10dd1c0");
                if (this.encodedUserInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "0d1869a7-abbf-416c-a11a-8d639dbc68df");
                    sb.append(this.encodedUserInfo).append("@");
                } else if (this.userInfo != null) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5bb04196-2d26-4b79-a640-69a7014d5db4");
                    sb.append(encodeUserInfo(this.userInfo)).append("@");
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "b5b58cde-13a3-41f5-849a-b74633d7db39");
                if (InetAddressUtils.isIPv6Address(this.host)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "b2deedb0-5bd3-4cd7-b389-8beb26300d88");
                    sb.append("[").append(this.host).append("]");
                } else {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "c4a14660-6e9e-497d-92ad-b5b9ec1e351c");
                    sb.append(this.host);
                }
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "966fee4d-3186-4d25-99cc-ec7608669e39");
                if (this.port >= 0) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ae00df6c-8393-49c2-94d3-7aacc8728abe");
                    sb.append(":").append(this.port);
                }
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9fec8103-5dc9-4aa1-8f28-3d906bc7f871");
            if (this.encodedPath != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "a08a577e-7ecb-4ad9-a3d6-89e29a6908f4");
                sb.append(normalizePath(this.encodedPath, sb.length() == 0));
            } else if (this.path != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "44015b64-c233-4db3-8503-1d765d8f5d54");
                sb.append(encodePath(normalizePath(this.path, sb.length() == 0)));
            }
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "6fa6f9d1-3162-4f3d-a1f1-62c68cabfdda");
            if (this.encodedQuery != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9fcae1ea-7d26-4486-92fc-f24828fa2ae5");
                sb.append("?").append(this.encodedQuery);
            } else if (this.queryParams != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "24d82b6b-6276-4c04-bbfe-f4808784237a");
                sb.append("?").append(encodeUrlForm(this.queryParams));
            } else if (this.query != null) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ba29c18c-e140-44a8-bf04-16a292b5370d");
                sb.append("?").append(encodeUric(this.query));
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "d8514c3c-faaa-4d19-ad78-c9f12bb83444");
        if (this.encodedFragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "e79fab7c-3aff-499e-95d4-d4e24725200d");
            sb.append("#").append(this.encodedFragment);
        } else if (this.fragment != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "32851a8a-e1a6-4d3e-94d0-1a6bfd3e5718");
            sb.append("#").append(encodeUric(this.fragment));
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "4802a039-10a2-4e07-bbc3-ff5d2bbff13f");
        return sb.toString();
    }

    private static String normalizePath(final String path, final boolean relative) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1eb2558b-5cfc-4f42-b62d-d2997d430d46");
        String s = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "478d0971-abcb-428d-a062-13471e2cb424");
        if (TextUtils.isBlank(s)) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "bd0e4cd8-8271-4454-b215-34a33450dfee");
            return "";
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1517f1f9-6b1b-47cc-ade6-250ea98bd184");
        int n = 0;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "680c9c6e-a794-417a-8a6c-2c19e6fa1f42");
        for (; n < s.length(); n++) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "af630fb5-a43b-465c-bfa6-e96bafa5ad47");
            if (s.charAt(n) != '/') {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "85a05656-0e4d-410d-a257-41e7efa2646c");
                break;
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "2db3bd76-863f-4e90-813e-4ead10a97c0f");
        if (n > 1) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "6e18d83b-4143-47dc-8fd1-070282c13640");
            s = s.substring(n - 1);
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1a8a0417-60c7-4ac1-b63c-82c28b1508b8");
        if (!relative && !s.startsWith("/")) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5a6183d8-1287-4e8c-8c95-1cf2015a2686");
            s = "/" + s;
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "66f04b91-d2c6-4609-92b8-49fa7ac53286");
        return s;
    }

    private void digestURI(final URI uri) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "2b804d48-e5f8-4255-86bf-d0257384d7f0");
        this.scheme = uri.getScheme();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "34afce80-a8bc-4a18-b11e-9ee72b3e9b68");
        this.encodedSchemeSpecificPart = uri.getRawSchemeSpecificPart();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "94e86d71-92c8-4b9e-8d0b-bf111eddb2d8");
        this.encodedAuthority = uri.getRawAuthority();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "13630828-e3d8-4735-a257-8c29a60cb6c2");
        this.host = uri.getHost();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "65d63b5e-73ef-4c83-8a03-96aa5979db1e");
        this.port = uri.getPort();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5066f9f9-860b-498e-a7cb-b588d611afcf");
        this.encodedUserInfo = uri.getRawUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ed2141b0-25dd-46ee-addc-2fbf2fb5a498");
        this.userInfo = uri.getUserInfo();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ecec74bd-fb65-41a5-b008-403d7d28b4e4");
        this.encodedPath = uri.getRawPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "df14477f-dfda-40dc-aaef-0aff621d582d");
        this.path = uri.getPath();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "20ebf785-6247-447e-babd-2a2bdac3bb09");
        this.encodedQuery = uri.getRawQuery();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "0cb692a3-862a-4f11-beb5-9eff348c4365");
        this.queryParams = parseQuery(uri.getRawQuery(), this.charset != null ? this.charset : StandardCharsets.UTF_8);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ee792794-eb57-4ba9-8011-85fbeb3ac823");
        this.encodedFragment = uri.getRawFragment();
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "efd61f5b-68a2-4083-a74a-f3d5bdff24c0");
        this.fragment = uri.getFragment();
    }

    private String encodeUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1226cfba-5dc7-43bc-9f91-4b8e5fe7a8f5");
        return URLEncodedUtils.encUserInfo(userInfo, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodePath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "21d23f58-2658-4ef4-8a78-02d2282349d8");
        return URLEncodedUtils.encPath(path, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUrlForm(final List<NameValuePair> params) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "86cc08f3-15e1-4934-9319-24bf5d87fa70");
        return URLEncodedUtils.format(params, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    private String encodeUric(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "7d99d9b5-ad30-4b9e-8bd0-6a15f3326af3");
        return URLEncodedUtils.encUric(fragment, this.charset != null ? this.charset : StandardCharsets.UTF_8);
    }

    /**
     * Sets URI scheme.
     */
    public URIBuilder setScheme(final String scheme) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "0059bff5-ecaf-4edb-ba33-2bbf5f042868");
        this.scheme = scheme;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "8aef0001-2606-4c8d-8838-d422815b7d75");
        return this;
    }

    /**
     * Sets URI user info. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setUserInfo(final String userInfo) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "dc935676-3ceb-45d3-99c2-dcdeab801abb");
        this.userInfo = userInfo;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "89ce7217-62f8-4fdd-b8f9-a7a23b436a63");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ffc0e39c-5989-4cdf-8cde-a3f22a08f34f");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "95117a46-f0e5-4a58-b158-4b93f748da51");
        this.encodedUserInfo = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "f436d71b-e421-4f20-9c21-179fb33e4bba");
        return this;
    }

    /**
     * Sets URI user info as a combination of username and password. These values are expected to
     * be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setUserInfo(final String username, final String password) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5b005de3-aa18-4425-b433-5a4879cea9a7");
        return setUserInfo(username + ':' + password);
    }

    /**
     * Sets URI host.
     */
    public URIBuilder setHost(final String host) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "e7f445ac-3124-40ef-be93-5a612a433e72");
        this.host = host;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9258b166-244c-4078-9104-004f380c2167");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1386304f-1ad0-4ed8-a4e0-a623e15d9ec7");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "05493989-4002-4b4b-a307-44faeb384fc1");
        return this;
    }

    /**
     * Sets URI port.
     */
    public URIBuilder setPort(final int port) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "d9b10b05-a73b-41c9-b1de-8e802ea2f634");
        this.port = port < 0 ? -1 : port;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5153a751-75b2-4237-9de6-c06999650fba");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "3f9c6ad6-ee9c-4367-806e-55978fa3fa16");
        this.encodedAuthority = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "067f9c4a-63c5-4580-aea9-5c1b69bd5cd6");
        return this;
    }

    /**
     * Sets URI path. The value is expected to be unescaped and may contain non ASCII characters.
     */
    public URIBuilder setPath(final String path) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1c6b4d87-83c0-4923-b185-d25927a87559");
        this.path = path;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "c2716cc0-8571-4f5a-bf47-b94cbaf70d83");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "c0916d2b-f03b-4544-ba97-55bcb305481a");
        this.encodedPath = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5b88e713-169c-4361-8916-c35cc88c74bc");
        return this;
    }

    /**
     * Removes URI query.
     */
    public URIBuilder removeQuery() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "e2dd3d67-9d23-4b86-b350-53b4cf1dd1f6");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "6b919f22-aa44-4ae7-a724-771ece83851a");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "563d3aac-70a3-46b6-afe5-17569d2c98f7");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5f8fe640-2c1c-4194-ba1d-97d5d45894f8");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "65734c19-a040-405d-8964-453f5e437cd0");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "c0b7f117-117d-499d-a3cf-bf0fb1914fe1");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ba46c384-ea41-49bd-a5ed-e95859eeb1da");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1aecda95-0d89-4d0e-a72b-f860b719bb25");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "31ded9c4-34fa-42f2-b772-9141845fd292");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "fa7ad7ed-dd78-481e-8aad-288e5053231a");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "69c9cea6-e20c-4649-b103-81118236aaf8");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "daf158ba-a1c3-4a37-a7fd-448d87ba7ce1");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "52ea5f8f-bb92-4886-a48e-3c6d9155b6c9");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9b0703af-2c92-4c6e-bf1a-34327869d433");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "bffa59bf-0903-41bd-b89f-e99f22f55c42");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "394f8a70-e2fe-4fda-a95f-6b331145a990");
        this.queryParams.addAll(nvps);
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "339e95b0-51db-445e-ab1a-c608585fbb33");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5873510c-71b7-4dfd-aec0-85a10844571d");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ff803cbb-2948-474b-80dd-78b9c5873887");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "fea35558-53ec-44f1-9915-6c3dbd66c977");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "2b70f21a-fe59-4040-97aa-960a332f4b1c");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "54c59bd6-adbb-4f5a-a438-0ea4d7d145a9");
            this.queryParams = new ArrayList<NameValuePair>();
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9c2a0da2-e63a-42a0-8e05-31a307e40831");
            this.queryParams.clear();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9c6559e3-e26f-4181-ae78-bf1a3c82a791");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "0ca88e11-6426-43ea-ba52-f1baef65576b");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "a5b85e45-e1ff-44d4-a1f9-18c8da66039a");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "4db9d78a-f5de-43b3-a67a-05d5c003b6ff");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "fd5537ce-fd07-41b3-88df-fadff43d7a57");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "388815c6-dce4-4358-b6ec-7f049a08c40d");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "f9da7e09-ec11-4246-9fec-99bf0108c5c8");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9bf35102-360a-47b4-856d-2a4afdc7e972");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "3156fffb-fdc8-49e2-9775-aca8541504fb");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "79d4810b-07b9-4497-be9f-71c2e434fc95");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "793b1ba9-e3c7-465d-9657-88cac1913c23");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "4fc781d3-1222-40b1-b613-2a84f4b6a2cb");
        if (this.queryParams == null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "fc903f1d-adf7-47e8-941d-567aba603ac4");
            this.queryParams = new ArrayList<NameValuePair>();
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "0e478316-3238-4dc1-af34-e6900ceec368");
        if (!this.queryParams.isEmpty()) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "6e9807bb-dbff-409c-a260-e06df2990463");
            for (final Iterator<NameValuePair> it = this.queryParams.iterator(); it.hasNext(); ) {
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "f1a4c85b-b388-4cae-b3b6-48815697508e");
                final NameValuePair nvp = it.next();
                writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "03568a15-0734-4300-9188-86d2dcfd7332");
                if (nvp.getName().equals(param)) {
                    writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5321c436-15d3-4513-baa4-59735f03aecf");
                    it.remove();
                }
            }
        }
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ef2f9665-e2c3-4759-898f-1539537ba347");
        this.queryParams.add(new BasicNameValuePair(param, value));
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "05068960-4cb2-4b89-96b7-94bf8b5203ea");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "34408eba-0cb1-43b7-845f-b636b14834db");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "9d1d0035-6efb-40c7-bdb8-e21cee549fbf");
        this.query = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "06f2c9bd-5bd1-42eb-b8d6-068b60feb29c");
        return this;
    }

    /**
     * Clears URI query parameters.
     *
     * @since 4.3
     */
    public URIBuilder clearParameters() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "1967d7be-9742-4f14-8a54-f9965436a157");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "a88228bc-332c-408d-abc0-fa09a194a2e7");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "dc0a6f10-fdf1-46cf-94be-85f7a880c9a3");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "c34dc917-0a2c-494f-b8ad-a3f1cd21f09e");
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
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "d7f39ec4-5b02-40d2-be4b-c4e97e329ef7");
        this.query = query;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "f93c60a6-b1dc-4ada-a32e-f356af3d7324");
        this.encodedQuery = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "6a404c40-ffc6-4306-a23b-9b396db395ad");
        this.encodedSchemeSpecificPart = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "f823a857-3785-46dc-a115-882364d51260");
        this.queryParams = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "44b2ffa5-acd5-460a-8f11-bf3d3b49a81e");
        return this;
    }

    /**
     * Sets URI fragment. The value is expected to be unescaped and may contain non ASCII
     * characters.
     */
    public URIBuilder setFragment(final String fragment) {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "2dc0953b-b9a8-427f-b63e-b679372e2e50");
        this.fragment = fragment;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "03c5a637-b9c4-4060-96a3-41e451c7b68c");
        this.encodedFragment = null;
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "09813f23-4d98-41e2-809a-420b326d95c8");
        return this;
    }

    /**
     * @since 4.3
     */
    public boolean isAbsolute() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "96f8f6d7-65e8-4ee9-bb32-e76baa820801");
        return this.scheme != null;
    }

    /**
     * @since 4.3
     */
    public boolean isOpaque() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "a1df65fb-ae0c-4c39-a910-4305ff981469");
        return this.path == null;
    }

    public String getScheme() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "a7f56496-c91c-428a-96ca-6cc9fc167727");
        return this.scheme;
    }

    public String getUserInfo() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "23bfbedd-36d2-4309-a2d0-a3f0242c5a95");
        return this.userInfo;
    }

    public String getHost() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "5e71bbf3-4cd2-4905-afba-484bae5c4429");
        return this.host;
    }

    public int getPort() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "27ab9384-d776-47c5-bdc7-7e29630ff3eb");
        return this.port;
    }

    public String getPath() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ee2b5021-f4c4-4ad8-804e-0c51be0c4dea");
        return this.path;
    }

    public List<NameValuePair> getQueryParams() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "80b9b8df-cf7b-48ad-ae7b-e67a6b9d5d52");
        if (this.queryParams != null) {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "44b3c1d7-5ec1-4fcf-bb9e-ff143e4a1bde");
            return new ArrayList<NameValuePair>(this.queryParams);
        } else {
            writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "ef5f1e83-3136-4e39-be59-9d2843a89195");
            return new ArrayList<NameValuePair>();
        }
    }

    public String getFragment() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "b421c67c-f7dd-40cb-be50-ee67f48e8c5a");
        return this.fragment;
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/URIBuilder/URIBuilder_8_10.coverage", "c3d3e6b9-13f7-4cf3-8a25-25152838f8ec");
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
