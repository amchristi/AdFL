package org.apache.hc.core5.http.message;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import org.apache.hc.core5.http.Header;
import org.apache.hc.core5.http.MessageHeaders;
import org.apache.hc.core5.http.ProtocolException;
import org.apache.hc.core5.util.CharArrayBuffer;
import java.io.*;

/**
 * A class for combining a set of headers.
 * This class allows for multiple headers with the same name and
 * keeps track of the order in which headers were added.
 *
 *
 * @since 4.0
 */
public class HeaderGroup implements MessageHeaders, Serializable {

    private static final long serialVersionUID = 2608834160639271617L;

    private final Header[] EMPTY = new Header[] {};

    /** The list of headers for this group, in the order in which they were added */
    private final List<Header> headers;

    /**
     * Constructor for HeaderGroup.
     */
    public HeaderGroup() {
        this.headers = new ArrayList<Header>(16);
    }

    /**
     * Removes any contained headers.
     */
    public void clear() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "718a2224-b8fd-4e0f-85d0-2b2594cd0aa6");
        headers.clear();
    }

    /**
     * Adds the given header to the group.  The order in which this header was
     * added is preserved.
     *
     * @param header the header to add
     */
    public void addHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "5f4e4c5f-c215-4679-9737-ac046ca83767");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "58401235-4dc7-4010-a697-2bf4ff0cadc5");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "c138a87d-f928-4846-9a48-4260e4855e04");
        headers.add(header);
    }

    /**
     * Removes the given header.
     *
     * @param header the header to remove
     */
    public void removeHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "0bb2508b-aae2-4364-988d-13f25f430692");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "7a834d77-e200-4bce-9332-d147e3886daf");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "9ad3461d-7eed-4b91-8187-9a327b4cc225");
        headers.remove(header);
    }

    /**
     * Replaces the first occurrence of the header with the same name. If no header with
     * the same name is found the given header is added to the end of the list.
     *
     * @param header the new header that should replace the first header with the same
     * name if present in the list.
     *
     * @since 5.0
     */
    public void setHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "c4618690-c6d0-482e-ac2b-9749e88aceee");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "64783fda-d235-4f77-82fc-e306b09adb06");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "8b063f23-d347-4879-9951-fd7e50aeef2f");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "278cb50d-a57c-4e93-956a-7aab84a99c02");
            final Header current = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "fd687ef6-73a5-4e74-85c2-5d9a443d5669");
            if (current.getName().equalsIgnoreCase(header.getName())) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "48d9ea2a-24af-43e4-92f3-c162ed996d09");
                this.headers.set(i, header);
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "261d8b1b-d0c7-4f4c-9a67-4bef42ce305f");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "87cd469c-52da-4b18-aec0-62ab5a030a73");
        this.headers.add(header);
    }

    /**
     * Sets all of the headers contained within this group overriding any
     * existing headers. The headers are added in the order in which they appear
     * in the array.
     *
     * @param headers the headers to set
     */
    public void setHeaders(final Header[] headers) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "ffdf72f6-c92c-44ef-bfe6-cd809147f2fc");
        clear();
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "dd84eb1c-8fd6-4c81-a89a-c1f1e93e3b1c");
        if (headers == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "a040cfd2-1f31-4282-9398-05184af25f07");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "f4423951-04aa-4441-a071-f57b3af653e7");
        Collections.addAll(this.headers, headers);
    }

    /**
     * Gets a header representing all of the header values with the given name.
     * If more that one header with the given name exists the values will be
     * combined with a "," as per RFC 2616.
     *
     * <p>Header name comparison is case insensitive.
     *
     * @param name the name of the header(s) to get
     * @return a header with a condensed value or {@code null} if no
     * headers by the given name are present
     */
    public Header getCondensedHeader(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "7bc4b783-a927-48bf-b880-a7548ef5ecf0");
        final Header[] hdrs = getHeaders(name);
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "7e83e811-913c-472b-9580-bf3f5a6cdd32");
        if (hdrs.length == 0) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "80498f9f-8977-4867-a37c-766daba80276");
            return null;
        } else if (hdrs.length == 1) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "cec9c5aa-ceff-4cac-928d-11b58522c61d");
            return hdrs[0];
        } else {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "50ca9e5e-95d9-4ab5-93b6-79c2e732499a");
            final CharArrayBuffer valueBuffer = new CharArrayBuffer(128);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "ed538d76-8580-41f9-b70c-7e3707e054b9");
            valueBuffer.append(hdrs[0].getValue());
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "3c6d88fb-6cf3-49ee-83bd-8aca5081b029");
            for (int i = 1; i < hdrs.length; i++) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "23bb3e0f-443d-4d76-855b-9b7cd6345061");
                valueBuffer.append(", ");
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "3a8363d8-5557-4d68-9b97-a534534bd2d8");
                valueBuffer.append(hdrs[i].getValue());
            }
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "2c625eaf-5cc0-43ae-81bf-b72d320b59d5");
            return new BasicHeader(name.toLowerCase(Locale.ROOT), valueBuffer.toString());
        }
    }

    /**
     * Gets all of the headers with the given name.  The returned array
     * maintains the relative order in which the headers were added.
     *
     * <p>Header name comparison is case insensitive.
     *
     * @param name the name of the header(s) to get
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getHeaders(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "4c996984-6578-4d4e-a176-b8eccc57580a");
        List<Header> headersFound = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "faaf2fc1-d1b4-4485-9c7e-29722de19d8a");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "385beb6c-e97c-4825-9fe5-0e86bf2b8fa5");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "3264f333-4c94-4b7b-8c16-db4080637911");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "ddff8b31-92e2-41d2-92bd-aabe98e2aaca");
                if (headersFound == null) {
                    writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "6d408954-8b53-47ae-a7b8-bb17da81ab39");
                    headersFound = new ArrayList<Header>();
                }
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "12a00007-e6a3-4c0d-b0f8-cce178cd62c6");
                headersFound.add(header);
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "5d57f06d-5cef-4a33-be22-8d87dddca3cb");
        return headersFound != null ? headersFound.toArray(new Header[headersFound.size()]) : EMPTY;
    }

    /**
     * Gets the first header with the given name.
     *
     * <p>Header name comparison is case insensitive.
     *
     * @param name the name of the header to get
     * @return the first header or {@code null}
     */
    @Override
    public Header getFirstHeader(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "32b2a44b-c218-4e53-ae29-6eb245d21317");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "256744ab-5a86-4c28-8bb1-601577405c57");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "b73b2341-24be-4af7-a49c-55f55467a99a");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "4ae35ecb-8d86-4fb2-92c7-344bed4ae64b");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "e3c2cc75-cc3b-4c49-8e55-82efc5327dfd");
        return null;
    }

    /**
     * Gets single first header with the given name.
     *
     * <p>Header name comparison is case insensitive.
     *
     * @param name the name of the header to get
     * @return the first header or {@code null}
     * @throws ProtocolException in case multiple headers with the given name are found.
     */
    @Override
    public Header getSingleHeader(final String name) throws ProtocolException {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "c98bae70-593d-4dbd-a6c9-d4ef60ce3d65");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "aa6ccfc5-323c-46ca-a67c-a4e4c07e57a6");
        Header singleHeader = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "187aa0a1-9442-4a7f-b243-ba0582774958");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "0ced36aa-6e5d-4592-92f8-4ee00f065578");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "040d0141-1db3-40f5-b9fa-4bb67e2e4d0c");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "8ea7c8c9-20f4-44c4-bff4-e2979db9d406");
                singleHeader = header;
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "f75e2f11-d324-4aca-a213-49c3404e519f");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "f88f8d21-7982-43d1-ac1c-d075726e6883");
        if (count > 1) {
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "7bc99a86-f841-4562-aa25-d0c17f1d1179");
        return singleHeader;
    }

    /**
     * Gets the last header with the given name.
     *
     * <p>Header name comparison is case insensitive.
     *
     * @param name the name of the header to get
     * @return the last header or {@code null}
     */
    @Override
    public Header getLastHeader(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "a9009d87-de72-40ca-839a-53c0c3b57624");
        for (int i = headers.size() - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "07e26ebc-0b50-455f-a733-da8c1d9364b5");
            final Header header = headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "201ae2ca-c359-43ef-988c-18014038e12c");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "251d8c6b-2fe4-481f-8aa6-3ef62665dba4");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "e0ce7f97-10e2-4e72-a7bd-4fee1858645c");
        return null;
    }

    /**
     * Gets all of the headers contained within this group.
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getAllHeaders() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "b517d66b-71c3-4c74-ba3f-04014cdf4b9b");
        return headers.toArray(new Header[headers.size()]);
    }

    /**
     * Tests if headers with the given name are contained within this group.
     *
     * <p>Header name comparison is case insensitive.
     *
     * @param name the header name to test for
     * @return {@code true} if at least one header with the name is
     * contained, {@code false} otherwise
     */
    @Override
    public boolean containsHeader(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "f38b342b-c420-4215-ac79-3f0fa86d7526");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "8fe59134-357a-4084-8e13-2b51e8d43f72");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "abf1b624-878d-4810-9ec3-9f94a049712b");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "b8470ae0-a750-490a-bd61-e2f78ced6f44");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "012ea020-ff4f-4941-a46f-b90595c29f99");
        return false;
    }

    /**
     * Checks if a certain header is present in this message and how many times.
     * <p>Header name comparison is case insensitive.
     *
     * @param name the header name to check for.
     * @return number of occurrences of the header in the message.
     */
    @Override
    public int containsHeaders(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "d121f6ce-fe95-4e5b-b818-a52185ed25aa");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "1a2dcfaa-75ce-4a58-96fe-29c040ede008");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "3b899deb-de8c-480f-a703-5ae2d22f85e7");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "7da760ad-8497-4a68-a22f-8c7b358232dd");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "b25bd884-4297-40c7-8877-7dad5c86096e");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "f8d1af25-6aa5-4f91-b197-93422eb0d74c");
        return count;
    }

    /**
     * Returns an iterator over this group of headers.
     *
     * @return iterator over this group of headers.
     *
     * @since 5.0
     */
    @Override
    public Iterator<Header> headerIterator() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "88e7eb96-b6ef-4d01-8d83-db6e3f62626c");
        return new BasicListHeaderIterator(this.headers, null);
    }

    /**
     * Returns an iterator over the headers with a given name in this group.
     *
     * @param name      the name of the headers over which to iterate, or
     *                  {@code null} for all headers
     *
     * @return iterator over some headers in this group.
     *
     * @since 5.0
     */
    @Override
    public Iterator<Header> headerIterator(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "5176f5f9-533b-46ea-be14-cd303f8110f3");
        return new BasicListHeaderIterator(this.headers, name);
    }

    /**
     * Removes all headers with a given name in this group.
     *
     * @param name      the name of the headers to be removed.
     *
     * @since 5.0
     */
    public void removeHeaders(final String name) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "7b43bbe1-8dcb-4cf5-a1ff-c91312ef0fd7");
        if (name == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "1a481d95-05e5-41b5-8379-bf2d1fd00e57");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "634b475c-0c6b-423e-8c1c-148d1163791b");
        for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "26d6dea0-14a3-4c36-a924-573e108f64d2");
            final Header header = i.next();
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "ad22e9ec-abb0-4654-958c-3f14a879d00c");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "da0a5c1a-19a8-4ba5-8303-a26dcffab79c");
                i.remove();
            }
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_2_10.coverage", "53d80091-2ded-4bf4-a691-469a354743d4");
        return this.headers.toString();
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
