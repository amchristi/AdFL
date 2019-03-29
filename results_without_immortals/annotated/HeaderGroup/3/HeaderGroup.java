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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "71c94658-77d7-45b5-a40b-b57e114af3d8");
        headers.clear();
    }

    /**
     * Adds the given header to the group.  The order in which this header was
     * added is preserved.
     *
     * @param header the header to add
     */
    public void addHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "4c49f083-67e8-48f4-9ea7-0286af225d9c");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "c1d9a258-5776-4d66-8437-208b2e5d3d1b");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "8642835a-dc96-4839-8ce9-7c90528f0841");
        headers.add(header);
    }

    /**
     * Removes the given header.
     *
     * @param header the header to remove
     */
    public void removeHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "261323bc-b7de-482b-84a4-e737a27841c7");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "9b1f4185-db2f-4cfc-8106-8a5aaf36b546");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "1a2bce48-cb4d-43a8-b027-f26776c88c08");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "8a11fd2d-c6da-41a8-bd4d-a6fd57313d96");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "b0472cf4-ab6c-455f-96b1-9874c7becf19");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "2c190631-d3e9-4f39-8aab-2d4dd9e2ce15");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "7b88bcd3-0e03-4611-95f6-4d230afc3b23");
            final Header current = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "d261dfc2-3561-4e94-9c03-db72b2828b39");
            if (current.getName().equalsIgnoreCase(header.getName())) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "fadad2b1-55ac-452a-afc8-389455d03f78");
                this.headers.set(i, header);
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "f4c5ce2c-6c85-491b-94ad-f7374a7cf8ed");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "3863e857-6329-4c40-9d10-da03abebb61c");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "a328c59c-3277-4f6e-bf12-d922327c82b6");
        clear();
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "11ddfd0a-adad-4380-913a-2a69e3338d41");
        if (headers == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "dccd282e-978d-4366-a443-927de25d0891");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "f6da0dea-a526-4dc9-8b67-537567e5fc22");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "3ee74fa8-b95a-4883-9a2f-b08b52127cba");
        final Header[] hdrs = getHeaders(name);
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "814b6d1b-202c-4e78-8b3c-e5b6571101e8");
        if (hdrs.length == 0) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "bb313ef6-9ba6-42c9-b465-414d0ca2249f");
            return null;
        } else if (hdrs.length == 1) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "ffd817b1-a41e-4432-8792-b06a77908206");
            return hdrs[0];
        } else {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "09f675f4-b585-4756-9651-d0aeaf7956e4");
            final CharArrayBuffer valueBuffer = new CharArrayBuffer(128);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "885c3ddc-459e-4908-ad72-c121a94c0197");
            valueBuffer.append(hdrs[0].getValue());
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "7c9015e3-126f-4691-a27d-7c9323a6111d");
            for (int i = 1; i < hdrs.length; i++) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "57518e5d-0956-4a86-8ca6-cb80c1f5ea2c");
                valueBuffer.append(", ");
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "3b3b8da6-d77d-445c-9a75-486211d05998");
                valueBuffer.append(hdrs[i].getValue());
            }
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "ac80e5d4-2604-4b35-bea8-81c030852695");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "2eeb2cd0-1a85-4ecc-a4ca-a2bcd58f2186");
        List<Header> headersFound = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "c4d8c433-e6c0-45aa-8db0-f4af46f1a515");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "9bf864f5-7a48-4ba1-b579-688bc0734ea1");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "36f94149-6926-4878-b231-2b034dbf0f20");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "8e5ac991-73aa-4074-bfaf-1390c69e1340");
                if (headersFound == null) {
                    writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "ee63d636-b218-4a33-aab9-6d9545b5bda9");
                    headersFound = new ArrayList<Header>();
                }
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "a105643a-5fef-4299-b571-7ef1660b8adc");
                headersFound.add(header);
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "05a8f985-05ee-4dcb-9ad6-b14857b9ac8f");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "5b9693e9-7dab-4207-aabd-45988109e686");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "31630572-adce-4a1c-a843-9a6e9aeabcc8");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "fe73a708-0d09-475b-b460-6a1f49b86864");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "9c130282-1fe3-42c5-ad7d-6169784eab2a");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "080232d7-2c65-4b74-a6d2-d8e4f5d0bca4");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "17a1b129-31e9-4c77-bc66-1e1d93e0cf89");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "f9b9d361-92ab-4bcf-b0dd-a9cc12376784");
        Header singleHeader = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "344bb293-d89d-4124-8115-524febf298f4");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "6bc67ba4-3bc1-41f3-99f4-ae642b8bfd5c");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "be71e6f0-3b9e-4855-a119-c35c7db1ba1a");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "bbc9b414-bb39-4e11-a1b3-7993daee0fd1");
                singleHeader = header;
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "dc647891-f899-4941-9dfc-9404acd28ac7");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "8c75964e-04bd-46f5-8835-3170af479af9");
        if (count > 1) {
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "f53bffa5-40c5-4904-903c-15a3eecfd2ef");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "ae0b9302-b733-4713-bf1b-68b7f1273a56");
        for (int i = headers.size() - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "74cfc72a-01bc-40ad-9145-9a84ee529e5b");
            final Header header = headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "34e55f08-7776-4c38-b55f-54d5a2dea86b");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "a334c43c-9224-4b92-8547-48f93f6a8aa4");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "02ebb916-661c-4003-8b1a-feb6556b12f2");
        return null;
    }

    /**
     * Gets all of the headers contained within this group.
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getAllHeaders() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "4c214f65-3961-4f58-8efc-0a3f0b7c1200");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "65bab3f0-1d2f-432d-a15c-2256796b9171");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "9da836b5-5b24-440c-bb46-f49b0a9f70a3");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "e64fb96c-bf63-4b07-ad47-6917f99180df");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "08544ac1-ef77-4cfa-85c9-1780c60f1be3");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "0e5d55df-10c3-44c6-93c3-1f504bca6df5");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "63c81756-1914-4421-8304-017311b1078e");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "2ba0b6d8-323b-4de6-a256-49edbcacc370");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "00585728-9b52-4907-878d-a4829653aeb7");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "ed614aaa-babf-4a09-bbac-363bb2ac00d0");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "2799f578-419c-41ff-b7ee-7db18b001426");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "bb421a58-9265-419e-988e-42d8ff0561d6");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "f03ad142-f0ca-4555-8df0-03115f090063");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "06928f18-93a4-48bd-8cdb-e7abfde65f85");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "d23cd0a8-7e58-49ee-9517-4f4f84d5bfcf");
        if (name == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "ff5457e0-1c65-47a1-8b95-c93f8691eeb1");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "73e1423d-5d62-4cca-92dc-8fd671b690bf");
        for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "b26acb71-6d1d-4bb0-8e8d-5af3f49382a2");
            final Header header = i.next();
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "4585bc8d-efd7-4ea2-8098-b4815331c781");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "543b3a6d-7c1f-4faf-a00e-e0984d367d8f");
                i.remove();
            }
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_3_10.coverage", "dc45864d-9682-46e9-8283-880de0569a5b");
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
