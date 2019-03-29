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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "97c55568-cd98-4b50-b16e-0cc29a48751c");
        headers.clear();
    }

    /**
     * Adds the given header to the group.  The order in which this header was
     * added is preserved.
     *
     * @param header the header to add
     */
    public void addHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "27cd4491-017d-462e-8bfa-940dab015c77");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "406e2f0a-79c8-4762-b3a1-fcaeef446354");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "e510d061-0b5d-448e-9933-ea49661cdea3");
        headers.add(header);
    }

    /**
     * Removes the given header.
     *
     * @param header the header to remove
     */
    public void removeHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "cb3e4ea4-91db-444a-841c-d1c2601ef36b");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "7185c3c9-ad8e-44e2-9704-c1fa48d9dfae");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "a814b005-9c24-4210-9501-8771b2e75f08");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "71ab5208-4837-4153-82b5-53d57ee66421");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "a29d0e28-9304-42d3-ba41-6aa3bf4499ce");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "d2c66464-0909-4457-82b3-4f264459f976");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "6f9f7d97-81aa-490d-a751-1776384a0180");
            final Header current = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "7e72949f-a2e1-4f40-a678-9657668c9760");
            if (current.getName().equalsIgnoreCase(header.getName())) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "4f603391-c62f-4953-970e-320b0466afa3");
                this.headers.set(i, header);
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "9b645803-006a-49c3-8d67-c8f433017bcb");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "d450a24a-48bc-476c-8204-f67eded5e627");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "40001ecf-8f9d-420f-9b96-72a4e74a30e6");
        clear();
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "abad39f8-73ca-483c-89f3-9492582daae1");
        if (headers == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "7ab56921-c694-4a68-aaf1-40e197cbfa3e");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "b77079f4-9ddd-4a31-814f-5a877503c832");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "df1cfc89-9f92-4512-96cd-bc04fe2ee734");
        final Header[] hdrs = getHeaders(name);
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "57cb12d9-3c0a-4bb6-8d5b-81ef4d2c1a46");
        if (hdrs.length == 0) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "2261d93b-ba65-4ea0-8a02-1fa88e702748");
            return null;
        } else if (hdrs.length == 1) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "b6ff0631-c52e-4b3e-a76e-b91c1ecbf2ef");
            return hdrs[0];
        } else {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "2b7873af-b95d-4bb2-abbc-ba3ae11614f8");
            final CharArrayBuffer valueBuffer = new CharArrayBuffer(128);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "1569058a-f083-4907-843f-abca3af1af7d");
            valueBuffer.append(hdrs[0].getValue());
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "51faa84d-524f-4f8d-999f-3f6102b0e7a0");
            for (int i = 1; i < hdrs.length; i++) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "a861a782-1aaa-4c88-ac26-64f57f774415");
                valueBuffer.append(", ");
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "a112dba9-29cb-4322-8da1-785785a8811c");
                valueBuffer.append(hdrs[i].getValue());
            }
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "fd1aaaa2-fdb3-40d9-a29d-ec7a1ac8e54a");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "867b636e-6603-497a-8f6e-84f14c661f10");
        List<Header> headersFound = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "68d0ce10-a9f8-45d9-9449-c8b46901c2b1");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "5b2df331-fc3d-4b7c-bc1b-670348b7a2d5");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "1e7e8eb0-bfb1-4cf8-a793-5e999edb82f9");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "59cdad45-a4cd-43fe-a6e2-f1217bd47314");
                if (headersFound == null) {
                    writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "b40c8bc5-f05b-4857-b6d8-43544d7c4f1c");
                    headersFound = new ArrayList<Header>();
                }
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "6bf6db4f-f2aa-4d82-afd6-69b107b92d4b");
                headersFound.add(header);
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "9faf8327-5eee-40ab-a5fb-8770cf5de375");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "3faa70c1-57fb-4419-acc3-1129cd537fda");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "740488ea-bbae-4c8a-8952-82b351c5b921");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "fc4d0ed4-c4cb-46c1-8096-8f7097cc5509");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "1a1170d2-0b49-40d3-bdc6-b95ab88a3f63");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "3ddb006b-7147-4d5f-a609-96f3334959e0");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "c19cbe7d-ec4b-4b73-8e94-bb63089b57a6");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "a8acfef1-71db-4cd6-9d57-3cc5a6131027");
        Header singleHeader = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "48700b1e-0660-4d32-8dd7-1f477df50dc1");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "318605cd-d03f-4e1d-a78e-482083938558");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "6530f6a2-a90d-49b1-8307-2fb26e6c8259");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "b43d7db3-b117-41d1-916d-0028fd63ac5c");
                singleHeader = header;
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "60682843-afac-44de-a9df-6ee316fefa7f");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "da62cb1a-4a22-4492-906d-5f3bbdd993ef");
        if (count > 1) {
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "ceda94d8-4dec-4f14-8dcd-1696ed37849e");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "76c959eb-8d1b-4cd0-accc-a790ab91e70c");
        for (int i = headers.size() - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "f14e806b-e3ff-413e-8869-feda2cada973");
            final Header header = headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "e393a530-59c1-4bea-a38e-188fe0e9a42a");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "c4618997-f30b-4879-ab3f-4f7d884898d4");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "4cbfafcb-98c5-4dcb-898b-cb0b4224485b");
        return null;
    }

    /**
     * Gets all of the headers contained within this group.
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getAllHeaders() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "2b0c950e-2ddd-4f1a-a7e1-ff5c0d119d45");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "617803eb-1004-4ca5-9d6b-9cda147bbf48");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "3cf67ad9-6356-474c-8d80-76789b89cfc1");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "3aa54a91-84fe-4571-a549-4ba2fe60eb31");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "4e864669-9d40-4ade-b903-807e3bbc5feb");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "55760b58-9820-40ec-bfcb-f9b3dc795ca5");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "16484b48-d694-44bc-96ac-5459aaa261de");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "9281312e-78e0-46cd-a348-946a0ae689da");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "bd73bff5-2b23-4c0e-b646-6d1d24b4333b");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "663c7014-63b4-4015-aed0-36113fd53f98");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "4c054ac8-e683-435e-8301-5f2b428473ff");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "4fc438d6-dfd3-42d1-b009-eae90572fa95");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "440c6af3-f629-4ee7-bce7-5b008cb0a197");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "cc74daa2-9e53-4c2a-aa6b-7fe5e1d5f606");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "e1ddaebd-2d0a-4c27-adcc-38452527e2dd");
        if (name == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "4fff22ef-4397-400f-b636-c4a4fd6958b2");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "9ad3cd87-38e7-4281-a507-16931993108e");
        for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "2fcc64ad-83b2-45b7-b4c3-6d0b7671a89a");
            final Header header = i.next();
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "8fb38cb6-6f4f-4f1a-a37d-266f89db57b7");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "88cbd173-d07d-42fb-a0d8-1c39a11f7ef2");
                i.remove();
            }
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_5_10.coverage", "96ceed5d-4066-47e1-9a95-41cad48359d0");
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
