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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "e19ed1c7-9349-4d23-9330-43b7fe95dd65");
        headers.clear();
    }

    /**
     * Adds the given header to the group.  The order in which this header was
     * added is preserved.
     *
     * @param header the header to add
     */
    public void addHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "fed70472-296d-46b3-bc51-3cf9b75cb2ef");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "242e57cd-8fe1-42c5-aea7-9715504706b3");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "c2a991a0-3e18-443d-8e48-2b9e4de622dc");
        headers.add(header);
    }

    /**
     * Removes the given header.
     *
     * @param header the header to remove
     */
    public void removeHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "be4db958-7d56-43af-ae58-0cfbaae86069");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "98b3ca76-40db-4010-8400-86c134f76129");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "941f4494-3eea-4966-b0b6-81cbaee040e8");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "2f2bb778-c87c-49b2-af09-87182057466c");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "41e3b888-0bc3-4c2f-8d16-632470b4f54a");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "be2d6481-fdcd-4a8c-b958-a3c4b96c63af");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "ba069d2a-b63d-4ba9-be9c-d8a3bd2accad");
            final Header current = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "c6521f97-3aaf-4d53-bd7b-26e213bdde45");
            if (current.getName().equalsIgnoreCase(header.getName())) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "bba62954-17db-4a8e-85b2-53189e49fe2f");
                this.headers.set(i, header);
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "a0c2ae74-9b37-4e8f-bc99-dadde696a2cd");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "eab0e89d-cb2d-4def-b780-535fdeee759b");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "d72f519b-2a5a-430b-861f-1ae822a938e8");
        clear();
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "63f703b7-ade6-4c39-8a0c-5392a1334260");
        if (headers == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "cba0a55d-5cb0-4c6e-958a-16076ed03d6c");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "8c8deabd-bff4-4f63-a767-c26bc5d6dcd3");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "54a4dd5a-8104-4c15-9b87-9f5306989365");
        final Header[] hdrs = getHeaders(name);
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "5df5fddf-4f33-4075-85d8-78921674a4b3");
        if (hdrs.length == 0) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "60fc9847-1291-4e5b-86d8-0b6eb1a2659a");
            return null;
        } else if (hdrs.length == 1) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "c57309ad-5ab4-4388-a38c-3cbc81a9352f");
            return hdrs[0];
        } else {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "86333efb-2995-4f8b-8548-8bae63098018");
            final CharArrayBuffer valueBuffer = new CharArrayBuffer(128);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "a4b9bdcc-4db6-466d-a535-1275d2c4f2d1");
            valueBuffer.append(hdrs[0].getValue());
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "24e0b9f0-2f89-4d4e-ba93-14bcb34c5fc2");
            for (int i = 1; i < hdrs.length; i++) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "2541a345-b180-499f-a9a9-13418ba4b2cc");
                valueBuffer.append(", ");
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "dd6a2a37-7a28-47c5-a5b7-7ed2fe5a1bb8");
                valueBuffer.append(hdrs[i].getValue());
            }
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "331bf393-2bb9-469b-99a9-a735c6d89d6a");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "37d6816c-f91a-43c9-8d85-46a47cce88bd");
        List<Header> headersFound = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "aa04c6e7-6d5f-4509-8d8b-5c60bf5127ff");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "8bd390f5-8245-4c67-ad89-a72e5b1776d8");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "fbd79601-19d2-4ffa-9895-844fceac2ec7");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "12c9ec20-66a1-4778-9183-4e57ec2999a0");
                if (headersFound == null) {
                    writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "fac8a2b9-a70a-4631-a862-eb874f57bae0");
                    headersFound = new ArrayList<Header>();
                }
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "4a8ec7fa-d701-4a45-a95b-abcc632399a6");
                headersFound.add(header);
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "6b37ae32-caf6-4c85-9b9d-fe2e15cd3ad3");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "ca9d454f-6995-4513-a83c-f7c65df9402e");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "15734ea7-b84f-4629-97cd-b27ef97ef030");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "be7d814d-afd3-4286-8ad0-85905091a5bc");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "6c6a1f21-323a-478e-95f7-7d96481b2664");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "5bdb8a7a-a0aa-4b7c-9690-798339c1740b");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "f15a8c91-c15f-4456-9da6-8751ed836c84");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "1e3741d1-09ed-428a-a9c3-b2bca17447ec");
        Header singleHeader = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "e8dec439-9719-48ac-8a59-35682cc1fe8c");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "e81d259c-8baa-47da-a614-3ea493a5d714");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "0925bf60-775e-4154-a3e6-2230a425cba1");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "d460a4dd-e4b8-42f8-8039-f0914d91bcbf");
                singleHeader = header;
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "ef45ee00-fe78-4c17-933e-a929a717301c");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "dfd6214b-5c6c-4b79-9e8e-025543c8bd50");
        if (count > 1) {
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "13c94495-5203-4bc4-9844-d084f5f8b18f");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "987d31fc-4d3a-4147-8b0e-5bd8fdf9b1aa");
        for (int i = headers.size() - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "87147ea1-f4c1-494d-ac5d-495c377b7291");
            final Header header = headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "6139d6f4-7ed9-4d9d-bc3f-fdbef0f3baee");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "26dd7ee3-fa50-41ee-b0c5-7a468dd28722");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "f36e6da9-61b0-4b2b-8ba8-118e36efb4f6");
        return null;
    }

    /**
     * Gets all of the headers contained within this group.
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getAllHeaders() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "af01266c-3e04-486f-bcba-979e86e00688");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "663688f5-cfca-496e-8ff5-f378db5c2f50");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "7c4c728e-4ece-4273-b391-46534ab5d3a0");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "a88be6ae-d2ba-460d-a146-45c41f7aecaa");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "3969bb03-bb8d-4159-a7e3-8a3dfd35cc06");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "69914237-32ca-41cd-8021-baacb71ab2bb");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "824f129b-159d-4e54-83fb-9bc6b4a1068d");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "a2506518-d113-4a13-bc25-325f5e6c788d");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "44cb3be2-211f-41d8-a416-c95e7a81848a");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "0655c735-9d87-4975-9514-82ff53d68e40");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "c4b6ecba-9e6d-4d21-aa62-53d576b950ca");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "f252d354-855d-4f84-8750-5189815ea554");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "dbf12580-03ab-4ff5-bf43-ff8a9a351f2d");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "8fe6070e-9edb-4996-a4c5-cc07b12f11cb");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "272ac816-11f5-459d-a86a-20c6ecfe7ac3");
        if (name == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "8e3432c4-5627-46a2-b840-bd96ce9bfbc6");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "6cbe3a5d-c01c-440e-9fca-67b06282e890");
        for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "844c3ff0-3cf0-4921-9d65-e5974771531e");
            final Header header = i.next();
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "6e5c3a3b-f6eb-4748-a0c7-0438e072f9a8");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "36f296b2-d8d6-4b99-ab02-6554cdea541e");
                i.remove();
            }
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_1_10.coverage", "2338b8ce-61f3-40a3-96fa-f0007ce5bd03");
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
