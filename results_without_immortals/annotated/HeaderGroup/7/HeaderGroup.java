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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "f6286f09-9d63-4768-8dde-e23fdc3fbffd");
        headers.clear();
    }

    /**
     * Adds the given header to the group.  The order in which this header was
     * added is preserved.
     *
     * @param header the header to add
     */
    public void addHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "da6b34f8-47be-42a1-a16f-b6fc1d6172fb");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "f0ce0e63-f97a-47c9-bff1-86193d426bb5");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "308dde11-a9fe-4a4f-8eb6-e7f907d97a99");
        headers.add(header);
    }

    /**
     * Removes the given header.
     *
     * @param header the header to remove
     */
    public void removeHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "8fc592a0-c8ed-4478-92e0-0fc849801985");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "037f8b61-2b9c-4fef-9344-ead62a4bb68d");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "2cc48be4-4178-48ea-ba4b-3b19942a5bdc");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "d1cff5ab-7f7a-4b6d-af2b-09330ecbe293");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "2e88a436-c8f0-4bfc-be87-3809ff95c2b2");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "e030b5c3-39a8-4c4b-879f-e3195e7ea304");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "c37ccaa1-170a-4daa-be89-0704e5af80c0");
            final Header current = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "3d4f1369-a353-498f-88b9-ad6fb427a3fa");
            if (current.getName().equalsIgnoreCase(header.getName())) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "2f306062-65c2-41c1-943e-62568d62283c");
                this.headers.set(i, header);
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "1ccf95dd-85e9-4d3a-8f95-971f5c43c729");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "43995bad-5bba-4131-9a85-4fe280b18edf");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "a3735d8b-469c-4ebb-9ed6-9d2972c515f9");
        clear();
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "e52daba6-d473-4fff-ac73-5fa40b6c7589");
        if (headers == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "dd1b8beb-4f83-4c29-a079-f08854086cc4");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "035fb4e5-0d94-4c38-862b-623ef8176d5d");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "65192b56-abe4-428a-b539-c62d6b90f124");
        final Header[] hdrs = getHeaders(name);
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "0c043508-01f6-4f15-9733-92fb86bc31a9");
        if (hdrs.length == 0) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "697aa0b7-f17c-49dc-a38f-99ddaaba8646");
            return null;
        } else if (hdrs.length == 1) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "d9e980ad-b5b3-48f8-b356-5aa52ba0ae93");
            return hdrs[0];
        } else {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "6b38fef7-096b-47e7-8314-56dc8d81052f");
            final CharArrayBuffer valueBuffer = new CharArrayBuffer(128);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "74b21118-c4b4-41b1-9e99-2b84082ab4f8");
            valueBuffer.append(hdrs[0].getValue());
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "5858b0f9-eda2-4655-8f87-fdbd7049201f");
            for (int i = 1; i < hdrs.length; i++) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "489c0a28-4e2c-40f3-ba04-50c53cc5b21f");
                valueBuffer.append(", ");
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "96e24edd-cbe4-4950-ab4f-e55ba0a3bd5c");
                valueBuffer.append(hdrs[i].getValue());
            }
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "a70c3327-73dd-4251-9685-a053ada4e9ea");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "a2a4ff74-089f-46b1-bfce-e9f9ce810a9b");
        List<Header> headersFound = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "44186b6d-49b6-45fb-bd15-9d992c454011");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "b8c028a5-f7c3-4a9b-ae73-63ece7974a7a");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "2af34b13-946f-4713-91e1-8933495ce3c1");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "d6b4337f-f4d6-4ab9-ae2b-6609e026d3f9");
                if (headersFound == null) {
                    writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "685343e5-c85a-4f3e-857b-7d36e3530943");
                    headersFound = new ArrayList<Header>();
                }
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "f0c9f0ad-b9d9-423c-89d3-2067284f6d13");
                headersFound.add(header);
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "b7a5bf6f-2eef-40c1-8f62-e5d2452f5918");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "b8d85c76-410d-492b-ad50-ab16ad712595");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "93f11493-e1ea-4a68-a002-38d3c0b42506");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "56a194b9-1654-4527-b011-eafb778f4a10");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "c7cc3c60-5040-4199-80fc-72c925151d7a");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "f9f3aa25-b3b8-464d-88af-dc76937c6bcf");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "49ccb642-354d-4599-bdc3-e56416d53b75");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "818f1b92-5ae7-4b4c-886a-9cb40271f687");
        Header singleHeader = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "ccc037ed-8acd-46a5-b3e5-a8c5a63f1362");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "d6e1a7e7-7c1c-4e9a-89e4-932ba2d097a3");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "1aeac9b7-73e2-4c45-9636-99b4b0bc4532");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "3741a42b-d044-4303-a9ce-6861ab7b2729");
                singleHeader = header;
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "80328989-1121-48b1-82a1-7e9af361106e");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "8c742376-0274-40d8-bd40-a2fe661ca6dc");
        if (count > 1) {
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "b462c257-957a-4c0f-aa33-4230e342d5a1");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "c29bf0df-c35d-4cc7-87f1-3609d4d21927");
        for (int i = headers.size() - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "e187d04d-4f2d-4935-9357-4395d80ec57b");
            final Header header = headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "c563f594-c6ca-4792-b58a-19aada95c60b");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "342925af-a235-497e-943c-a44f14786e5d");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "b5bc2cb2-b1e4-4df3-b80c-082fe51747eb");
        return null;
    }

    /**
     * Gets all of the headers contained within this group.
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getAllHeaders() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "a1e71894-a554-4009-8ba7-01fc0aaa6d1b");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "85f71761-41c8-4f84-b8d8-52b6fecc329b");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "ff65b9f5-eb46-488c-94af-09b19327e232");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "c6616bab-cb81-46e5-9ff3-04adb92bc977");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "db881e34-a0b3-4f34-86b7-fe8554f9471d");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "78d9a3cf-c19d-4849-979b-d1f07c570284");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "4e6063ac-694f-48f4-9faf-6f618da0491f");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "f8c77081-84e5-45c1-9434-52e15bf5be3c");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "e1d45bb8-6acd-4110-b3c6-4452bf4bd99f");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "396fb1ad-710c-423a-85dc-8b5b21781487");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "17d5f207-f4a3-4f7f-ab22-038fce79e663");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "73e170e3-52ec-4d47-b065-8c57e1fed5d7");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "70d84e43-e135-4773-b94f-8b7b165cd5e5");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "1b0ad212-21ef-4e04-9b2c-00da393c83ca");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "9df573fd-6d2f-414c-8072-d851cf67d99d");
        if (name == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "c8c1c561-e703-455a-b15a-223b868daede");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "97cb492c-aa4e-4cf8-b9df-38b24664ba83");
        for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "606c782b-4c3f-43c9-a7f2-08593bd3b6d5");
            final Header header = i.next();
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "d6edf8a7-1d90-428e-8015-230510f491af");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "399562d5-2fc3-4130-97cf-9719948eb6cb");
                i.remove();
            }
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_7_10.coverage", "dcac999d-c15f-421d-98ed-a344da595165");
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
