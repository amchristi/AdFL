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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "f6461248-b627-4441-b9f7-0b75ae4b89db");
        headers.clear();
    }

    /**
     * Adds the given header to the group.  The order in which this header was
     * added is preserved.
     *
     * @param header the header to add
     */
    public void addHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "3ebe9822-82d3-49c4-8d41-1431442e96fc");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "2b481d0d-e4ee-4a1e-95c6-a15d41ef4d5e");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "91cc6d0c-b243-4fb7-84a4-4938beb40eb0");
        headers.add(header);
    }

    /**
     * Removes the given header.
     *
     * @param header the header to remove
     */
    public void removeHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "82d52175-2d8d-4efa-b681-b1aa4336b988");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "c2e1abbc-a336-455d-b9d3-5fc4ee731874");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "e274635c-adec-4c03-ad14-9183cac2923b");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "5e5b3c21-e32a-4984-aac0-d0e7cf252c55");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "64bc502f-bcbc-4384-bf82-a355406b27df");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "13d347bf-9516-458c-a89d-5a542f1e8f11");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "51b3eff7-d425-41fa-a53b-23b6a7745251");
            final Header current = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "0c634fea-6792-4ddb-876e-f9842bdfa05b");
            if (current.getName().equalsIgnoreCase(header.getName())) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "7b0da4d4-6297-430b-96ba-e110cba79531");
                this.headers.set(i, header);
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "02d9a7b5-c816-4561-8d8c-fdafa9fde3af");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "1605403d-9e20-46ea-8eef-727f3c778f86");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "026cacd0-d6cf-4731-95c7-dd5edc9cf0c9");
        clear();
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "0e0f97ab-e0f1-465a-8a32-7bebd1c0adba");
        if (headers == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "38741b4f-2373-481b-a939-27d8c4bc4c82");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "e008a751-83b5-4db5-843a-2ce4030e2400");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "3ca05290-088d-4291-b57b-365665150500");
        final Header[] hdrs = getHeaders(name);
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "7c99ecdd-0322-445a-bb80-cb72c676cac1");
        if (hdrs.length == 0) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "5790eb0d-120b-439c-bf45-608233fb614e");
            return null;
        } else if (hdrs.length == 1) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "cf1f91f4-3326-46dd-9a7c-046f26c44925");
            return hdrs[0];
        } else {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "1580c90b-9d1b-4aa8-ad4a-ffa31eaeccba");
            final CharArrayBuffer valueBuffer = new CharArrayBuffer(128);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "7284d45e-edb3-454f-922f-2642944bfd30");
            valueBuffer.append(hdrs[0].getValue());
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "9622e36d-1cbe-416b-89c9-0289704e7f6e");
            for (int i = 1; i < hdrs.length; i++) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "9e85b8b0-b7b6-4d7d-b510-87bcfeda5385");
                valueBuffer.append(", ");
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "1f5dc1dd-51a4-40f1-893b-e6b70b9f7f10");
                valueBuffer.append(hdrs[i].getValue());
            }
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "d3f5204e-9534-4fed-891c-5e3dc9e2b33d");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "17e0a5a9-fe7a-431d-92fc-d538bb3c99d6");
        List<Header> headersFound = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "89a974a7-ea4d-4cee-80be-3ae75ce04d4d");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "9b3add39-e354-4a6e-b8d3-fe3764356015");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "46520c1c-0408-402c-b0df-6f31c89110ec");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "14386c6d-1560-4614-970b-3a31e2c5060a");
                if (headersFound == null) {
                    writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "b0dddbbb-e118-4a9d-9b3d-2d27b9b3ba48");
                    headersFound = new ArrayList<Header>();
                }
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "8e1e51a1-3945-4ce9-8eb9-8172d69365ab");
                headersFound.add(header);
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "53d418d9-b3fc-46cb-a566-dc014c85f222");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "f8cee459-fb79-4b20-9e05-93e01f03e707");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "83ea57bd-7bbe-4b2f-b533-e20ae6bc7f8a");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "df7cc037-0636-471e-8680-d76b563bc208");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "0b9111b8-03df-489b-8347-b889e4eaa52f");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "59d57709-f3fe-467d-8791-8f36b554050e");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "5c7c40bb-162d-40c3-8dc0-c7e0939e5a1a");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "bb7f62d1-478e-404e-a3bb-86dbc14997f1");
        Header singleHeader = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "c727307e-06f2-4f27-87f6-af713a9f3330");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "412ca1b0-6fb3-4051-b80c-7a779f1e8ab9");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "6a0f0fee-43e6-4a35-9b06-9992c9ab8426");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "315ce3b4-062f-4deb-9759-9c7d567cca5c");
                singleHeader = header;
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "c4a224d3-d15f-4ceb-a066-fe1bddaa751a");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "208339e0-f14d-498b-b186-1e9cf7443365");
        if (count > 1) {
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "199ccc4a-f4ff-443e-9a5a-bc13a28a702f");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "ce9adbb1-79c9-4780-9fdd-c3a0584ddedc");
        for (int i = headers.size() - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "11f4ac52-5ed0-43f3-9012-24a1125286ff");
            final Header header = headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "5cfb1565-fc07-44dc-a465-8f23515ca45f");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "3a16cd4e-be1f-4d40-ae67-f7d06e001685");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "762bfe3b-5dbe-43b3-8fba-c9e8e466edcd");
        return null;
    }

    /**
     * Gets all of the headers contained within this group.
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getAllHeaders() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "7f2db037-dbe0-4a02-b26b-0d2ed7d62093");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "9503d625-06ae-43a1-aab7-3aee97c957a2");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "24494c41-a5da-48c2-be8e-6405e9ebf931");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "438453b5-b8a9-42b0-b6d9-06cce2889a02");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "774212d4-b916-4a85-9726-1700eebc0569");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "cbcdf2b0-2cb4-46ba-ac4e-f42208ff3d42");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "cfe2772c-dbe4-4403-8f7d-aa49e02a7583");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "eddda092-bb71-4e9d-b0ec-9f8d2cc0d839");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "9843e8a9-488b-4137-ac78-4dc5f75853ab");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "8bbac479-9af2-431c-a87a-e6c53f6b4498");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "85a60a08-2698-4a31-8e4a-f75f145ada7c");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "8f6256c4-06ab-4538-bb12-59b98d9a95d9");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "b7b460c4-e23d-429a-9848-dadad7fea5b4");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "4b45e7d5-1704-449b-9572-838d49aff22a");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "6fd097f6-368b-4301-bbf5-1a2ba640c8e7");
        if (name == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "6abd12b9-a9a9-472b-888a-a7d2482591e6");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "5f5e76e3-4dad-46b9-a987-3a33fd68e4e1");
        for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "c9f72aed-38d8-4b96-aceb-ad720d9743ee");
            final Header header = i.next();
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "22962207-8e6f-48fd-a938-772d6ba35664");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "b5ca7514-ee52-4aa6-9eba-2b96cb6beb89");
                i.remove();
            }
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_6_10.coverage", "816fefec-81c4-4326-9446-1ff9ac94ff83");
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
