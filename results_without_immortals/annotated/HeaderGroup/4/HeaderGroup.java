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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "ea5d976f-e305-4846-aa76-3dc6ee3f6aff");
        headers.clear();
    }

    /**
     * Adds the given header to the group.  The order in which this header was
     * added is preserved.
     *
     * @param header the header to add
     */
    public void addHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "5f3c547d-5b0a-41d2-bb98-93c2e3968be9");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "290b1a9f-eb5e-4fa3-b3f9-293966b9df5a");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "67555115-b444-44b7-9df7-65691ce26903");
        headers.add(header);
    }

    /**
     * Removes the given header.
     *
     * @param header the header to remove
     */
    public void removeHeader(final Header header) {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "579d492d-014d-40a5-a1ac-22260873c6b6");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "15710d87-9b18-4feb-a7bd-c578c7cd7af0");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "cb9dfa4c-7f86-4ce6-b540-b4019491f61d");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "29b8261c-176e-43b7-8790-7d96446c7e45");
        if (header == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "6ff955d1-ef48-4539-a4e6-746270dfd01e");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "4bcf5953-551a-42dd-90aa-f7fa83abfe30");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "518085d0-4721-4cea-bf82-16a0cb0c662d");
            final Header current = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "4cce2112-12fd-4af1-b8e6-f24b777b969f");
            if (current.getName().equalsIgnoreCase(header.getName())) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "63d2fbf8-dbae-49bb-93e5-cdb274542af2");
                this.headers.set(i, header);
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "90337fdc-c880-468c-988d-641104c5eee9");
                return;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "09af07ae-0a34-47e2-8519-d009f8700134");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "6f1e0817-bbb7-44be-a634-fd29054efd88");
        clear();
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "d1119122-45b3-4cbb-96b4-de4acfcb3e91");
        if (headers == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "edb6aa45-c3ec-41d6-bb2a-7ff5dc5c56cc");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "e96e1bf0-18cd-411a-b3bd-5b52da44b80e");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "3d730747-b121-465d-8028-1c69a8964de8");
        final Header[] hdrs = getHeaders(name);
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "93cf2f35-3364-4683-bc80-a11cd7a1ed9d");
        if (hdrs.length == 0) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "a308b037-56f5-4eea-9ea7-040647262df7");
            return null;
        } else if (hdrs.length == 1) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "b02dc344-b97d-453a-9fd1-50942e30ad7e");
            return hdrs[0];
        } else {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "6f398cb0-25c3-481a-8684-ec3eaecb9344");
            final CharArrayBuffer valueBuffer = new CharArrayBuffer(128);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "0b6f4966-b2b1-408b-8195-035967904602");
            valueBuffer.append(hdrs[0].getValue());
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "b1058329-c9fe-444e-8fc2-4dd8ee927a8a");
            for (int i = 1; i < hdrs.length; i++) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "3feb7aa0-0b13-4265-a9e2-f1ea3263de11");
                valueBuffer.append(", ");
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "7ad9773c-2c68-4968-bb31-f5763d969292");
                valueBuffer.append(hdrs[i].getValue());
            }
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "ea57512d-4bc7-442f-9669-ca45cd890517");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "b62b0b57-463d-4ac6-9b48-7dcdae4c4f67");
        List<Header> headersFound = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "1930c48b-f61d-4147-85e8-454cc6c3946f");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "2914200a-d435-475c-9258-643534adc246");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "debf876d-69b4-486a-86d6-f4615eae6a5a");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "62f728bf-90a9-4c9d-9ed9-22447f6a2b2b");
                if (headersFound == null) {
                    writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "85a43f74-935b-42ae-8690-a45f72c2010f");
                    headersFound = new ArrayList<Header>();
                }
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "cdb46108-fa1b-49de-acc1-c4c80092d418");
                headersFound.add(header);
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "fe117f8b-eac4-4bd3-bd50-d9ec0016099a");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "c687ed28-e6ba-4e7a-9628-e4005423c842");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "b6f58b72-0b06-4636-a9a1-8290036de52d");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "09d113f7-eee5-4dec-a536-1f13da22f4d7");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "3d6baa15-eec5-4afa-aa75-ec2f90093829");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "e68f9f27-2604-4315-9e00-dbed40d3129c");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "9a578fb7-72d5-46d0-a7f9-7876f94ff470");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "74c5ce68-d154-4cd0-b566-f23f509b53b3");
        Header singleHeader = null;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "651db07b-22d2-4669-80c0-8ce32252b75f");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "81a88392-d548-47e0-aa9d-b58307cd107d");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "d7e2b38c-274a-41b1-beda-2b26a3d4875f");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "8643ea30-afa6-4de4-8a63-01308622b4a8");
                singleHeader = header;
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "c5f13c3a-31ef-4c11-8b7b-0a87cefaad36");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "d448350e-90ae-4ecf-9d38-d69799fd810d");
        if (count > 1) {
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "eb6d5add-06d1-4ffd-a915-81d741d025aa");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "41b82518-4a35-49cb-94d7-08395ea34eb0");
        for (int i = headers.size() - 1; i >= 0; i--) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "b64877ee-534c-43b0-a894-b456aa04d14d");
            final Header header = headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "0eddd49d-1f0a-4eec-8e03-0be0e2470762");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "1e6ca81a-fe8a-4711-b712-9d376a740e65");
                return header;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "4901709e-ee8b-45a7-9aac-7df072b7df5a");
        return null;
    }

    /**
     * Gets all of the headers contained within this group.
     *
     * @return an array of length &ge; 0
     */
    @Override
    public Header[] getAllHeaders() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "7d75118d-d0cc-44ab-9f8c-44cd2933a843");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "901aac01-2987-47b7-a3c2-644743cab263");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "c91efc0a-3c93-422b-b7f3-7983852e6824");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "986ce827-e3a1-43fc-a981-2ad60ea3f827");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "ba293603-f360-4422-a355-d4996d653820");
                return true;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "1e356d23-3174-4b50-ab9f-c97ec1733c15");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "67d20489-7429-4dcf-8763-d7929e22a118");
        int count = 0;
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "0674aa38-a72a-4600-84cd-e8136bb45dff");
        for (int i = 0; i < this.headers.size(); i++) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "4ca74398-87f7-4795-a54e-501fa300e89b");
            final Header header = this.headers.get(i);
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "f31df384-a4c7-4c14-95b7-f756272f7de2");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "f594bdc9-0799-4916-bce6-b8ac63e3315d");
                count++;
            }
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "6e6f0b0e-2b67-4f1b-aa15-2f6ed020f572");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "99cbe76a-b542-4bc5-a5c8-de86b080ce02");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "a1f56809-48ee-4198-b2e1-3e5e9ad83400");
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
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "2a92f708-0366-4530-8f15-ff0160c7e72f");
        if (name == null) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "a35b65d6-2dc3-4989-9a62-8eae51f5c852");
            return;
        }
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "1e68ba58-8e34-4196-86b3-4b3d570a7e99");
        for (final Iterator<Header> i = headerIterator(); i.hasNext(); ) {
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "402040a7-0593-4970-ac45-35ebdfa4c3f6");
            final Header header = i.next();
            writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "90353c4d-4157-437c-bd6f-30a594c16b9d");
            if (header.getName().equalsIgnoreCase(name)) {
                writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "34883e86-c816-472f-ac16-edb643583392");
                i.remove();
            }
        }
    }

    @Override
    public String toString() {
        writeline("/home/ubuntu/results/coverage/HeaderGroup/HeaderGroup_4_10.coverage", "900edbd8-cd53-4922-9cff-d2aa11b82bcb");
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
