package org.apache.hc.core5.http.impl.io;

import java.io.IOException;
import java.io.InputStream;
import org.apache.hc.core5.annotation.Contract;
import org.apache.hc.core5.annotation.ThreadingBehavior;
import org.apache.hc.core5.http.ClassicHttpRequest;
import org.apache.hc.core5.http.ClassicHttpResponse;
import org.apache.hc.core5.http.ConnectionReuseStrategy;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.Header;
import org.apache.hc.core5.http.HttpEntity;
import org.apache.hc.core5.http.HttpException;
import org.apache.hc.core5.http.HttpHeaders;
import org.apache.hc.core5.http.HttpResponseFactory;
import org.apache.hc.core5.http.HttpStatus;
import org.apache.hc.core5.http.MethodNotSupportedException;
import org.apache.hc.core5.http.NotImplementedException;
import org.apache.hc.core5.http.ProtocolException;
import org.apache.hc.core5.http.ProtocolVersion;
import org.apache.hc.core5.http.UnsupportedHttpVersionException;
import org.apache.hc.core5.http.impl.DefaultConnectionReuseStrategy;
import org.apache.hc.core5.http.impl.Http1StreamListener;
import org.apache.hc.core5.http.io.HttpExpectationVerifier;
import org.apache.hc.core5.http.io.HttpRequestHandler;
import org.apache.hc.core5.http.io.HttpRequestHandlerMapper;
import org.apache.hc.core5.http.io.HttpServerConnection;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.protocol.HttpContext;
import org.apache.hc.core5.http.protocol.HttpCoreContext;
import org.apache.hc.core5.http.protocol.HttpProcessor;
import org.apache.hc.core5.util.Args;
import java.io.*;

/**
 * {@code HttpService} is a server side HTTP protocol handler based on
 * the classic (blocking) I/O model.
 * <p>
 * {@code HttpService} relies on {@link HttpProcessor} to generate mandatory
 * protocol headers for all outgoing messages and apply common, cross-cutting
 * message transformations to all incoming and outgoing messages, whereas
 * individual {@link HttpRequestHandler}s are expected to implement
 * application specific content generation and processing.
 * <p>
 * {@code HttpService} uses {@link HttpRequestHandlerMapper} to map
 * matching request handler for a particular request URI of an incoming HTTP
 * request.
 * <p>
 * {@code HttpService} can use optional {@link HttpExpectationVerifier}
 * to ensure that incoming requests meet server's expectations.
 *
 * @since 4.0
 */
@Contract(threading = ThreadingBehavior.IMMUTABLE_CONDITIONAL)
public class HttpService {

    private final HttpProcessor processor;

    private final HttpRequestHandlerMapper handlerMapper;

    private final ConnectionReuseStrategy connReuseStrategy;

    private final HttpResponseFactory<ClassicHttpResponse> responseFactory;

    private final HttpExpectationVerifier expectationVerifier;

    private final Http1StreamListener streamListener;

    /**
     * Create a new HTTP service.
     *
     * @param processor the processor to use on requests and responses
     * @param connReuseStrategy the connection reuse strategy. If {@code null}
     *   {@link DefaultConnectionReuseStrategy#INSTANCE} will be used.
     * @param responseFactory  the response factory. If {@code null}
     *   {@link DefaultClassicHttpResponseFactory#INSTANCE} will be used.
     * @param handlerMapper  the handler mapper. May be null.
     * @param expectationVerifier the expectation verifier. May be null.
     *
     * @since 4.3
     */
    public HttpService(final HttpProcessor processor, final ConnectionReuseStrategy connReuseStrategy, final HttpResponseFactory<ClassicHttpResponse> responseFactory, final HttpRequestHandlerMapper handlerMapper, final HttpExpectationVerifier expectationVerifier, final Http1StreamListener streamListener) {
        super();
        this.processor = Args.notNull(processor, "HTTP processor");
        this.connReuseStrategy = connReuseStrategy != null ? connReuseStrategy : DefaultConnectionReuseStrategy.INSTANCE;
        this.responseFactory = responseFactory != null ? responseFactory : DefaultClassicHttpResponseFactory.INSTANCE;
        this.handlerMapper = handlerMapper;
        this.expectationVerifier = expectationVerifier;
        this.streamListener = streamListener;
    }

    /**
     * Create a new HTTP service.
     *
     * @param processor the processor to use on requests and responses
     * @param connReuseStrategy the connection reuse strategy. If {@code null}
     *   {@link DefaultConnectionReuseStrategy#INSTANCE} will be used.
     * @param responseFactory  the response factory. If {@code null}
     *   {@link DefaultClassicHttpResponseFactory#INSTANCE} will be used.
     * @param handlerMapper  the handler mapper. May be null.
     *
     * @since 4.3
     */
    public HttpService(final HttpProcessor processor, final ConnectionReuseStrategy connReuseStrategy, final HttpResponseFactory<ClassicHttpResponse> responseFactory, final HttpRequestHandlerMapper handlerMapper) {
        this(processor, connReuseStrategy, responseFactory, handlerMapper, null, null);
    }

    /**
     * Create a new HTTP service.
     *
     * @param processor the processor to use on requests and responses
     * @param handlerMapper  the handler mapper. May be null.
     *
     * @since 4.3
     */
    public HttpService(final HttpProcessor processor, final HttpRequestHandlerMapper handlerMapper) {
        this(processor, null, null, handlerMapper);
    }

    /**
     * Handles receives one HTTP request over the given connection within the
     * given execution context and sends a response back to the client.
     *
     * @param conn the active connection to the client
     * @param context the actual execution context.
     * @throws IOException in case of an I/O error.
     * @throws HttpException in case of HTTP protocol violation or a processing
     *   problem.
     */
    public void handleRequest(final HttpServerConnection conn, final HttpContext context) throws IOException, HttpException {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "5fa59980-7ddc-4468-93cd-afe41f2ca39e");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "bee3fdac-b858-4f2f-9de9-8e99cef91465");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "8e9b36e0-6574-477d-acb0-2df92162409b");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "0315c82f-9075-4e44-beea-82c4281b05b2");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "8ed22821-65be-43dd-99f1-0cc06e3f7992");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "45504bd9-f418-41b6-89fa-3345aab6f554");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "a6f35fcc-f1fe-452e-9440-e5664812117e");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "f0eafeb3-dca9-44d6-81d9-a50f24edd64f");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "b106ec6a-de30-43a0-8e97-c3dca3183529");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "65d0e376-78ae-43ac-8299-8c3feb645b25");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "2267b7e0-dc94-4f78-a1ba-c45297ae72d0");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "3d9ef595-df2d-42b9-a9e7-f707671c8e90");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "f7d25c82-d302-4b88-80ce-9dd5d8c86f29");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "a5efe55b-00c4-4be8-ac47-8c870257f545");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "07c6e0c3-729c-4c21-b05e-ea8ca4df5804");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "cbab2f7c-1b51-4ca9-bfdd-f1b9ae29491d");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "9c04a2ff-ecc7-4c8a-ab4e-fd367f1405a4");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "81dd064c-6fdd-4927-b4af-896f9a5de081");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "8b7e1e8e-5f5b-4f0d-90af-f29edce656af");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "0c98610c-67d9-4e30-a08c-da9839d90c80");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "1c10cde3-a362-436e-80de-0464fd535e93");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "0f2ac21f-33ed-4c21-ba43-498801c1ac4a");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "a22c9f39-a2e8-49e5-9e8a-ed6053335a35");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "232bb680-fd08-4996-8671-e7a64b75328f");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "1a4d2624-9fbe-4522-a6eb-5321954cdaf4");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "f39c8673-da05-4c0c-b299-16c1dcdf755c");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "4406500b-4c24-4abf-9f66-2a449fd5ea7d");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "a18a75c8-d107-43d8-b572-4c3e89521a96");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "b1576fb8-6d8a-40d4-8fda-2937b00836a0");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "cb8ff23f-2608-42e6-9971-f3cb9f451f61");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "27c2f03a-2057-40f5-8c7a-db924f7b2341");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "6466f664-d24c-4af6-8511-9b9f6d692d38");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "7b53906b-e0d6-481e-8620-199df34dad04");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "2d60978d-cd27-4021-bb5f-2633e710bd42");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "c831e09a-b05a-409e-9bf2-dc1f9e8604b1");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "d26f14c2-7611-498f-93e0-610c7acbf544");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "251ca6fd-aba5-40d5-b41a-ba7e866c800a");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "e8607852-ae3d-44ed-b055-99b32857ff7b");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "50d23fdc-7d4c-4a51-ab7e-b3865a2131d5");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "ef2e077f-6aff-4bda-89cf-328eb20a3e1d");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "557d75f8-6125-46df-b654-b5aa50f9549b");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "716f1f7a-8c03-44e0-a7de-cfcd2c190d3f");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "b2577c5e-5703-4d81-a43d-c0a497251963");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "0bcd3fd3-ae7c-4bed-8494-a22a5a1cf380");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "0380eb00-9e21-48d4-9234-3a00f412626c");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "9a85695b-25ce-4d41-a4d4-9504b1d51086");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "528850fd-5be1-4d39-a3bc-efc413974a73");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "1b8c2504-f05a-4a4e-9b00-54128e258a75");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "397641ab-b7b9-4708-8cbe-aba6ca1facea");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "05e14cac-b0db-41b8-aaba-79a2c9858b3e");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "000f568e-5ea4-49f1-a73d-da3122eb88cd");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "b7afc602-44dc-4362-9d48-13862f295f6d");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "6a9b7c8c-b76a-4c17-a269-d6c2c635b647");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "c05d5327-21ca-424e-844d-50a33b9382f2");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "349cfb1b-f1b1-4fcc-859a-25c3be961ea6");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "9e94b313-26b2-4e6a-950d-4c3cc0ed306d");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "54ade18e-988a-4dad-8f99-d20d9f67de2b");
        return status >= HttpStatus.SC_SUCCESS && status != HttpStatus.SC_NO_CONTENT && status != HttpStatus.SC_NOT_MODIFIED;
    }

    /**
     * Handles the given exception and generates an HTTP response to be sent
     * back to the client to inform about the exceptional condition encountered
     * in the course of the request processing.
     *
     * @param ex the exception.
     * @param response the HTTP response.
     */
    protected void handleException(final HttpException ex, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "93ec785c-53b5-4e7e-989a-e5c3028cd4f8");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "f11bb775-9165-45b6-bca9-3aa0612ce608");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "50e63c3e-d968-4ced-9015-79c288b1750b");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "1198e4e5-e195-4019-bf4f-be0280abe0b2");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "b1817dee-91ae-4d5a-ad36-2dbe9493cb00");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "f946e21e-81c3-4155-b0da-d9edec1677c7");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "9cdfe155-3272-4957-b083-b48005da8ed9");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "035a3ae9-049c-4f84-ac02-94f7649e3d5a");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "182924f8-0664-4f8d-a99a-93ce177d923f");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "3b4733c8-0135-4e8c-a384-d8bdac3f6fa3");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "f46cc347-ec96-4932-9582-15eb7bfbe66b");
        response.setEntity(entity);
    }

    /**
     * The default implementation of this method attempts to resolve an
     * {@link HttpRequestHandler} for the request URI of the given request
     * and, if found, executes its
     * {@link HttpRequestHandler#handle(ClassicHttpRequest, ClassicHttpResponse, HttpContext)}
     * method.
     * <p>
     * Super-classes can override this method in order to provide a custom
     * implementation of the request processing logic.
     *
     * @param request the HTTP request.
     * @param response the HTTP response.
     * @param context the execution context.
     * @throws IOException in case of an I/O error.
     * @throws HttpException in case of HTTP protocol violation or a processing
     *   problem.
     */
    protected void doService(final ClassicHttpRequest request, final ClassicHttpResponse response, final HttpContext context) throws HttpException, IOException {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "b10c109d-3098-4c72-91e9-4feaf171173d");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "ea8d4b7e-e288-49f7-a933-4f77be1f3a73");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "b04203a8-a5c1-4a79-8f9c-36f6708fb70a");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "4b6c1203-d497-4898-9ad9-ade25af883ea");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "7d22b948-2e00-4b75-805a-d25cc75bbba6");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_1_10.coverage", "14166a1f-4e6b-467a-8cf3-3cd8028b2ab4");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        }
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
