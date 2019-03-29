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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "94897607-e403-4490-8816-ce7f1d15b735");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "11161189-28b3-4361-9601-ea034fddad00");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "85d7c746-3bd5-44db-a7ae-4137dee93141");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "b1971a45-a5a5-4243-8b6c-67de80c99ed2");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "266da478-a472-441f-94dc-489ad782321e");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "92839f4e-c8ea-4c33-a0f9-182f63f6af42");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "9dfbc4d9-5ce3-4a82-87d0-e01cc56e7965");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "e59c1e92-feff-40fb-8cf8-7d0c2c2a4fef");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "36a99f94-bcaf-457e-b92e-3adea8fb0a9a");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "12f415c4-9b0e-4d27-9462-6e501296b087");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "50828c9d-e5c6-4e76-906f-9629e7bca1f9");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "487f10e5-c380-4abf-a4f0-90cfb61bfe07");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "6f2f5c28-2f54-4467-aa1e-e797debfb16a");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "f42c17f9-11a2-42a4-981e-ccb0d601fbe7");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "275a2a52-eb87-4385-bb45-baa9745aa652");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "5f8681a2-8c8d-4725-9ec3-5666430f9772");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "672274cd-e5fe-4ef7-b060-ac18b2c7d587");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "24f277f0-95da-4845-b6ea-e7490696ea1d");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "2c2f787b-2736-497d-b401-aeaffb65cfd2");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "e60e2f0d-d879-4df6-a781-35cb96dc043f");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "fa3bf0ac-6f31-495c-9bed-aac716c2f026");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "6e9297cd-5686-41bd-96a7-838e8a292dce");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "62bae28f-e39b-4dea-9a60-4381b5a433a7");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "6baa8a39-f503-4d6a-99f8-e496d8674341");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "156e74e7-9f7b-46ff-843e-7e0bc20b5ce2");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "ed1d611d-41f8-4b4b-a91c-a5bff66d2ed7");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "4c03c83a-c5f9-460c-86d1-40ad3468bd59");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "97d6def5-9dce-4f70-957f-eb32b036d001");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "2f8ac8d0-5d9a-4214-a763-48e1bcf3ec3b");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "d824ccfb-8e8d-42f4-8ec8-714f298c57bc");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "2e417a6b-98eb-4cdf-83fd-4a4e0520c936");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "e97d2748-d946-4f8a-b95c-110c802ec961");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "8abf6937-e915-44ce-942c-61fab1058b39");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "1115ce47-39e0-4d8d-a212-282034aa3814");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "eba437b4-1ea3-4d4d-a5dd-dd0578a31cc9");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "134363c6-3091-4cf9-a2e1-60c0ebeced94");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "46b27f02-5768-42a4-80e3-8caad01ab50e");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "e3564dcb-5823-4389-8d84-8c85d2dd526c");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "6c67bbd6-9ff8-4759-a1a2-1811c7cacb41");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "a239b5f3-415b-423a-82e8-0f916a26dabe");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "3be66e89-ce8d-4309-8e91-f10ca1d95ca6");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "e965fdb4-4cff-4080-931c-0978adb302e8");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "1a48875b-f298-46c6-bdf5-ab9630aeed82");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "2a6ee922-4bdc-4752-b677-3addd5607708");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "0ff132b9-1066-4312-8c56-ded7153331f8");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "f2356829-6beb-41de-b47d-75080110423c");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "7e9abc8a-40b9-4970-9855-fa049f91cf42");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "457621f7-56de-4e68-86c9-eb4c06e22184");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "2fc7d23c-78a7-4677-a109-2c1a77821d54");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "c33eb086-d701-4195-8293-1df00bea952d");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "c91a7be0-59dc-4ef4-9ae2-78fa3cfb27d0");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "1b867224-b6c9-42cb-b413-0663ff6fbc81");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "04ff4cc3-9a3d-4705-bb6f-7e1ceaa4af40");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "97153866-4ee3-4ac4-acb5-eefc66a82990");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "52bfab00-87e7-4a25-98f7-60d73deef3a7");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "24b4f59d-8c55-4e7c-9fb5-c34eb7a24d23");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "f25ae1d7-fcb1-4af1-a02d-86c894b4bd60");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "01bd61ae-8610-4656-bc7d-b23e84452266");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "d5d818a0-4d07-4d3b-85b0-6f80e3ea0575");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "5cf16fa3-114a-45fc-8d00-ec51184eacd5");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "0b09c7cd-6668-40c9-8dd3-bd76ab86e90a");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "0b408c1f-53d1-4071-9320-939c6f97d103");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "efec7a14-5bba-4a7a-8861-154c95658fe4");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "681e1a27-1d82-4f9e-ba7f-c2e95ee29b5c");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "9dd2ba19-115e-472b-9b33-215c655502ae");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "cf59eac2-be23-42b3-bc52-dc24887bf343");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "574e4d2e-118a-471b-9ee2-77467e28c6fa");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "020567ab-a86a-4173-92f1-db0307a7360d");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "2c3e9ca3-630e-4670-b86a-3881d8f2c235");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "c15f6da7-6ae8-46a4-ba2f-cc7718b440c8");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "a6845855-55cd-42d1-ad0a-0ba6608e0e1a");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "676b59b7-d2ce-4391-a5f9-1301f8475676");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "b4205759-f20f-4f41-8b34-3bb879808da9");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_4_10.coverage", "765cc3ac-fabd-40c7-98e3-0cf57c9a9f3f");
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
