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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "d4a17289-82e1-43d9-952d-fb2a6270150f");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "cd2061cf-6e35-4199-91ed-7653a490df43");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "47935d09-409e-49e1-a0c8-f66e4507ad47");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "6a950b9d-0f3c-47dc-bf2c-3c08a5f9f703");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "d30eed19-1af7-47df-ab07-2beaf0303579");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "91c99485-6200-433b-95e2-83ed2eb9e564");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "57e66adc-32db-4f61-b5b9-af0bb418ce55");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "c0dad9a0-589c-419d-b452-40de84838314");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "513e8567-65f3-4a57-8006-8bdf56364623");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "88349fbb-cc2b-410b-9312-c228b30deccc");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "7773e074-862f-4659-9a03-f497ca427b12");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "2dcb8272-dfd4-48aa-b2f6-5c5c13e25140");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "e23c3ef9-5df7-4bd2-b14b-b207e390fbf0");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "21b593f9-5626-4ec9-997f-e6ecf1a33eb4");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "78cbc37c-4150-4867-b55b-71a5ccb1bc77");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "e581e581-e124-4e45-a950-ce03ef22f833");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "d26b7369-e940-4747-9fb4-b91c05f88e45");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "69cb667d-b646-4a2f-acdf-7e5f97577052");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "16e151d3-4b4c-4892-91d7-4507abacc3fe");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "e28868e3-5f44-4d5a-8f27-962ed634b9b5");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "880c7aba-8b1a-46fb-acbe-a6e49150ddc1");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "207a2d6f-89fd-444f-bbb6-43dd23f5ba6a");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "97e701d9-9606-4bf7-b1e6-7525739355a9");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "1536cd26-abe9-4865-86da-7c694b149db8");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "16c57d68-4c8a-4ea6-9673-99611bd6d03c");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "7cc0169f-075f-4bf3-8669-e24d5ac8067c");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "beba5d7a-db3c-40a3-818c-6d956c388d0a");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "728cfec1-b7d9-4533-8c5f-b968b00094e5");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "c7c15640-b391-49d2-92cc-17f5c6354028");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "9b6d1646-0374-4cbc-9fa5-6f13368b3074");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "0b96adc2-3a3f-4fb9-ad03-3a21c068302b");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "1ceff8d1-017d-4607-a5f1-25029361d4bb");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "4b197bd1-2959-49f8-aec4-a41d4f4e8b94");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "6eda412e-0a60-4814-b8ad-aa030b827da0");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "2a703ed7-9c00-48d2-aa58-1b0adaea8884");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "4003d295-1a00-449c-8d25-ea78b009ac37");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "e0400707-e719-42b8-b5c5-361b242f88c4");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "f95375cd-a7a4-453c-84dd-6747364db429");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "c5150851-1be5-4d0a-a481-9344cd80d359");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "1e89976f-5b6b-414f-994c-2318a7200c18");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "56dff425-f18b-46e2-b795-484acaedf2e5");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "71712626-c70f-48a4-8dd1-c9126965b975");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "e4fd2383-a26b-4e86-bfd1-738dc55746d8");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "3c3b8a89-5310-451d-bde5-d962aad1d0f4");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "6432636d-7f7e-4d36-8177-ce86ec7321f7");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "1a2b9eee-d623-4a5c-97af-a386cf0b2134");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "87624dc7-fc98-4110-87c4-04db467887f0");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "cf7e8447-1dcd-41fd-96ad-d6a55895486c");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "7c467725-152b-45e7-8ef2-2cf9bd87032e");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "5e80d157-9286-4542-8e96-fe9605b7da69");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "61cf593d-a8be-44c4-b678-fb553c90e152");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "d04d7aa4-f1e4-469e-9ec8-6aa4dbbc9be3");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "45db0d82-a9f2-4ebf-9ec3-15afc2788262");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "5493e2b2-d3a4-4c0a-bf38-935f0dc5615d");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "7783e3f0-0818-4833-9556-2b4a90f94f18");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "2b3c2f87-d1c0-46fb-adf0-001c7cd75386");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "733710b6-960c-472b-be56-214b91be8731");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "9e9caa8f-6562-445d-a25f-4f995bf1c645");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "bd39067c-c891-43b6-a82c-f4091ef464ef");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "e2596cc3-b5c1-4a3d-8d63-bfb84b8838a3");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "144a4919-4d0a-4840-a5c6-8434232568b2");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "4588de90-b055-4502-bd21-0b394858fa81");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "df738869-bd88-4706-8740-f8ef11b48fa0");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "07e19392-815f-4ae5-a6dc-81c4086023a6");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "481959e4-0557-4841-a897-231b52f553d1");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "cc31db62-e463-421e-b390-247de1c311e1");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "039cb98c-981a-450d-a6d0-8ce56685bf06");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "72282326-6682-47d5-99c3-67336460c01b");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "35b22f1c-52d8-4ffa-9545-6fdffe6e53c6");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "230557da-c98b-4a45-9194-9768ba0a6b68");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "e3769b91-7058-48c9-9b8e-fe7bd72957f4");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "f8942f90-eaa5-468c-bdd6-6d99541a3dc5");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "6bdf057a-050c-44e5-bf41-dcf4d822dcc9");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_3_10.coverage", "27f6391c-7496-4b8c-a66e-bc191fad99bb");
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
