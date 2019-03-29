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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "a373f585-4699-4102-ae1e-1649f953a57a");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "ded7ebd1-c6e0-4b5c-a802-2adfb0440255");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "974c98c1-eef8-48ce-9c92-9d0755563582");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "66616473-eca2-4911-81f9-2fa724ccfb2c");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "196afc84-7586-48fa-aa97-fb828ba79287");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "7fa3be5c-c84e-4cc8-b7ca-368a44c77117");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "c4ba4a67-f6eb-43de-a233-ccd63af73526");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "9156a9d7-f00c-4f06-a4e4-f8f6ab77db4d");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "0f9be0eb-76e9-467e-9866-37c6d685a702");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "63e4d4ec-31d0-4da3-9bc5-0cbd1fe5dfaa");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "68ea270d-2d9a-4e9a-bae1-0e43fa55b9a2");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "c3e083bf-66ca-4b80-a530-ac8d9cdc761b");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "eb8d55e9-21c2-432a-8cea-54b8c2f0e3cb");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "bdf2502f-6ff3-404a-8707-8a78583c9dd3");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "2fb8820d-5b6f-4320-ac05-81b25061bb51");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "a0c86830-8824-4e5e-9467-26990c497ca4");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "4a02b514-0091-4a28-a734-48798bef8824");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "4aacfba8-33e8-4395-b8e9-40a6722f335c");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "e5edd268-1000-4fdc-a952-982c01436eff");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "e57c92da-97a6-482a-a49c-694c18cb054e");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "34d6a40b-5a05-4874-81a5-21fa9a5fd681");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "4fdcff0a-2019-4598-9d8c-cc24adf062c0");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "5bde783a-cc9c-4d5e-9019-e0b138eb65af");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "8f870e49-d90b-4067-b811-b2e334e03364");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "92f23170-c90e-4a53-bd0a-690d681f934a");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "b5c9913f-6449-4f19-bd5a-36c5a774d7cd");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "3b52f00d-c849-4efb-8557-644d2f00173e");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "60329464-cc65-4b78-8937-48da85c467ea");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "42111f99-ba25-4441-b4e1-12f2ae5ba39e");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "80168db9-fd9d-4fb7-bba3-b62ac4bcb74b");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "b3b9472d-d08d-49f1-944d-c2737e2f6865");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "15fc2d8d-721b-46c9-99f4-beb59fdee1ed");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "8b04385c-da0e-43c9-8d14-15da85d3bc88");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "daab6dbc-5eef-4797-8872-68d96d88e195");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "0df6179c-2524-4ac1-b68b-c8031d98f94f");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "04d8d9ad-31b5-447b-b2b7-ce97806aceb3");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "e92b733c-ae81-42a9-b6bd-e42b9ed2c3f3");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "539fe636-0d96-4348-b8aa-08538d324d24");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "358d37f0-2e77-4ff3-926d-2cba968289f1");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "574ae856-6808-4371-b350-b7f8013d6205");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "25335cfc-59ad-430c-9a2a-437929a80297");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "0cb7b8fa-1779-4d1a-becc-488c95ff130f");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "cf42acaf-7457-4dae-8816-6d063a1852ae");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "aebdb38a-6159-431e-aef6-13f0e7e77d7f");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "2bffd79f-df81-405f-b743-672f44fd5478");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "d7e06d79-8efc-46fd-80fc-2cebd397b9de");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "3d2f6530-479d-48a1-b209-48122b053f7b");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "bf59057b-6d90-4db5-ae71-3a8bd2991acb");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "6442601c-7964-49d0-af27-229cd469ba2f");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "96ca3d02-39ff-4530-a584-2f8265cdfd91");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "2b8a2ac5-7634-4349-ab92-bccb6fa9ebbf");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "d97bbf4e-0518-46a5-bd15-a591ac6c7065");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "a89d74d6-5970-498c-99c7-556011d2693e");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "9a9d6537-99f0-49ac-9195-15a76a2c7e32");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "7b81e416-a373-4066-bdec-39b8175db91b");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "af04b08d-c90e-4c6f-92f7-b1be95fbef2f");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "5c29965f-f18b-47f8-986f-fb5ba2c3a7e3");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "a5762a63-9684-406e-92d0-37aad9b61639");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "a8c31300-2412-493a-82a5-738b4195fc23");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "216520d8-649b-4da8-bc8f-99683dfe5abd");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "e2ae2591-5ed7-43a5-bb34-f1a7f91bb7fd");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "bb1d610f-90e0-4a87-be3e-5ee2ef875b00");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "05bd20a1-0f28-4197-ab0d-a151e2a7b0e5");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "ebd95b73-cfcb-4dde-b995-42207d7be18e");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "e9a31032-5389-46b1-9d6a-95169e255987");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "a5727021-6908-4936-ab54-fdb7674d8ad1");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "17636c88-cf79-4e74-901e-5658a8b0c005");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "6d53cc7f-9085-4e09-8810-04cfba73a7ce");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "ddda0044-b9d0-44d9-a47d-d08f33ab6889");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "be9ee6de-e371-4810-9412-8db565119972");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "947bca15-b6e6-42f5-a7d4-9102c6e8f798");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "150285cb-f96f-4e43-b4fd-580a2488815c");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "a6d78a74-558d-4b0c-9756-0a25b1922cdd");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_6_10.coverage", "80304fab-a7a8-45df-b834-95a5c011daec");
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
