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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "febffdb6-44dc-4c18-a057-6f17d97fa571");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "bad0e34b-ebc9-4047-aeda-46f4f32008fb");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "44536b2b-a609-4fff-bbbc-e1b3010475d7");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "677cd51f-b2b2-4661-8782-f6d7d9a7e675");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "209faa0a-7a94-4d64-895b-931a70e18e1b");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "2a75e4e0-4198-4016-ab2b-17c3b1af384f");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "1a57f719-40fc-4306-a91f-7daebffccb8c");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "5b3b9fce-39d5-4913-9346-905072e69225");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "eac9f10d-0d3e-49df-a9b2-0feed282ca46");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "ca345d58-851c-456e-ba0e-398c7fdb3d9d");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "fc937ad3-2c7d-4fb0-ad1c-299275b7c0ba");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "69ea9a60-9aac-4433-8c07-75b5d1292a2b");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "ace19fc5-8d21-4bae-8dcb-486116935ba1");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "702caad9-1616-437e-bf7e-a83dcbc9ee32");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "5dd6f7bb-60c3-4ecf-b70e-1e4e416d7374");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "1e02d640-4f81-4da8-b393-789f8fac6918");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "881e2e7b-dac1-4d57-b3fe-776ad459e44a");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "00dabbb8-a994-4633-8d0e-22c838db969c");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "2ec84089-b9d9-4318-a3ef-fec46af7f8a8");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "a3b88329-d815-4085-872c-e531c54b5615");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "eff8c4ec-ca16-434c-bc99-3e2ffb3517c0");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "4a9cdff8-8251-400a-bf73-30261fdde740");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "5129bbb1-5b40-4997-87ec-ff74a657c386");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "4565f19b-8fcc-4bab-9952-7f60a4b7b6ce");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "2200d71e-7117-44fc-bd01-c568eb1ddd02");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "77085047-b286-4d4c-8899-141b644e1741");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "49beb421-9468-4219-83a6-283908e2ec64");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "429ba788-eb7b-43ba-9bdf-609eaeb846c3");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "3a282d00-f7cd-48cd-b650-d2e652230618");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "7cb00262-0dab-4dac-bab2-023a1b8a43c0");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "d3413c8d-fbe4-400d-95a0-4500bf4cb7d3");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "46cd231d-8265-4666-bdfe-e1a9179c673e");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "9b1ae768-645b-4d56-9ad9-3ab32e74e187");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "1865a12c-ca96-4993-83b1-8771fc851d6d");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "616c31ae-efdd-4bf6-a35b-4fbd1d3fceee");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "a03f4be6-7ad7-4a78-9e9a-67649d83750b");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "4c37a99f-c317-4314-b400-1e0f44506169");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "ee827116-7f2b-4ee0-a411-0c4ab2517fc6");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "e602040e-b467-4547-97a4-90995747007e");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "86704245-c9a4-47ec-a424-15af387a236b");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "44d6ed3b-225e-4eb1-9c15-8cd4fa7ba023");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "abd9d8a8-7c1b-474e-964d-4e3647d12360");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "ec9bc12e-b592-4a7c-b7fe-a5fca9d90af6");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "47397207-9e60-4629-a485-e844a6cbb9eb");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "9e20550e-a73c-4e8a-b656-2786593b479e");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "e27f5a8d-e02d-401c-97ef-f9b7b0bf9177");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "6c7a8a73-1394-455d-9bdb-55ba6918e39c");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "d99f339c-f934-42b2-b5ef-9ef6a6b88dbd");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "dfb9831b-8d24-4218-be0d-910d6cd65588");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "5ded9218-9270-4915-8ea6-1b3a6fc57995");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "e4f395af-8615-4f66-8645-52c6d6c97be9");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "131e764c-8bbd-49b2-95fb-59924f4bd038");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "2e30eb62-5244-41e2-97d3-9d0157c87184");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "f5b4f406-b461-4638-a48d-41c730f797b8");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "98a1d717-0d56-4b74-84a5-f8df9b3b6e61");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "24b66954-70ed-4938-a638-d41dbc74bf14");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "6ff6bada-d5a0-4025-ab07-b294c1d790e0");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "8ebcee22-2cc7-4dc6-8e1d-481477f8a92d");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "54e2d816-da14-4337-a114-7f42ae6ee9dc");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "899015ab-8817-44c7-b7dc-817eeee02792");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "b737570f-57c8-48ba-adf3-547d1e242dce");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "cee60e65-4c43-407b-8f73-c4da9568c778");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "23b44fb8-a76a-467c-b852-b7d445061430");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "b5a0ead4-f054-4b56-a512-2d50ff47dd9e");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "6659c5af-6116-4da4-965e-c44223fab4cb");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "5ee28d05-208f-4056-a4e4-8ead99c10b2b");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "6230f59d-e593-42a7-9dd8-b1103d093e59");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "c5fad030-fe6e-4403-996a-211bf4bd7858");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "b87ac052-f2fb-42ac-9358-e8af1cb855d8");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "cdf8e4f5-b347-4516-93c3-530ce1c1bf6d");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "79280b14-3fb4-4834-9681-da4418ddabc7");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "89e1e04b-8a0a-45db-81c7-3054dbe732d6");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "12144e75-db5e-40df-bee7-e4b6abf40dcd");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_9_10.coverage", "25da9e35-2e9a-40ec-9cfc-39332d309a31");
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
