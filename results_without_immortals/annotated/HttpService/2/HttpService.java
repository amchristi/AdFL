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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "95ae63ca-dbc6-4282-8510-f7c2c61049ff");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "9ea7a728-21cd-422d-a91a-21bf9e102487");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "7938adf6-182a-4e94-a7e5-3dddb1616764");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "af7e1d12-4624-4a80-8126-550403388b96");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "0a359c5f-74e7-4af3-a020-398b285666d7");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "d41cdf86-83c9-493e-a84c-72c4faa2d311");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "a9c048b6-6026-4032-9dd8-f6f4cdd964a5");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "87b30002-ecc0-4656-90fd-93a80fa55f81");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "2f5f82ca-f483-42c4-a527-9d690ef83484");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "deebbc4c-fadc-4ed6-b511-de880dfab826");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "cd46dd04-addf-4512-a2cc-9196418b6a15");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "0bf15b49-eac9-496b-92cf-b75fe36446ed");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "2e38c2fb-7a93-42b3-91bd-d73157e519d0");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "91a2b8f5-d9d9-4cd5-91d7-0da746d2157f");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "c99c91f1-3f12-4687-8375-bfd7431c0ccb");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "0cccdb31-944c-4af1-aade-0c8bdccb0337");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "8ccd69c5-5711-4783-9502-140623ca18c1");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "5cff92ef-fd62-46c9-9d6b-48db4109989c");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "461f6495-75cb-4266-95c5-26187350ef9b");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "110db4f8-1d0e-4497-a63b-5f65fe8d1bd7");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "77461713-deba-4f46-8766-8595232462b1");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "9154d94a-ebc6-4903-96ce-e1cb3f220f5e");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "13e77d9a-5901-4cd6-81b2-884a43697b95");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "995ab40a-4725-405d-b220-d4cdab830050");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "178f046b-056f-47b6-9738-666b0d8db341");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "c8b41b1a-c2b7-4e4e-8e59-dd63d156b4b6");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "cd6db8f0-a394-4a94-8a27-f87d60c33b02");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "2aa997d3-6a58-4f96-b904-72d9954894dc");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "1fd6c6f1-4cbb-41c2-ac64-47d86dce4203");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "f5ba49f9-ef98-43b3-9708-5f5a34c681fc");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "743bb3d5-0e10-45f7-881a-004e62eb845a");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "4a5436f2-5517-47b7-939c-9584f7017beb");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "c7b2ae16-21bb-4fc8-8cf7-e6f6bdeaa6fd");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "1c1348ff-22ed-4902-a20c-4902fa66581b");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "09d3db6a-3070-493c-9b09-8d9bf8172413");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "553ef52b-6b36-4e2c-a848-803acc80435a");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "f5cd471e-b961-4298-870a-73a0b908bf72");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "123a2fba-5cc6-4450-babb-cae42b81d4b6");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "c7777a9f-b031-47d9-9d2f-0147dc51f18f");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "aadbc272-d6a5-42e0-91a1-b89b6487976b");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "adf6d27c-34e2-4d2b-bc6f-6a6684fa5f5c");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "455ad6d5-c95b-4acd-926c-403a9006debe");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "49ce5d1c-410d-4129-ab09-7fe2189deac9");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "c18471e2-6dea-4725-8b76-dda7f372f89c");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "87b312ba-3c73-44cd-82dd-98ec06959b47");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "12573a79-a7af-40f2-b176-abd7361d7a26");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "e9cea2a7-735c-43fc-8921-a4707451eb10");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "839dfb1a-e1c3-4e75-a4c3-c4d07167b5a5");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "64ea3891-f1d8-48ab-96d3-24756f79cec4");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "0f1a0fa8-1042-4ecc-9d9b-a2d472f4962c");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "a35fbf25-6d9c-47af-9664-12770cd61262");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "2a3e0cd4-5e5a-44f4-880c-6b0901656e26");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "4e9db654-609c-456a-a125-576e5d6dde8e");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "7856f1e3-19bd-4033-b1d2-ad0761e6a315");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "900c32c4-2ed4-47cf-a0d6-28d221922a8f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "061b2738-3a10-491b-a1bd-4b073e707279");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "3a9c6c09-5c44-406e-9b29-82b053afa5cd");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "0703fd82-81ad-4cd9-a9f7-ded281b61fd9");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "83391900-ef8e-4f88-9f9f-f7cbb874fdf5");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "bbfea8c4-a807-4abd-abe9-588cf29ddbdf");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "04d40198-f3da-45c4-a856-fdd387f26eb9");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "423350d6-53b0-45d2-8968-1d74a034f28c");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "2e6cb335-3b6f-465a-8a03-e6f72b3e5a43");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "0119fab3-16b2-48cb-8b7a-e808924a0a33");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "a4874539-3863-4eed-954b-771c061aa308");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "481ff473-a727-4d6c-a4b1-fc053ea4a2b7");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "68ca1d04-a8cd-4779-8324-f167e2bfae8f");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "d90257e0-dd55-47cf-8028-5f148ee9bcaa");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "b98c3533-367d-4561-9255-cf1fd573010a");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "1502701d-53d9-4472-a8ed-47ecbec8bad5");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "d10e4abe-2332-4dc5-a31f-3b01f689da4b");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "ee6ad578-c17f-4ead-9c19-d0a6ddb4132c");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "48a2489e-9b76-4768-a264-e8470455b14c");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_2_10.coverage", "ef45e796-095e-469d-9d5b-3d66968ff601");
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
