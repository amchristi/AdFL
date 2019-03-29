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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "7295cf7b-9bb0-4ac3-b084-924c5dcafe01");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "addc6411-dfa3-4ac1-b696-fde131cff0dc");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "ba6bbc49-56b0-48b8-94a5-52ca92d4ee20");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "4818ac83-0566-4ef5-85c3-cd924cd7eded");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "255ee7e0-5171-42e9-8e58-e4d387275256");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "0fb539f3-aa17-429d-8781-79f556453f2c");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "1c527663-bc91-41b1-9829-2b412c51ddef");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "860da2af-9c58-443b-812e-d8e9d9d5f685");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "333159ad-9271-49c5-a736-f13c06e4224b");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "ce31425b-777a-48b6-8efe-5fcd4d921a37");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "16f2d5a7-e38e-4c6f-ae04-3dc8e7518432");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "595416bd-0167-433b-8ea3-9ea5a056bffe");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "82874bf4-a22a-4caa-8c42-4f0f538a6fbc");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "f06842ce-36aa-479b-9ba2-e83708d11471");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "0eb1d190-683b-426d-b7e3-e2b6cd9ed58b");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "05e8ef11-d279-4366-a886-047e9c10c18f");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "a3b85fc6-37c1-41d8-88d3-7a76c37c9f9f");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "13828096-1a0d-428f-ae59-9f99dda142f8");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "0065a35c-5304-49f4-a10c-ae2163015922");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "23579edb-368c-49c9-bccf-29867eb54186");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "4f80690d-a699-4545-a4a8-64d67c5d4890");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "4d95df2c-0b71-4c8c-85c0-f23807f0af39");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "6bbff6ed-c16d-4b4d-98a6-1c3d63dd6750");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "e1aaa243-b99d-4f97-8bc3-7a50ec5d7a83");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "02517d77-066e-43cc-bd45-e142d9977726");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "f51573ee-3b43-4dfa-a8e2-9dbb2d4cd88f");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "f8b33d5f-30cf-4747-8ace-5357ec60d609");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "cac2b992-a155-49f5-9b61-0bc52a92fa69");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "86796798-e533-42f2-94f9-b502fc46f66c");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "e4bb27ab-df50-4925-a7b5-b9327723f660");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "1e7e717c-9d8f-489a-be25-8f632ccd4d33");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "3126b656-358e-4f8d-8ef8-93f7addac8d7");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "1975823d-61a2-4490-80cc-84077002a1d9");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "67c507ae-df22-43c4-9180-550c1bbba449");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "d51cedb2-0a1a-4d4a-bfc4-79d8be4a6515");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "52f8a011-6dc7-485e-bfb4-23f6afe97cb2");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "c6155c98-5788-428a-8169-8c60ba93f240");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "e3d4810e-f87c-489b-b822-294eae90f88d");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "16322321-5db6-40d8-85ad-a5323fc1a43d");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "4e641b3b-db1c-4524-a0db-7293ea0f99a0");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "35ccf10f-f0a4-4127-a930-31ad9222e64e");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "907ff20e-5ab5-4366-a824-e32ac7745541");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "657b9611-7919-45ca-8ec3-afbf0a7fa339");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "810f4583-f750-4619-b111-10b690f09470");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "b691059f-6215-4ae5-894c-75dda6fa9135");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "7e34d7a5-5317-4cdf-aa07-1ea16c3bc759");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "06c0cba5-d6a9-49bd-9a95-9f60e41079af");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "b0cdc786-1e8a-4fae-aac1-9e3eee4735ff");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "4311606a-4078-48c7-b773-0bb86fe5d922");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "a4bf376d-599a-43e5-a67b-7d3ceec68a30");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "a3b63fb2-370b-4b24-8da9-19c3482133c4");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "f1e86ca8-4ec5-44c0-9c39-baa92215ebc6");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "09be6ecc-ee99-4590-8457-e902726d6f7d");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "85c3b97d-403b-452d-ac9e-950e18957035");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "165737dc-c320-43e0-b5f1-7bcc4b84990f");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "2439039a-c1d8-4263-b2ad-d99bf523390d");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "fadc1e68-3414-41c8-b53d-1ad738933a13");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "dbee1c66-b00c-42e3-a92a-1409cc37768d");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "5d399346-7339-40bd-bd57-620f31726605");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "5fb87645-a824-4a9b-9395-69cbda69d5c1");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "0694de42-8bbb-41e0-b5bb-6dc8b734fc9d");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "e1a29565-c558-444d-83b7-630f5575863a");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "3da86d48-22ff-47c2-aa55-4b54373740f6");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "c2325443-3625-4cd2-977c-d483fae38a1c");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "40a499be-a9f0-4ec2-aea5-08722bfa1719");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "78bbca04-3cad-4d1d-8810-32c2d9ae4fdb");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "ceef7c74-2f57-4116-869c-b9bf6a4db3ef");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "d87e1868-da1d-4eba-aaed-d86cad7e908d");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "b826527c-dbf6-49a0-8b8a-4500b390d695");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "e0c4c303-1314-4325-9db8-852f00cd139b");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "cf83e632-37e5-439b-8765-c922da002fff");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "5506f820-6de8-48b6-918c-80863803560a");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "bf362754-aa25-495e-aa63-802acd740aeb");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_8_10.coverage", "02e07e6d-8537-4651-9150-5acd0deba518");
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
