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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "8279e4ed-3126-45de-9d29-d8fd9b5085ac");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "1d51001f-f0ef-4a15-8fe1-66ba8ab3c022");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "1b736686-682f-4e75-b604-d4bee22c0874");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "77ecdb0b-0239-41f6-922d-9beef79ced4c");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "d6c37099-2b24-4600-a742-4a2b800ff7af");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "631f883d-1e6a-44d8-b881-51d195d8a584");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "f31d7ba1-e82b-4456-a2b4-f318374d5dd6");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "19217d3e-fff8-481f-a320-51180cf4e9ff");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "1e72d54a-7cad-497a-b3ff-668f5ccf53b0");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "04e36ab0-ad8a-4141-8389-0dde8fde697b");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "55e4040a-2325-46c8-8599-41640cf0123e");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "59758f01-49d4-498d-8bbe-6ec2dd89eb95");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "e0ea6d33-6035-4451-8526-1b62e199e6e9");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "bfcecf4c-febb-4d85-80d7-4b5e5247062d");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "9ec69972-011e-4afa-a020-52f6a0fc65bf");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "989dba6b-22b9-4cd2-9ed9-6e449c3782fd");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "ac7ffd9e-3bf5-436e-8d82-f0bbe90ef9b5");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "ece98fdb-ff6b-469e-b66e-f67cd650de51");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "f51f619f-cf23-4aa4-9b51-13443e66a712");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "2b55a661-85df-463a-a752-eb8b99f0558e");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "89d24de2-1fce-4fb6-ac35-73122f6d0689");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "b541928b-4822-4c32-a2d0-e1ef5251c029");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "7632caaf-0f67-4e10-99ea-db82593fd482");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "88145e60-9ba7-4815-aa23-d66e972131e9");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "1cdccc9a-d264-4d61-a92b-85546c3f09de");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "b2690b3f-8226-4cc5-984d-d93ac7047d3b");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "59d0d3be-6b6f-4b6b-85b7-f5118cf32bca");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "86fdfb13-6c0f-4a00-9e05-7cb1ec6c893e");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "4a28f5f6-972e-4255-a42c-eacac78371a2");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "ccb70b5b-3070-461d-b5aa-d57b8a9755fd");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "b8b05161-85ec-452b-a616-99376e325a75");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "f8e79260-1239-480a-925d-787a23896ed0");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "4d157406-3ffe-494e-a411-21ac592adcbf");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "12625e41-aa45-42e1-9190-eb1a5cdb75fa");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "b1f5f955-1264-4461-b118-f539617ae871");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "b8e5a258-471f-43bc-8c78-3e2bb58bd958");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "2c5558a8-325f-4c86-9117-58cf99f26612");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "65c67f73-a6af-4a3d-9005-cc093522d25c");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "96dd89ce-f126-4828-94a1-6400b2e8b689");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "427c3aed-61f5-48e8-a39f-e5da0915ee59");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "1a13ee4f-0a7e-43da-9623-c029dbed777f");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "c9d76617-5b3c-4416-99d8-16b2576fb1c6");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "51559862-710b-4d18-b439-964833e830a8");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "324a754c-a322-4ed0-8759-706a2a7e9096");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "9454beff-d630-4447-8336-99f03bd78d9b");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "36533049-ebc6-4f66-9684-dccfccf65163");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "427e5afe-51b7-4a02-809a-e4c0c2ef7c6c");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "63da49e1-bd97-4057-a881-0b03b42e6afa");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "1c7016ce-c660-430f-9bec-73369770093a");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "293941dc-ef55-4ca6-82f4-dc586735e12e");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "f07dd332-05c7-4338-93fa-c6f5a74d1a18");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "02be96d0-7287-4d20-8390-e892cd3713c0");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "0c3dc39c-496f-4ee3-8171-ff787a0d6cad");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "4834a063-f684-4114-af4b-37e3c08a4e2b");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "23ffd9e8-31aa-4695-aa9e-ff05c8fb23b3");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "b501e04b-a518-4049-ba56-273f145f67be");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "4a541c24-75e6-4092-9ff3-965c6fa7e034");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "a7bc0566-bb05-4e86-8800-2a4daf866476");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "53720bb5-408a-4ab3-b4ce-8984ff008c11");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "b0dc4b47-901c-4dee-be1f-de6cdc78b9cc");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "6ca143f5-366e-4fcf-bc33-95f0c259a7fa");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "ce16390b-51cd-4a28-bd44-d99552874a22");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "5ece5171-ce28-425c-9f43-4a0de1e86bf5");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "bfca1564-5426-41a0-9c6f-ccaed9ac8c9b");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "dfdd30a7-6fd1-49a2-af5f-ced6ea69589c");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "352454a8-c833-4a53-a474-9b3f5084ea15");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "df018f0b-65ce-487f-8f21-0fb9f3eb8536");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "37cff9e2-ed55-4435-8dfd-a7d4b0fbd6a8");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "06b2d5e2-2ae8-477f-a9da-82a7d31834b1");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "653104a5-3615-49bd-8856-9f72c4b2cf63");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "4931f5ef-68ab-4cc9-823d-8ee9efca89b6");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "396b67dd-0819-4339-a50f-c24071e81da6");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "e259eadb-b3b5-482e-b0d0-45ceea27a083");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_10_10.coverage", "8cfd1430-5bd2-4bf6-a9db-22d8a0f397bd");
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
