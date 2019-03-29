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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "a4e8ea4d-028a-4d96-8e70-be53bc4c2a27");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d05b1e25-2971-4bb8-88c9-821d1eed88ff");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "1a1adc7b-27ab-435b-bdd6-557e190fb2d5");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "fddc2dee-6956-4ac3-8f9d-0ee99c5b59ad");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "ca94dc33-6663-4103-b646-f192bcf5ab07");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "da858fcd-d561-41dd-b0e0-9ea9012e3c74");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d521ee5f-5f89-420c-9209-68de3d265489");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "3890053d-dbad-449b-9497-bafd8d04d251");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "57934bfc-2fbf-4148-be51-9faa56b6e80a");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "e75fa2ea-8e93-4c16-bbcc-f0fc5832d404");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "94179096-9b2f-4aaa-995d-51381909027b");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "7385e8d6-74d6-424a-8796-7dab99601274");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "4368c762-6d2e-414a-b4bd-01723d2dc859");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "69aea589-21dd-4b0d-8c73-47313c04fd54");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "a9d724df-2e8c-4add-b2c8-a6c1218ed3dc");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "f3cafff4-46bb-4b74-b859-69991670be34");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "543dd294-6772-436b-a2e3-c13dab85c18b");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "13d9ea23-0548-47c7-9ef1-c9d1394cd4e7");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d517e407-fda1-4950-9522-6df20d679eb0");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "9cfe4b08-5f7a-44c6-95f4-d39824444858");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "47a0a357-54e4-4835-bf2a-f8f6b91d19b2");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "4cad4360-2b2e-4864-b566-63b1c5a79e44");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "5ad348ce-0d7b-48db-be18-bf9883e00dbe");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "e3325224-25df-41bb-95a0-f21576974f46");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "2fc7cd83-b870-45c1-a2a5-6aaac91394af");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "228e185c-f402-49f4-8119-b373bd09b441");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "70d93070-96ea-4174-ad35-68af3b097ae8");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "ccf1fc46-6cff-4eb1-9ba3-72cb587e4aaf");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "bf0e5b17-5d2c-4b76-a03e-bf50d2da61d9");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "7b32a9cf-1671-4189-8513-17dfa35fbc11");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "b6c084b8-50a9-4fb5-a211-d366f7d2fa83");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "f7cdb419-1364-46c8-8e0c-5fd2fcfffa36");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "0eb1633a-fc2b-42a8-874e-b30ff03b669f");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "c85c7d00-e34d-45ba-a829-0db43b4a43b1");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "063a2178-cf45-4eeb-b5e4-b97143be9d59");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d95b95f2-2453-46a5-98b9-e19d9fb0f030");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "24b42383-8102-492c-9759-5ab3a420b574");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "b635b623-482e-4b38-a6b8-58fbe98cbd0c");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "8f0c77f7-2446-4823-819c-bed83d36f532");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "28b954f0-e83a-4019-97d2-14e1c32ee2a9");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "106b9b0d-403e-4154-9fe3-bcaf51498125");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "cc257350-013d-4746-acde-ee1046319d84");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "b04d3ded-a32e-4ceb-9071-67b618cd56ad");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "fdd85168-a4fa-42e3-a3dd-fb3d916a8dc4");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "e71f9876-62fd-46bf-bd23-b794eb6454c9");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "9881dee6-0d66-456b-879a-536a86ce4163");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "12aa0e99-fc8b-4bd3-8e21-6ea03c3f9d13");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "f0d94722-d5ad-4bb7-bc29-a86cd99bb1c9");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "ea981ea0-c486-4ec8-a969-7cc3644d9f5a");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "87b8a412-cb65-4dd3-936d-9bdbc0235851");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "cc588bdc-c49b-49ca-b881-b58af3942295");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "03601942-1532-487f-8f4c-f65eb9e56a3a");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "69a0c7ba-de1f-4fdf-b627-c386386080b7");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "cd76c4cf-3975-4013-a515-edf050b1420d");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "813d9e15-7a54-420c-8332-ef5c6246ee0c");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d0382a93-9134-483e-99f3-3897f76a2080");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d4a8d0ea-bef1-4d0f-b5aa-529ec9918bec");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "886c70b0-5498-4b87-8c14-20a454f28255");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "e84dc200-bcf5-4b7e-96cd-fbc03af2f759");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "9a9a4d39-a7ce-4812-9ced-9c6f981b279c");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "157a7a84-ca63-4f55-9ee7-04c14c7625c0");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "0078993c-b9fe-40da-9605-b90d276e9178");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "ac636428-2059-4420-9933-3c1cb0673383");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "771ed90c-12da-4a7e-8270-fb74368a04c6");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "78693636-70d9-4cc2-baca-1f8c54ae5e88");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "bff2a424-a697-43a6-b1e2-4aa58f8ec962");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "196422d9-5a71-419d-a5dd-80e93ea3090c");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "24631975-e76f-4a63-af57-8051d770d30a");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "1012ebd6-8931-4730-a878-9f3bdbbe6a44");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d97d1dd0-e982-4f05-beb0-43fd291a8070");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "8cbcefb3-fa6c-45d8-ab83-36e501443c68");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "d4b15939-a6fe-486e-852b-5a15d4c35b45");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "7d5e637b-84c5-4e30-bb8f-564d516e5508");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_5_10.coverage", "7d96651f-215b-42b3-898a-d1755171e2db");
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
