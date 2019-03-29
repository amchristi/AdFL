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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "a9645274-8e87-4d8b-acc3-6228ba5a6cb8");
        final ClassicHttpRequest request = conn.receiveRequestHeader();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "eb45ba77-a9c8-482e-be63-0db1b4678131");
        if (streamListener != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "5c421115-c843-42d5-83d3-fd0c6f4c3c03");
            streamListener.onRequestHead(conn, request);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "06981374-a8bb-48cb-8559-15f35b221286");
        ClassicHttpResponse response = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "314dd09b-f108-46dc-9bbe-3b1fea43b0e0");
        try {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "54c56221-4abf-4278-b04f-d12140aa5161");
            try {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "a869a6cf-29ce-4f9c-9d54-a00b46f5db65");
                conn.receiveRequestEntity(request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "22a68dbf-4bed-41a9-9ef4-0c47dedfa835");
                final ProtocolVersion transportVersion = request.getVersion();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "cd9035df-e04e-4a2f-83ca-be6321b8c071");
                if (transportVersion != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "9926181c-7af7-4a13-98ee-6bd44baf79c6");
                    context.setProtocolVersion(transportVersion);
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "7250c6a4-98d1-48fd-99df-6e2475290c7d");
                context.setAttribute(HttpCoreContext.SSL_SESSION, conn.getSSLSession());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "b4506656-789d-41b5-9dc9-0666672d5894");
                context.setAttribute(HttpCoreContext.CONNECTION_ENDPOINT, conn.getEndpointDetails());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "4ee33e3c-e3e5-4b8e-bb75-e50c19b01ad5");
                context.setAttribute(HttpCoreContext.HTTP_REQUEST, request);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "107d9d99-8c15-4e4a-80e7-d8c65ba3f0ac");
                this.processor.process(request, request.getEntity(), context);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "b25fc40f-d37a-4e5d-9a06-c3a66a51b81a");
                final Header expect = request.getFirstHeader(HttpHeaders.EXPECT);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "4232c8c1-aaf9-40a8-ad3a-f29e7ee18ed9");
                final boolean expectContinue = expect != null && "100-continue".equalsIgnoreCase(expect.getValue());
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "87ca2b92-8b3d-42e5-b573-4d6541f93fa3");
                if (expectContinue) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "9cb1b29e-45a9-49e0-a7df-f83321d933e5");
                    final ClassicHttpResponse ack = this.responseFactory.newHttpResponse(HttpStatus.SC_CONTINUE);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "cf1bc6d5-21a4-4c9e-b756-d8a161a8594b");
                    if (this.expectationVerifier != null) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "2adbdeca-6107-4f6d-90cd-e717d344e6fc");
                        this.expectationVerifier.verify(request, ack, context);
                    }
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "ab451ed0-57ab-4ffb-a2aa-85329b089629");
                    if (ack.getCode() < HttpStatus.SC_SUCCESS) {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "4dd90708-faa5-4cc1-a99a-eb02f254bf28");
                        conn.sendResponseHeader(ack);
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "fac7cf35-4a32-4811-8788-0d595c1a77b2");
                        if (streamListener != null) {
                            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "8da7301b-6113-4ea1-934c-d4867fbc2586");
                            streamListener.onResponseHead(conn, ack);
                        }
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "a8a62424-4085-4178-9521-1dcd25fb08ad");
                        conn.flush();
                    } else {
                        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "2c2ff4a4-c985-4ab3-b337-452eb8c2e501");
                        response = ack;
                    }
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "d52995c2-a8bc-4772-b1a6-9f9886dcc6ef");
                if (response == null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "5bb2928b-6db9-4447-be97-89fbdeeb18ff");
                    response = this.responseFactory.newHttpResponse(HttpStatus.SC_OK);
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "4995ba97-321d-412b-b0e3-ea9998453b55");
                    doService(request, response, context);
                }
            } catch (final HttpException ex) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "1ce8ffc4-3a20-40d2-92de-aa18651d17ef");
                if (response != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "8e310153-48f5-43ae-ba43-39175994692b");
                    response.close();
                }
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "7f45fd38-f9a7-4239-a550-71d9d9332b6e");
                response = this.responseFactory.newHttpResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR);
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "42174105-7526-4864-9eca-8e481338e59a");
                handleException(ex, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "11588ff5-c84a-4926-8ed2-9b0606425c75");
            context.setAttribute(HttpCoreContext.HTTP_RESPONSE, response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "b9a0e748-65df-4565-86b3-d200510e1aed");
            this.processor.process(response, response.getEntity(), context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "5b93b630-3682-4e2d-a21a-eafcfe2c8c0e");
            conn.sendResponseHeader(response);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "6ae338b1-c4b2-4df4-9102-58e54ffcf551");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "8e46885a-42d6-40bc-9227-55269832c383");
                streamListener.onResponseHead(conn, response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "4022f903-ba54-4264-9ea0-bf25bfe80b7f");
            if (canResponseHaveBody(request, response)) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "0474c117-fa7c-4985-912d-bbdf118042b4");
                conn.sendResponseEntity(response);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "38739e40-36cf-4ac3-9e5b-cfc0fd2e461a");
            conn.flush();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "7b19158a-3528-4eb9-9352-57d71bca9c8b");
            final HttpEntity entity = request.getEntity();
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "88b799f1-8d39-4cb0-87b9-3f694d65c485");
            if (entity != null && entity.isStreaming()) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "e8eeefd9-4c97-412d-9ead-a09cbf4abfd2");
                final InputStream instream = entity.getContent();
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "a049a098-af4a-4010-b1d6-46033c28bf4a");
                if (instream != null) {
                    writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "69fab521-abb6-4384-90b9-337147191f05");
                    instream.close();
                }
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "84e03bdd-e4f8-4929-88ac-ea414a66bd07");
            final boolean keepAlive = this.connReuseStrategy.keepAlive(request, response, context);
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "55ad1695-b9f0-40ba-81cb-1e0f7b97d939");
            if (streamListener != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "3b489ccf-5e65-4b16-aec7-7d885c3410a2");
                streamListener.onExchangeComplete(conn, keepAlive);
            }
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "d05343a7-6caf-4634-8286-f9a73676fe52");
            if (!keepAlive) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "19bbc2ef-dd9c-4b12-8103-fe67bbd82eab");
                conn.close();
            }
        } finally {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "42ee6975-2760-4f46-91bc-dfeab3c7b413");
            if (response != null) {
                writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "b897333b-ac7b-4d8e-a11c-8a695567a5fa");
                response.close();
            }
        }
    }

    private boolean canResponseHaveBody(final ClassicHttpRequest request, final ClassicHttpResponse response) {
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "1f8a49fe-857c-4865-816d-13f17a498dec");
        if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "0d1c9e8d-2d12-4494-9a1e-4c49ac1691d5");
            return false;
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "48a4e096-0e3e-4362-8292-3e36aa6de8f9");
        final int status = response.getCode();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "ca0bb7e3-07e2-4366-8b80-4568743f0cea");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "57d9b997-2012-48e2-8495-37cc4f41cac0");
        if (ex instanceof MethodNotSupportedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "ba0eccd7-f386-48f4-94e7-56b7d0ae5b7d");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof UnsupportedHttpVersionException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "b756d481-3ee2-4ec1-97f5-6ea78bc2aed3");
            response.setCode(HttpStatus.SC_HTTP_VERSION_NOT_SUPPORTED);
        } else if (ex instanceof NotImplementedException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "bb51fc57-6c63-43ab-b2c2-3ac5cc2faeeb");
            response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
        } else if (ex instanceof ProtocolException) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "063b526e-e370-4508-a549-c5c3627e1a5d");
            response.setCode(HttpStatus.SC_BAD_REQUEST);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "a1ea164e-b39e-4326-a5b7-84c77824668a");
            response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "b64067e1-3497-430f-91fa-3ff636c609ea");
        String message = ex.getMessage();
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "64ab5993-0786-4115-b401-82d15976a6c5");
        if (message == null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "2c0c2cc6-ceb0-4316-a417-48cb1dd3feec");
            message = ex.toString();
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "a7ea9436-42e6-4aca-87af-dbe237f0e049");
        final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "f9bbb087-18f1-4a0c-b659-b75780d2a086");
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
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "23494a5c-2bee-4022-a1a3-e29296ba76d1");
        HttpRequestHandler handler = null;
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "1ce33b49-0271-4b69-9bee-e3a179601f9d");
        if (this.handlerMapper != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "fa9388d9-34a9-48cb-858b-a204c0839f85");
            handler = this.handlerMapper.lookup(request, context);
        }
        writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "d950de78-98f7-459e-82c6-ed7f967f9fff");
        if (handler != null) {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "171c88f4-47f8-43c0-a3c3-f8072ad97b44");
            handler.handle(request, response, context);
        } else {
            writeline("/home/ubuntu/results/coverage/HttpService/HttpService_7_10.coverage", "46a1a266-689b-4093-b035-b7ae7552faeb");
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
