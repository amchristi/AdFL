/*
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */

package org.apache.hc.core5.http.impl.nio;

import org.apache.hc.core5.annotation.Contract;
import org.apache.hc.core5.annotation.ThreadingBehavior;
import org.apache.hc.core5.http.URIScheme;
import org.apache.hc.core5.http.nio.ssl.TlsStrategy;
import org.apache.hc.core5.reactor.IOEventHandler;
import org.apache.hc.core5.reactor.IOEventHandlerFactory;
import org.apache.hc.core5.reactor.TlsCapableIOSession;
import org.apache.hc.core5.util.Args;

/**
 * @since 5.0
 */
@Contract(threading = ThreadingBehavior.IMMUTABLE)
public class ServerHttp1IOEventHandlerFactory implements IOEventHandlerFactory {

    private final ServerHttp1StreamDuplexerFactory streamDuplexerFactory;
    private final TlsStrategy tlsStrategy;

    public ServerHttp1IOEventHandlerFactory(
            final ServerHttp1StreamDuplexerFactory streamDuplexerFactory,
            final TlsStrategy tlsStrategy) {
        this.streamDuplexerFactory = Args.notNull(streamDuplexerFactory, "Stream duplexer factory");
        this.tlsStrategy = tlsStrategy;
    }

    @Override
    public IOEventHandler createHandler(final TlsCapableIOSession ioSession, final Object attachment) {
        final boolean tlsSecured;
        if (tlsStrategy != null) {
            tlsSecured = tlsStrategy.upgrade(
                    ioSession,
                    null,
                    ioSession.getLocalAddress(),
                    ioSession.getRemoteAddress(),
                    attachment);
        } else {
            tlsSecured = false;
        }
        return new ServerHttp1IOEventHandler(streamDuplexerFactory.create(
                tlsSecured ? URIScheme.HTTPS.id : URIScheme.HTTP.id,
                ioSession));
    }

}
