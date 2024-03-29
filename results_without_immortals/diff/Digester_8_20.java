Digester
~~~
findNamespaceURI
~
return null;
~
if (nsStack == null) {
    return null;
}
~
try {
    return (nsStack.peek());
} catch (EmptyStackException e) {
    return null;
}
~~~
getClassLoader
~
if (this.classLoader != null) {
    return (this.classLoader);
}
~
if (this.useContextClassLoader) {
    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
    if (classLoader != null) {
        return (classLoader);
    }
}
~~~
setClassLoader
~
this.classLoader = classLoader;
~~~
getCount
~~~
getCurrentElementName
~
int lastSlash = elementName.lastIndexOf('/');
~
if (lastSlash >= 0) {
    elementName = elementName.substring(lastSlash + 1);
}
~~~
getErrorHandler
~~~
setErrorHandler
~
this.errorHandler = errorHandler;
~~~
getFactory
~
factory.setNamespaceAware(namespaceAware);
~
factory.setXIncludeAware(xincludeAware);
~
factory.setValidating(validating);
~
factory.setSchema(schema);
~~~
getFeature
~~~
setFeature
~
getFactory().setFeature(feature, value);
~~~
getLogger
~~~
setLogger
~
this.log = log;
~~~
getSAXLogger
~~~
setSAXLogger
~
this.saxLog = saxLog;
~~~
getMatch
~~~
getMatches
~~~
getNamespaceAware
~~~
setNamespaceAware
~
this.namespaceAware = namespaceAware;
~~~
getXIncludeAware
~~~
setXIncludeAware
~
this.xincludeAware = xincludeAware;
~~~
setPublicId
~
this.publicId = publicId;
~~~
getPublicId
~~~
getRuleNamespaceURI
~~~
setRuleNamespaceURI
~
getRules().setNamespaceURI(ruleNamespaceURI);
~~~
getParser
~
log.error("Digester.getParser: ", e);
~
return (null);
~~~
getProperty
~~~
setProperty
~
getParser().setProperty(property, value);
~~~
getRules
~
this.rules.setDigester(this);
~~~
setRules
~
this.rules = rules;
~
this.rules.setDigester(this);
~~~
getXMLSchema
~~~
setXMLSchema
~
this.schema = schema;
~~~
getUseContextClassLoader
~~~
setUseContextClassLoader
~
useContextClassLoader = use;
~~~
getValidating
~~~
setValidating
~
this.validating = validating;
~~~
getXMLReader
~
reader.setEntityResolver(this);
~
reader.setEntityResolver(entityResolver);
~
reader.setErrorHandler(this.errorHandler);
~
reader.setErrorHandler(this);
~
reader.setDTDHandler(this);
~
if (entityResolver == null) {
    reader.setEntityResolver(this);
} else {
    reader.setEntityResolver(entityResolver);
}
~
if (this.errorHandler != null) {
    reader.setErrorHandler(this.errorHandler);
} else {
    reader.setErrorHandler(this);
}
~~~
getSubstitutor
~~~
setSubstitutor
~
this.substitutor = substitutor;
~~~
getCustomContentHandler
~~~
setCustomContentHandler
~
customContentHandler = handler;
~~~
setStackAction
~
this.stackAction = stackAction;
~~~
getStackAction
~~~
getCurrentNamespaces
~~~
getExecutorService
~~~
setExecutorService
~
this.executorService = executorService;
~~~
characters
~
// forward calls instead of handling them here
customContentHandler.characters(buffer, start, length);
~
return;
~
saxLog.debug("characters(" + new String(buffer, start, length) + ")");
~
if (customContentHandler != null) {
    // forward calls instead of handling them here
    customContentHandler.characters(buffer, start, length);
    return;
}
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("characters(" + new String(buffer, start, length) + ")");
}
~
bodyText.append(buffer, start, length);
~~~
endDocument
~~~
endElement
~
log.debug("  Fire body() for " + rule);
~
log.debug("  Fire end() for " + rule);
~
Rule rule = rules.get(i);
~
if (debug) {
    log.debug("  Fire body() for " + rule);
}
~
rule.body(namespaceURI, name, bodyText);
~
log.error("Body event threw exception", e);
~
throw createSAXException(e);
~
log.error("Body event threw error", e);
~
throw e;
~
Rule rule = rules.get(j);
~
if (debug) {
    log.debug("  Fire end() for " + rule);
}
~
rule.end(namespaceURI, name);
~
log.error("End event threw exception", e);
~
throw createSAXException(e);
~
log.error("End event threw error", e);
~
throw e;
~
saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
~
bodyText = substitutor.substitute(bodyText);
~
try {
    Rule rule = rules.get(i);
    if (debug) {
        log.debug("  Fire body() for " + rule);
    }
    rule.body(namespaceURI, name, bodyText);
} catch (Exception e) {
    log.error("Body event threw exception", e);
    throw createSAXException(e);
} catch (Error e) {
    log.error("Body event threw error", e);
    throw e;
}
~
log.debug("  No rules found matching '" + match + "'.");
~
int j = (rules.size() - i) - 1;
~
try {
    Rule rule = rules.get(j);
    if (debug) {
        log.debug("  Fire end() for " + rule);
    }
    rule.end(namespaceURI, name);
} catch (Exception e) {
    log.error("End event threw exception", e);
    throw createSAXException(e);
} catch (Error e) {
    log.error("End event threw error", e);
    throw e;
}
~
// forward calls instead of handling them here
customContentHandler.endElement(namespaceURI, localName, qName);
~
return;
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
}
~
log.debug("  match='" + match + "'");
~
log.debug("  bodyText='" + bodyText + "'");
~
name = qName;
~
String bodyText = this.bodyText.toString();
~
Substitutor substitutor = getSubstitutor();
~
if (substitutor != null) {
    bodyText = substitutor.substitute(bodyText);
}
~
for (int i = 0; i < rules.size(); i++) {
    try {
        Rule rule = rules.get(i);
        if (debug) {
            log.debug("  Fire body() for " + rule);
        }
        rule.body(namespaceURI, name, bodyText);
    } catch (Exception e) {
        log.error("Body event threw exception", e);
        throw createSAXException(e);
    } catch (Error e) {
        log.error("Body event threw error", e);
        throw e;
    }
}
~
if (debug) {
    log.debug("  No rules found matching '" + match + "'.");
}
~
log.debug("  Popping body text '" + bodyText.toString() + "'");
~
for (int i = 0; i < rules.size(); i++) {
    int j = (rules.size() - i) - 1;
    try {
        Rule rule = rules.get(j);
        if (debug) {
            log.debug("  Fire end() for " + rule);
        }
        rule.end(namespaceURI, name);
    } catch (Exception e) {
        log.error("End event threw exception", e);
        throw createSAXException(e);
    } catch (Error e) {
        log.error("End event threw error", e);
        throw e;
    }
}
~
match = match.substring(0, slash);
~
match = "";
~
if (customContentHandler != null) {
    // forward calls instead of handling them here
    customContentHandler.endElement(namespaceURI, localName, qName);
    return;
}
~
boolean debug = log.isDebugEnabled();
~
if (debug) {
    if (saxLog.isDebugEnabled()) {
        saxLog.debug("endElement(" + namespaceURI + "," + localName + "," + qName + ")");
    }
    log.debug("  match='" + match + "'");
    log.debug("  bodyText='" + bodyText + "'");
}
~
// the actual element name is either in localName or qName, depending
// on whether the parser is namespace aware
String name = localName;
~
if ((name == null) || (name.length() < 1)) {
    name = qName;
}
~
// Fire "body" events for all relevant rules
List<Rule> rules = matches.pop();
~
if ((rules != null) && (rules.size() > 0)) {
    String bodyText = this.bodyText.toString();
    Substitutor substitutor = getSubstitutor();
    if (substitutor != null) {
        bodyText = substitutor.substitute(bodyText);
    }
    for (int i = 0; i < rules.size(); i++) {
        try {
            Rule rule = rules.get(i);
            if (debug) {
                log.debug("  Fire body() for " + rule);
            }
            rule.body(namespaceURI, name, bodyText);
        } catch (Exception e) {
            log.error("Body event threw exception", e);
            throw createSAXException(e);
        } catch (Error e) {
            log.error("Body event threw error", e);
            throw e;
        }
    }
} else {
    if (debug) {
        log.debug("  No rules found matching '" + match + "'.");
    }
}
~
// Recover the body text from the surrounding element
bodyText = bodyTexts.pop();
~
if (debug) {
    log.debug("  Popping body text '" + bodyText.toString() + "'");
}
~
// Fire "end" events for all relevant rules in reverse order
if (rules != null) {
    for (int i = 0; i < rules.size(); i++) {
        int j = (rules.size() - i) - 1;
        try {
            Rule rule = rules.get(j);
            if (debug) {
                log.debug("  Fire end() for " + rule);
            }
            rule.end(namespaceURI, name);
        } catch (Exception e) {
            log.error("End event threw exception", e);
            throw createSAXException(e);
        } catch (Error e) {
            log.error("End event threw error", e);
            throw e;
        }
    }
}
~
// Recover the previous match expression
int slash = match.lastIndexOf('/');
~
if (slash >= 0) {
    match = match.substring(0, slash);
} else {
    match = "";
}
~~~
endPrefixMapping
~~~
ignorableWhitespace
~
saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("ignorableWhitespace(" + new String(buffer, start, len) + ")");
}
~~~
processingInstruction
~
// forward calls instead of handling them here
customContentHandler.processingInstruction(target, data);
~
return;
~
saxLog.debug("processingInstruction('" + target + "','" + data + "')");
~
if (customContentHandler != null) {
    // forward calls instead of handling them here
    customContentHandler.processingInstruction(target, data);
    return;
}
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("processingInstruction('" + target + "','" + data + "')");
}
~~~
getDocumentLocator
~~~
setDocumentLocator
~
saxLog.debug("setDocumentLocator(" + locator + ")");
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("setDocumentLocator(" + locator + ")");
}
~
this.locator = locator;
~~~
skippedEntity
~
saxLog.debug("skippedEntity(" + name + ")");
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("skippedEntity(" + name + ")");
}
~~~
startDocument
~
saxLog.debug("startDocument()");
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("startDocument()");
}
~
// ensure that the digester is properly configured, as
// the digester could be used as a SAX ContentHandler
// rather than via the parse() methods.
configure();
~~~
startElement
~
log.error("Begin event threw exception", e);
~
throw createSAXException(e);
~
log.error("Begin event threw error", e);
~~~
startPrefixMapping
~
saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
~
stack = new Stack<String>();
~
namespaces.put(prefix, stack);
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("startPrefixMapping(" + prefix + "," + namespaceURI + ")");
}
~
// Register this prefix mapping
Stack<String> stack = namespaces.get(prefix);
~
if (stack == null) {
    stack = new Stack<String>();
    namespaces.put(prefix, stack);
}
~
stack.push(namespaceURI);
~~~
notationDecl
~
saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("notationDecl(" + name + "," + publicId + "," + systemId + ")");
}
~~~
unparsedEntityDecl
~
saxLog.debug("unparsedEntityDecl(" + name + "," + publicId + "," + systemId + "," + notation + ")");
~
if (saxLog.isDebugEnabled()) {
    saxLog.debug("unparsedEntityDecl(" + name + "," + publicId + "," + systemId + "," + notation + ")");
}
~~~
setEntityResolver
~
this.entityResolver = entityResolver;
~~~
getEntityResolver
~~~
resolveEntity
~
// cannot resolve
if (log.isDebugEnabled()) {
    log.debug(" Cannot resolve null entity, returning null InputSource");
}
~
return (null);
~
log.debug(" Trying to resolve using system ID '" + systemId + "'");
~
throw new IllegalArgumentException("Malformed URL '" + systemId + "' : " + e.getMessage());
~~~
error
~
log.error("Parse Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
~~~
fatalError
~
log.error("Parse Fatal Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
~~~
warning
~
log.warn("Parse Warning Error at line " + exception.getLineNumber() + " column " + exception.getColumnNumber() + ": " + exception.getMessage(), exception);
~~~
parse
~
input.setSystemId(file.toURI().toURL().toString());
~~~
asyncParse
~~~
parse
~
input.setSystemId(file.toURI().toURL().toString());
~~~
asyncParse
~~~
parse
~
input.setSystemId(file.toURI().toURL().toString());
~~~
asyncParse
~~~
parse
~
input.setSystemId(file.toURI().toURL().toString());
~~~
asyncParse
~~~
parse
~
input.setSystemId(file.toURI().toURL().toString());
~~~
asyncParse
~~~
parse
~
input.setSystemId(file.toURI().toURL().toString());
~~~
asyncParse
~~~
asyncParse
~~~
register
~
log.debug("register('" + publicId + "', '" + entityURL + "'");
~
if (log.isDebugEnabled()) {
    log.debug("register('" + publicId + "', '" + entityURL + "'");
}
~
entityValidator.put(publicId, entityURL);
~~~
register
~
log.debug("register('" + publicId + "', '" + entityURL + "'");
~
if (log.isDebugEnabled()) {
    log.debug("register('" + publicId + "', '" + entityURL + "'");
}
~
entityValidator.put(publicId, entityURL);
~~~
registerAll
~
this.entityValidator.putAll(entityValidator);
~~~
createInputSourceFromURL
~
connection.setUseCaches(false);
~
source.setSystemId(url.toExternalForm());
~
inputSources.add(source);
~~~
createInputSourceFromURL
~
connection.setUseCaches(false);
~
source.setSystemId(url.toExternalForm());
~
inputSources.add(source);
~~~
addRule
~~~
addRuleSet
~
log.debug("addRuleSet() with no namespace URI");
~
log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
~
if (newNamespaceURI == null) {
    log.debug("addRuleSet() with no namespace URI");
} else {
    log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
}
~
String oldNamespaceURI = getRuleNamespaceURI();
~
String newNamespaceURI = ruleSet.getNamespaceURI();
~
if (log.isDebugEnabled()) {
    if (newNamespaceURI == null) {
        log.debug("addRuleSet() with no namespace URI");
    } else {
        log.debug("addRuleSet() with namespace URI " + newNamespaceURI);
    }
}
~
setRuleNamespaceURI(newNamespaceURI);
~
ruleSet.addRuleInstances(this);
~
setRuleNamespaceURI(oldNamespaceURI);
~~~
addBeanPropertySetter
~
addRule(pattern, new BeanPropertySetterRule());
~~~
addBeanPropertySetter
~
addRule(pattern, new BeanPropertySetterRule());
~~~
addCallMethod
~
addRule(pattern, new CallMethodRule(methodName));
~~~
addCallMethod
~
addRule(pattern, new CallMethodRule(methodName));
~~~
addCallMethod
~
addRule(pattern, new CallMethodRule(methodName));
~~~
addCallMethod
~
addRule(pattern, new CallMethodRule(methodName));
~~~
addCallParam
~
addRule(pattern, new CallParamRule(paramIndex));
~~~
addCallParam
~
addRule(pattern, new CallParamRule(paramIndex));
~~~
addCallParam
~
addRule(pattern, new CallParamRule(paramIndex));
~~~
addCallParam
~
addRule(pattern, new CallParamRule(paramIndex));
~~~
addCallParamPath
~
addRule(pattern, new PathCallParamRule(paramIndex));
~~~
addObjectParam
~
addRule(pattern, new ObjectParamRule(paramIndex, paramObj));
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addFactoryCreate
~
addFactoryCreate(pattern, className, false);
~~~
addObjectCreate
~
addRule(pattern, new ObjectCreateRule(className));
~~~
addObjectCreate
~
addRule(pattern, new ObjectCreateRule(className));
~~~
addObjectCreate
~
addRule(pattern, new ObjectCreateRule(className));
~~~
addObjectCreate
~
addRule(pattern, new ObjectCreateRule(className));
~~~
addSetNestedProperties
~
addRule(pattern, new SetNestedPropertiesRule());
~~~
addSetNestedProperties
~
addRule(pattern, new SetNestedPropertiesRule());
~~~
addSetNestedProperties
~
addRule(pattern, new SetNestedPropertiesRule());
~~~
addSetNext
~
addRule(pattern, new SetNextRule(methodName));
~~~
addSetNext
~
addRule(pattern, new SetNextRule(methodName));
~~~
addSetRoot
~
addRule(pattern, new SetRootRule(methodName));
~~~
addSetRoot
~
addRule(pattern, new SetRootRule(methodName));
~~~
addSetProperties
~~~
addSetProperties
~~~
addSetProperties
~~~
addSetProperty
~
addRule(pattern, new SetPropertyRule(name, value));
~~~
addSetTop
~
addRule(pattern, new SetTopRule(methodName));
~~~
addSetTop
~
addRule(pattern, new SetTopRule(methodName));
~~~
clear
~
match = "";
~
bodyTexts.clear();
~
params.clear();
~
publicId = null;
~
stack.clear();
~
stacksByName.clear();
~
customContentHandler = null;
~~~
peek
~
log.warn("Empty stack (returning null)");
~~~
peek
~
log.warn("Empty stack (returning null)");
~~~
pop
~
if (stackAction != null) {
    popped = stackAction.onPop(this, null, popped);
}
~
log.warn("Empty stack (returning null)");
~~~
push
~
object = stackAction.onPush(this, null, object);
~
if (stackAction != null) {
    object = stackAction.onPush(this, null, object);
}
~
stack.push(object);
~~~
push
~
object = stackAction.onPush(this, null, object);
~
if (stackAction != null) {
    object = stackAction.onPush(this, null, object);
}
~
stack.push(object);
~~~
pop
~
if (stackAction != null) {
    popped = stackAction.onPop(this, null, popped);
}
~
log.warn("Empty stack (returning null)");
~~~
peek
~
log.warn("Empty stack (returning null)");
~~~
peek
~
log.warn("Empty stack (returning null)");
~~~
isEmpty
~
Stack<Object> namedStack = stacksByName.get(stackName);
~
if (namedStack != null) {
    result = namedStack.isEmpty();
}
~~~
getRoot
~~~
resetRoot
~
root = null;
~~~
cleanup
~
log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
~
source.getByteStream().close();
~
// Fall through so we get them all
if (log.isWarnEnabled()) {
    log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
}
~
try {
    source.getByteStream().close();
} catch (IOException e) {
    // Fall through so we get them all
    if (log.isWarnEnabled()) {
        log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
    }
}
~
// they each have an input stream that should be closed
for (InputSource source : inputSources) {
    try {
        source.getByteStream().close();
    } catch (IOException e) {
        // Fall through so we get them all
        if (log.isWarnEnabled()) {
            log.warn(format("An error occurred while closing resource %s (%s)", source.getPublicId(), source.getSystemId()), e);
        }
    }
}
~
inputSources.clear();
~~~
configure
~
// Do not configure more than once
if (configured) {
    return;
}
~
// call hook method for subclasses that want to be initialized once only
initialize();
~
// Set the configuration flag to avoid repeating
configured = true;
~~~
isConfigured
~~~
initialize
~~~
getRegistrations
~~~
peekParams
~
log.warn("Empty stack (returning null)");
~~~
peekParams
~
log.warn("Empty stack (returning null)");
~~~
popParams
~
if (log.isTraceEnabled()) {
    log.trace("Popping params");
}
~
log.warn("Empty stack (returning null)");
~~~
pushParams
~
log.trace("Pushing params");
~
if (log.isTraceEnabled()) {
    log.trace("Pushing params");
}
~
params.push(object);
~~~
createSAXException
~
if ((e != null) && (e instanceof InvocationTargetException)) {
    Throwable t = ((InvocationTargetException) e).getTargetException();
    if ((t != null) && (t instanceof Exception)) {
        e = (Exception) t;
    }
}
~
if (locator != null) {
    String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
    if (e != null) {
        return new SAXParseException(error, locator, e);
    }
    return new SAXParseException(error, locator);
}
~
log.error("No Locator!");
~
if (e != null) {
    return new SAXException(message, e);
}
~~~
createSAXException
~
if ((e != null) && (e instanceof InvocationTargetException)) {
    Throwable t = ((InvocationTargetException) e).getTargetException();
    if ((t != null) && (t instanceof Exception)) {
        e = (Exception) t;
    }
}
~
if (locator != null) {
    String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
    if (e != null) {
        return new SAXParseException(error, locator, e);
    }
    return new SAXParseException(error, locator);
}
~
log.error("No Locator!");
~
if (e != null) {
    return new SAXException(message, e);
}
~~~
createSAXException
~
if ((e != null) && (e instanceof InvocationTargetException)) {
    Throwable t = ((InvocationTargetException) e).getTargetException();
    if ((t != null) && (t instanceof Exception)) {
        e = (Exception) t;
    }
}
~
if (locator != null) {
    String error = "Error at line " + locator.getLineNumber() + " char " + locator.getColumnNumber() + ": " + message;
    if (e != null) {
        return new SAXParseException(error, locator, e);
    }
    return new SAXParseException(error, locator);
}
~
log.error("No Locator!");
~
if (e != null) {
    return new SAXException(message, e);
}
~~~
npeSafeCast
~
if (obj == null) {
    return null;
}
