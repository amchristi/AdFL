HttpService
~~~
handleRequest
~~~
canResponseHaveBody
~
if (request != null && "HEAD".equalsIgnoreCase(request.getMethod())) {
    return false;
}
~~~
handleException
~
response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
~
response.setCode(HttpStatus.SC_NOT_IMPLEMENTED);
~
message = ex.toString();
~
String message = ex.getMessage();
~
if (message == null) {
    message = ex.toString();
}
~
final StringEntity entity = new StringEntity(message, ContentType.TEXT_PLAIN);
~
response.setEntity(entity);
~~~
doService
