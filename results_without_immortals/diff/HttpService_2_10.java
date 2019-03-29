HttpService
~~~
handleRequest
~~~
canResponseHaveBody
~~~
handleException
~
response.setCode(HttpStatus.SC_INTERNAL_SERVER_ERROR);
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
