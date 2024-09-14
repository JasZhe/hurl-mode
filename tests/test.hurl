GET http://localhost:8899/get
SomeHeader: 1

POST http://localhost:8899/post
```json
{
  "number": 1,
  "string": "foo",
  "bool": true
}
```

POST http://localhost:8899/post
```xml
<!-- comment -->
<elem attr="something">one &lt; two</elem>
```
HTTP 200
[Captures]
access_token: jsonpath "$['access_token']"
refresh_token: jsonpath "$['refresh_token']"
