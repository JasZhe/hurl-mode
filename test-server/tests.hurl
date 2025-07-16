# Test basic endpoints
GET http://localhost:8080/
HTTP/1.1 200
[Asserts]
jsonpath "$.message" == "Hurl test server"

GET http://localhost:8080/health
HTTP/1.1 200
[Asserts]
jsonpath "$.status" == "ok"

# Test users API
GET http://localhost:8080/api/users
HTTP/1.1 200
[Asserts]
jsonpath "$" count == 2
jsonpath "$[0].name" == "John Doe"

# Create a new user
POST http://localhost:8080/api/users
Content-Type: application/json
{
  "name": "Alice Johnson",
  "email": "alice@example.com"
}
HTTP/1.1 201
[Captures]
user_id: jsonpath "$.id"
[Asserts]
jsonpath "$.name" == "Alice Johnson"

# Get the created user
GET http://localhost:8080/api/users/{{user_id}}
HTTP/1.1 200
[Asserts]
jsonpath "$.name" == "Alice Johnson"

# Test echo endpoint
POST http://localhost:8080/echo
Content-Type: application/json
X-Test-Header: test-value
{
  "test": "data"
}
HTTP/1.1 200
[Asserts]
jsonpath "$.method" == "POST"
jsonpath "$.body" contains "test"

# Test delay endpoint
GET http://localhost:8080/delay/1
HTTP/1.1 200
[Asserts]
jsonpath "$.delayed" == 1

# Test status codes
GET http://localhost:8080/status/404
HTTP/1.1 404

GET http://localhost:8080/status/500
HTTP/1.1 500
