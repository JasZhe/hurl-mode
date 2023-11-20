POST https://httpbin/post
User-Agent: My User Agent
Content-Type: application/json
[BasicAuth]
alice: secret

```xml
<something> something </somethingsad>
<something> else </something>
<something>
	<nested-something> blah </nested-something>
</something>
```

POST https://httpbin/post
User-Agent: My User Agent
Content-Type: application/json
[BasicAuth]
alice: secret

```json
{ "soething": 1,
	"something": "json",
	"something": "else",
	{
		"mything": "else"
	}
}
```

```graphql
{
    human(id: "1000") {
        name
        height(unit: FOOT)
    }
}
```
