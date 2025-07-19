GET https://dog.ceo/api/breeds/list/all
HTTP 200
[Captures]
aus_breed: jsonpath "$.message.australian[1]"
[Asserts]
jsonpath "$.message.australian[*]" count == 2

GET https://dog.ceo/api/breed/australian/{{aus_breed}}/images/random
HTTP 200
[Captures]
image_url: jsonpath "$.message"
[Asserts]
jsonpath "$.message" exists

GET {{image_url}} # can load image-mode in proc buffer to show the image
