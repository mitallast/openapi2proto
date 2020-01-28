# OpenAPI v3 support

Not supported:
 - `default` in enums, parameters and object fields
 - `minimum` in parameters and object fields
 - `maximum` in parameters and object fields

Supports: 
 - `x-proto-package`: custom protobuf package
 - fallback generate protobuf package from OpenAPI file path
 - `x-proto-service`: custom protobuf gRPC service name
 - fallback generate gRPC service name from title
 - `x-proto-import` custom protobuf import, list of imports
 - `x-proto-option` custom protobuf options, key-value map

## info:
 
Ignore:
 - `description`
 - `version`
 - `termsOfService`
 - `contact`
 - `license`

## servers:

Not supported

## tags:

Not supported

## paths:

Requirements:
 - `operationId`

Supports:
 - all http methods supported by OpenAPI
 - path parameters like `/pet/{petId}:`
 - `x-proto-request` to override request message id
 - `x-proto-response` to override response message id

### request:

Each request will be generated as gRPC call, using `operationId` as gRPC operation identifier.
For each call will be forcibly generated `{operationId}Request` and `{operationId}Response` protobuf messages
for backward binary compatibility. 

Requirements:
 - `operationId`

Supports:
 - `operationId`
 - `parameters`
 - `requestBody`
 - `responses`
 
Ignore: 
 - `tags`
 - `summary`
 - `description`
 - `headers`
 - `deprecated`
 
#### request parameters:

Each request parameter will be generated as field in `{operationId}Request` message.

Requirements:
 - `name` must be an identifier in terms of protobuf v3 syntax
 - `schema` is required
 
Supports:
 - `x-proto-field` to override protobuf field name
 - `x-proto-field-id` to override protobuf field id
 - `boolean` type in schema
 - `enum` type in schema
 - `date` type in `schema`
 - `required`
 
`schema` supports:
- `integer` with format `int64` or `int32`
- `number` with format `float` or `double`
- `string`
- `boolean`
- `date` (compiles as `string`)
- `datetime` (compiles as `string`)
- `$ref` to `#/components/schemas/{name}`
- `$ref` to `external.yaml#/components/schemas/{name}`
- `array`
- `oneOf`

`integer` supports `x-proto-format` with values `fixed` or `varint`.

`date-time` supports `x-proto-format` with values:
- `unix-time` as unix timestamp in seconds, represented as `fixed64`
- `unix-time-millis` as unix timestamp in millis, represented as `fixed64`
- `timestamp` represented as protobuf `google.protobuf.Timestamp`

Array items supported:
- `integer` with format `int64` or `int32`
- `number` with format `float` or `double`
- `string`
- `boolean`
- `date` (compiles as `string`)
- `datetime` (compiles as `string`)
- `$ref` to `#/components/schemas/{name}`
- `$ref` to `external.yaml#/components/schemas/{name}`

Not supports:
 - `minimum`
 - `maximum`
 
Ignore:
 - `description`
 - `in`
 - `explode`

#### requestBody:

Request body will be generated as field in `{operationId}Request` message, by default named as `request_body`. 

Requirements:
 - `requestBody` must contain `content`

Supports:
 - `content`
 - `x-proto-field` to override protobuf field name
 - `x-proto-field-id` to override protobuf field id

Not supported:
 - multiple content types
 - `required`
 
Ignore:
 - `description`

##### requestBody/content:

Requirements:
 - must contain content type
 - only one content type allowed
 - must contain schema

Supports:
 - `x-proto-field` to override protobuf field name
 - `x-proto-field-id` to override protobuf field id

Supported content types:
 - `application/json`: supports `array` of `$ref`, `$ref` in `schema`, `oneOf`, primitive types
 - `text/plain`: only `type`: `string` supports
 - `application/octet-stream`: only `type`: `string` and `format`: `binary` supports
 - inline object definitions in `schema`

Not supported:
 - `$ref` in `requestBody`
 - multiple content types
 
#### responses:

Supports:
 - `default`
 - `200`
 - `201`
 - without response
 
Not supports:
 - `1**`
 - `3**`
 - `4**`
 - `5**`
 - `$ref` in `response`
 - headers

##### response:

Response body will be generated as field in `{operationId}Response` message, by default named as `response_body`.

Requirement:
 - must contain `content type` (`media type`)
 - only one `content type` supports
 
Supports:
- `x-proto-field` to override protobuf field name
- `x-proto-field-id` to override protobuf field id
 
Supported media types:
 - `application/json`: supports `array` of `$ref`, `$ref` in `schema`, `oneof`, primitive types
 - `text/plain`: only `type`: `string` supports
 - `application/octet-stream`: only `type`: `string` and `format`: `binary` supports
 - inline object definitions in `schema`
 
Not supported:
 - `$ref` in `response`
 - multiple content types
 
## components:

Only `#/components/schemas/` supported with `type`: `object`

Supports:
 - `type` = `object`
 - `type` = `string` with `enum`

Ignored:
 - primitive definitions

Will be supported:
 - primitive definitions

### component objects:

Requirements:
 - object name must be an identifier in terms of protobuf v3 syntax
 - field name must be an identifier in terms of protobuf v3 syntax
 
Supports:
 - `required`
 - `x-proto-reserved` to override
 - `x-proto-field`
 - `x-proto-field-id`
 - recursive definitions

Properties supported:
 - `integer` with format `int64` or `int32`
 - `number` with format `float` or `double`
 - `string`
 - `boolean`
 - `date` (compiles as `string`)
 - `datetime` (compiles as `string`)
 - `$ref` to `#/components/schemas/{name}`
 - `$ref` to `external.yaml#/components/schemas/{name}`
 - `array`
 - `oneOf`

`array` and `oneOf` property items supported:
 - `integer` with format `int64` or `int32`
 - `number` with format `float` or `double`
 - `string`
 - `boolean`
 - `date` (compiles as `string`)
 - `datetime` (compiles as `string`)
 - `$ref` to `#/components/schemas/{name}`
 - `$ref` to `external.yaml#/components/schemas/{name}`
 
Not supports:
 - inline object definitions in `schema`