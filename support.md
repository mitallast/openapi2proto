# OpenAPI v3 support

Not supported:
 - external `$ref`
 - `default` in enums, parameters and object fields
 - `minimum` in parameters and object fields
 - `maximum` in parameters and object fields

## info:

Supports: 
 - `x-proto-package`: custom protobuf package
 - fallback generate protobuf package from OpenAPI file path
 - `x-proto-service`: custom protobuf gRPC service name
 - fallback generate gRPC service name from title
 
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
- `array`

Array items supported:
- `integer` with format `int64` or `int32`
- `number` with format `float` or `double`
- `string`
- `boolean`
- `date` (compiles as `string`)
- `datetime` (compiles as `string`)
- `$ref` to `#/components/schemas/{name}`

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
 - `application/json`: supports `array` of `$ref`, `$ref` in `schema`
 - `text/plain`: only `type`: `string` supports
 - `application/octet-stream`: only `type`: `string` and `format`: `binary` supports

Not supported:
 - `$ref` in `requestBody`
 - multiple content types
 - inline object definitions in `schema`
 
Will be supported:
 - `oneof` in `schema`
 - primitive types in `schema`

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
 - `application/json`: supports `array` of `$ref`, `$ref` in `schema`
 - `text/plain`: only `type`: `string` supports
 - `application/octet-stream`: only `type`: `string` and `format`: `binary` supports
 
Not supported:
 - `$ref` in `response`
 - multiple content types
 - inline object definitions in `schema`
 
Will be supported:
 - `oneof` in `schema`
 
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
 - `array`

Array property items supported:
 - `integer` with format `int64` or `int32`
 - `number` with format `float` or `double`
 - `string`
 - `boolean`
 - `date` (compiles as `string`)
 - `datetime` (compiles as `string`)
 - `$ref` to `#/components/schemas/{name}`