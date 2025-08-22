# CDDL in Documentation

This document demonstrates various ways CDDL can be embedded in markdown.

## Basic Schema Example

```cddl
; Basic person schema
person = {
  name: tstr,
  age: uint,
  ? email: tstr
}
```

## Complex Schema with Groups

```cddl
; HTTP message structure
http-message = {
  method: "GET" / "POST" / "PUT" / "DELETE",
  uri: tstr,
  headers: { * tstr => tstr },
  ? body: bytes
}

; Group definition
address-group = (
  street: tstr,
  city: tstr,
  ? postal-code: tstr
)

contact = {
  address-group,
  country: tstr
}
```

## Inline code vs fenced blocks

You can use inline `person = { name: tstr }` for simple references, but fenced blocks are better for complete schemas:

```cddl
; Complete authentication schema
auth-request = {
  username: tstr,
  password: tstr,
  ? remember-me: bool
}

auth-response = {
  success: bool,
  ? token: tstr,
  ? error: tstr
}
```

## Tildes work too

~~~cddl
; Using tildes instead of backticks
data = {
  values: [* number], 
  ? metadata: { * tstr => any }
}
~~~
