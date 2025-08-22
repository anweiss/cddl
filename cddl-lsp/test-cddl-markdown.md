# CDDL Fenced Code Block Test

This is a test file to verify that CDDL syntax highlighting works in fenced code blocks.

```cddl
; A simple CDDL schema for testing
person = {
  name: tstr,
  age: uint,
  ? email: tstr,
  addresses: [* address]
}

address = {
  street: tstr,
  city: tstr,
  country: tstr
}

; Test comment
fruit = "apple" / "orange" / "banana"
```

The above should have CDDL syntax highlighting applied.

## Another Example

```cddl
; JSON Web Token Claims Set
jwt-claims = {
  ? iss: tstr,           ; issuer
  ? sub: tstr,           ; subject  
  ? aud: tstr / [* tstr], ; audience
  ? exp: uint,           ; expiration time
  ? nbf: uint,           ; not before
  ? iat: uint,           ; issued at
  ? jti: tstr,           ; JWT ID
  * claim-name => claim-value
}

claim-name = tstr
claim-value = any
```

## Case Insensitive Test

This should also work with uppercase:

```CDDL
; Test with uppercase language identifier
message = {
  id: uint,
  text: tstr,
  timestamp: uint
}
```
