import { MarkupContent } from "vscode-languageserver";

interface ItemDetail {
  label: string;
  detail: string;
  documentation?: string | MarkupContent;
}

export const standardPrelude: ItemDetail[] = [
  {
    label: "any",
    detail: "any data type",
    documentation: {
      kind: "markdown",
      value: `**Any Data Type**

Matches any CBOR data item.

\`\`\`cddl
flexible-field = any
mixed-array = [* any]
\`\`\``,
    },
  },
  {
    label: "uint",
    detail: "unsigned integer data type",
    documentation: {
      kind: "markdown",
      value: `**Unsigned Integer**

Non-negative integer values (0, 1, 2, ...).

\`\`\`cddl
age = uint
count = uint .le 100
\`\`\``,
    },
  },
  {
    label: "nint",
    detail: "negative integer data type",
    documentation: {
      kind: "markdown",
      value: `**Negative Integer**

Negative integer values (-1, -2, -3, ...).

\`\`\`cddl
debt = nint
temperature-below-zero = nint
\`\`\``,
    },
  },
  {
    label: "int",
    detail: "integer data type",
    documentation: {
      kind: "markdown",
      value: `**Integer**

Any integer value, positive, negative, or zero.

\`\`\`cddl
temperature = int
offset = int
balance = int
\`\`\``,
    },
  },
  {
    label: "bstr",
    detail: "byte string data type",
    documentation: {
      kind: "markdown",
      value: `**Byte String**

Sequence of bytes/octets.

\`\`\`cddl
hash = bstr .size 32
certificate = bstr
encrypted-data = bstr
\`\`\``,
    },
  },
  {
    label: "bytes",
    detail: "byte string data type; alias for bstr",
    documentation: {
      kind: "markdown",
      value: `**Bytes (Alias for bstr)**

Sequence of bytes/octets. Alias for \`bstr\`.

\`\`\`cddl
signature = bytes .size 64
payload = bytes
\`\`\``,
    },
  },
  {
    label: "tstr",
    detail: "text string data type",
    documentation: {
      kind: "markdown",
      value: `**Text String**

UTF-8 encoded text string.

\`\`\`cddl
name = tstr
email = tstr .regexp "[^@]+@[^@]+"
message = tstr .size (1..1000)
\`\`\``,
    },
  },
  {
    label: "text",
    detail: "text string data type; alias for tstr",
    documentation: {
      kind: "markdown",
      value: `**Text (Alias for tstr)**

UTF-8 encoded text string. Alias for \`tstr\`.

\`\`\`cddl
title = text
description = text
\`\`\``,
    },
  },
  {
    label: "tdate",
    detail: "date/time string data type",
    documentation: {
      kind: "markdown",
      value: `**Text Date**

Date/time as an RFC 3339 text string.

\`\`\`cddl
created-at = tdate
; Example: "2023-12-25T10:30:00Z"
\`\`\``,
    },
  },
  {
    label: "time",
    detail: "time in seconds relative to UNIX epoch data type",
    documentation: {
      kind: "markdown",
      value: `**Numeric Time**

Time as seconds since UNIX epoch (1970-01-01T00:00:00Z).

\`\`\`cddl
timestamp = time
expiry = time
\`\`\``,
    },
  },
  {
    label: "number",
    detail: "integer or floating point number data type",
    documentation: {
      kind: "markdown",
      value: `**Number**

Any numeric value: integer or floating-point.

\`\`\`cddl
price = number
coordinate = number
measurement = number
\`\`\``,
    },
  },
  {
    label: "biguint",
    detail: "large unsigned integer encoded as a byte string data type",
    documentation: {
      kind: "markdown",
      value: `**Big Unsigned Integer**

Large unsigned integer that doesn't fit in standard integer types, encoded as byte string.

\`\`\`cddl
very-large-number = biguint
cryptographic-key = biguint
\`\`\``,
    },
  },
  {
    label: "bignint",
    detail: "large negative integer encoded as a byte string data type",
    documentation: {
      kind: "markdown",
      value: `**Big Negative Integer**

Large negative integer encoded as byte string.

\`\`\`cddl
very-small-number = bignint
\`\`\``,
    },
  },
  {
    label: "bigint",
    detail: "large integer encoded as a byte string data type",
    documentation: {
      kind: "markdown",
      value: `**Big Integer**

Large integer (positive or negative) encoded as byte string.

\`\`\`cddl
huge-number = bigint
\`\`\``,
    },
  },
  {
    label: "integer",
    detail: "alias for int or bigint data type",
    documentation: {
      kind: "markdown",
      value: `**Integer (Flexible)**

Any integer value, using \`int\` or \`bigint\` as needed.

\`\`\`cddl
flexible-int = integer
\`\`\``,
    },
  },
  {
    label: "unsigned",
    detail: "alias for uint or biguint data type",
    documentation: {
      kind: "markdown",
      value: `**Unsigned (Flexible)**

Any unsigned integer, using \`uint\` or \`biguint\` as needed.

\`\`\`cddl
flexible-uint = unsigned
\`\`\``,
    },
  },
  { label: "decfrac", detail: "decimal fraction data type" },
  { label: "bigfloat", detail: "bitfloat data type" },
  {
    label: "eb64url",
    detail:
      "expected conversion of byte string to base64url-encoded text string data type",
  },
  {
    label: "eb64legacy",
    detail:
      "expected conversion of byte string to base64-encoded text string data type",
  },
  {
    label: "eb16",
    detail:
      "expected conversion of byte string to base16-encoded text string data type",
  },
  { label: "encoded-cbor", detail: "embedded CBOR data item" },
  {
    label: "uri",
    detail: "URI text string data type",
    documentation: {
      kind: "markdown",
      value: `**URI**

Uniform Resource Identifier as text string.

\`\`\`cddl
website = uri
api-endpoint = uri
\`\`\``,
    },
  },
  { label: "b64url", detail: "base64url-encoded text string data type" },
  { label: "b64legacy", detail: "base64-encoded text string data type" },
  {
    label: "regexp",
    detail: "regular expression in PCRE or JavaScript syntax data type",
    documentation: {
      kind: "markdown",
      value: `**Regular Expression**

Regular expression pattern for text validation.

\`\`\`cddl
pattern = regexp
email-pattern = regexp
\`\`\``,
    },
  },
  {
    label: "mime-message",
    detail: "MIME message data type, including all headers",
  },
  { label: "cbor-any", detail: "self-described CBOR data type" },
  {
    label: "float16",
    detail: "16-bit floating point value",
    documentation: {
      kind: "markdown",
      value: `**16-bit Float**

Half-precision floating-point number.

\`\`\`cddl
precision-value = float16
\`\`\``,
    },
  },
  {
    label: "float32",
    detail: "32-bit floating point value",
    documentation: {
      kind: "markdown",
      value: `**32-bit Float**

Single-precision floating-point number.

\`\`\`cddl
coordinate = float32
\`\`\``,
    },
  },
  {
    label: "float64",
    detail: "64-bit floating point value",
    documentation: {
      kind: "markdown",
      value: `**64-bit Float**

Double-precision floating-point number.

\`\`\`cddl
precise-measurement = float64
\`\`\``,
    },
  },
  { label: "float16-32", detail: "16- or 32-bit floating point value" },
  { label: "float32-64", detail: "32- or 64-bit floating point value" },
  {
    label: "float",
    detail: "floating point value",
    documentation: {
      kind: "markdown",
      value: `**Float**

Any floating-point number.

\`\`\`cddl
ratio = float
percentage = float
\`\`\``,
    },
  },
  {
    label: "false",
    detail: "false",
    documentation: {
      kind: "markdown",
      value: `**False Boolean Value**

The boolean value \`false\`.

\`\`\`cddl
disabled = false
\`\`\``,
    },
  },
  {
    label: "true",
    detail: "true",
    documentation: {
      kind: "markdown",
      value: `**True Boolean Value**

The boolean value \`true\`.

\`\`\`cddl
enabled = true
\`\`\``,
    },
  },
  {
    label: "bool",
    detail: "boolean",
    documentation: {
      kind: "markdown",
      value: `**Boolean**

Boolean value: \`true\` or \`false\`.

\`\`\`cddl
is-active = bool
flag = bool
\`\`\``,
    },
  },
  {
    label: "nil",
    detail: "null value",
    documentation: {
      kind: "markdown",
      value: `**Nil/Null Value**

The null value.

\`\`\`cddl
optional-field = nil / tstr
empty-value = nil
\`\`\``,
    },
  },
  {
    label: "null",
    detail: "null value; alias for nil",
    documentation: {
      kind: "markdown",
      value: `**Null Value (Alias for nil)**

The null value. Alias for \`nil\`.

\`\`\`cddl
maybe-value = null / uint
\`\`\``,
    },
  },
  {
    label: "undefined",
    detail: "undefined value",
    documentation: {
      kind: "markdown",
      value: `**Undefined Value**

The undefined value (distinct from null).

\`\`\`cddl
unset = undefined
\`\`\``,
    },
  },
];

export const controlOperators: ItemDetail[] = [
  {
    label: ".size",
    detail: "controls the size of the target in bytes by the control type",
    documentation: {
      kind: "markdown",
      value: `**Size Control Operator**

Controls the size of the target in bytes by the control type.

\`\`\`cddl
bstr .size 32         ; byte string of exactly 32 bytes
tstr .size (1..100)   ; text string between 1 and 100 bytes
[* any] .size 10      ; array with exactly 10 elements
\`\`\`

**Applies to:** byte strings, text strings, arrays, maps`,
    },
  },
  {
    label: ".bits",
    detail:
      "only the bits specified by the control type are allowed to be set in the target",
    documentation: {
      kind: "markdown",
      value: `**Bits Control Operator**

Only the bits specified by a number in the control type are allowed to be set in the target.

\`\`\`cddl
uint .bits 8    ; 8-bit unsigned integer (0-255)
uint .bits 16   ; 16-bit unsigned integer (0-65535)
bstr .bits 128  ; byte string with exactly 128 bits set
\`\`\`

**Applies to:** unsigned integers, byte strings`,
    },
  },
  {
    label: ".regexp",
    detail: "target text string must match XSD regex value in the control type",
    documentation: {
      kind: "markdown",
      value: `**Regular Expression Control Operator (XSD)**

Target text string must match the XSD (XML Schema Definition) regular expression in the control type.

\`\`\`cddl
email = tstr .regexp "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
uuid = tstr .regexp "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
\`\`\`

**Applies to:** text strings
**Note:** Uses XSD regular expression syntax`,
    },
  },
  {
    label: ".pcre",
    detail:
      "target text string must match PCRE regex value in the control type",
    documentation: {
      kind: "markdown",
      value: `**Regular Expression Control Operator (PCRE)**

Target text string must match the PCRE (Perl Compatible Regular Expression) in the control type.

\`\`\`cddl
phone = tstr .pcre "^\\+?[1-9]\\d{1,14}$"
ip-address = tstr .pcre "^(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$"
\`\`\`

**Applies to:** text strings
**Note:** Uses PCRE regular expression syntax`,
    },
  },
  {
    label: ".cbor",
    detail:
      "target byte string carries a CBOR-encoded data item as indicated by the control type",
    documentation: {
      kind: "markdown",
      value: `**CBOR Encoding Control Operator**

Target byte string carries a CBOR-encoded data item as indicated by the control type.

\`\`\`cddl
embedded-map = bstr .cbor { name: tstr, age: uint }
encoded-array = bstr .cbor [* uint]
\`\`\`

**Applies to:** byte strings
**Purpose:** Embeds structured CBOR data within a byte string`,
    },
  },
  {
    label: ".cborseq",
    detail:
      "target byte string carries a sequence of CBOR-encoded data items as indicated by the control type",
    documentation: {
      kind: "markdown",
      value: `**CBOR Sequence Control Operator**

Target byte string carries a sequence of CBOR-encoded data items as indicated by the control type.

\`\`\`cddl
event-log = bstr .cborseq [* event]
event = { timestamp: uint, type: tstr, data: any }
\`\`\`

**Applies to:** byte strings
**Purpose:** Embeds a sequence of CBOR data items within a byte string`,
    },
  },
  {
    label: ".within",
    detail:
      "left-hand-side target type is a subset of the right-hand-side control type",
    documentation: {
      kind: "markdown",
      value: `**Within Control Operator**

Left-hand-side target type is a subset of the right-hand-side control type.

\`\`\`cddl
small-int = uint .within 0..100
limited-text = tstr .within text
specific-choice = "red" / "green" .within tstr
\`\`\`

**Use case:** Type refinement and validation`,
    },
  },
  {
    label: ".and",
    detail:
      "data item matches both the left-hand-side target type and the right-hand-side control type",
    documentation: {
      kind: "markdown",
      value: `**And Control Operator**

Data item matches both the left-hand-side target type and the right-hand-side control type.

\`\`\`cddl
constrained-number = uint .and (1..100)
tagged-string = tstr .and .cbor text
validated-email = tstr .and .regexp "[^@]+@[^@]+"
\`\`\`

**Use case:** Applying multiple constraints simultaneously`,
    },
  },
  {
    label: ".lt",
    detail:
      "left-hand-side target type is less than the right-hand-side control value",
    documentation: {
      kind: "markdown",
      value: `**Less Than Control Operator**

Left-hand-side target type is less than the right-hand-side control value.

\`\`\`cddl
small-number = uint .lt 100      ; less than 100
negative = int .lt 0             ; negative integers
past-time = time .lt 1609459200  ; before 2021-01-01
\`\`\`

**Applies to:** numeric types (uint, int, float, time)
**Note:** Strict inequality (not equal)`,
    },
  },
  {
    label: ".le",
    detail:
      "left-hand-side target type is less than or equal to the right-hand-side control value",
    documentation: {
      kind: "markdown",
      value: `**Less Than or Equal Control Operator**

Left-hand-side target type is less than or equal to the right-hand-side control value.

\`\`\`cddl
max-hundred = uint .le 100       ; 0 to 100 inclusive
non-positive = int .le 0         ; zero or negative
max-time = time .le 1609459200   ; up to 2021-01-01
\`\`\`

**Applies to:** numeric types (uint, int, float, time)
**Note:** Inclusive inequality (can be equal)`,
    },
  },
  {
    label: ".gt",
    detail:
      "left-hand-side target type is greater than the right-hand-side control value",
    documentation: {
      kind: "markdown",
      value: `**Greater Than Control Operator**

Left-hand-side target type is greater than the right-hand-side control value.

\`\`\`cddl
positive = uint .gt 0            ; positive integers only
future-time = time .gt 1609459200 ; after 2021-01-01
large-number = uint .gt 1000     ; greater than 1000
\`\`\`

**Applies to:** numeric types (uint, int, float, time)
**Note:** Strict inequality (not equal)`,
    },
  },
  {
    label: ".ge",
    detail:
      "left-hand-side target type is greater than or equal to the right-hand-side control value",
    documentation: {
      kind: "markdown",
      value: `**Greater Than or Equal Control Operator**

Left-hand-side target type is greater than or equal to the right-hand-side control value.

\`\`\`cddl
non-negative = uint .ge 0        ; zero or positive
min-age = uint .ge 18            ; 18 or older
current-time = time .ge 1609459200 ; from 2021-01-01 onwards
\`\`\`

**Applies to:** numeric types (uint, int, float, time)
**Note:** Inclusive inequality (can be equal)`,
    },
  },
  {
    label: ".ne",
    detail:
      "left-hand-side target type not equal to the right-hand-side control value",
    documentation: {
      kind: "markdown",
      value: `**Not Equal Control Operator**

Left-hand-side target type not equal to the right-hand-side control value.

\`\`\`cddl
non-zero = uint .ne 0
not-empty = tstr .ne ""
different-value = any .ne null
\`\`\`

**Inequality determined as follows:**
- **Byte strings:** bytewise unidentical
- **Arrays:** different number of elements or unordered pairwise inequality
- **Maps:** different number of key/value pairs or pairwise inequality

**Applies to:** any data type`,
    },
  },
  {
    label: ".eq",
    detail:
      "left-hand-side target type is equal to the right-hand-side control value",
    documentation: {
      kind: "markdown",
      value: `**Equal Control Operator**

Left-hand-side target type is equal to the right-hand-side control value.

\`\`\`cddl
exact-value = uint .eq 42
specific-string = tstr .eq "hello"
null-value = any .eq null
\`\`\`

**Equality determined as follows:**
- **Byte strings:** bytewise identical
- **Arrays:** same number of elements with ordered pairwise equality
- **Maps:** same number of key/value pairs with pairwise equality

**Applies to:** any data type`,
    },
  },
  {
    label: ".default",
    detail: "provides a default value for optional map members",
    documentation: {
      kind: "markdown",
      value: `**Default Value Control Operator**

Provides a default value for optional map members when the key is not present.

\`\`\`cddl
config = {
  timeout: uint .default 30,
  ? retries: uint .default 3,
  ? debug: bool .default false
}
\`\`\`

**Applies to:** optional map members
**Purpose:** Specifies fallback values for missing optional fields`,
    },
  },
  {
    label: ".feature",
    detail: "indicates that the target is part of a named feature set",
    documentation: {
      kind: "markdown",
      value: `**Feature Control Operator**

Indicates that the target is part of a named feature set for conditional inclusion.

\`\`\`cddl
advanced-config = {
  ? experimental-option: bool .feature "experimental"
}
\`\`\`

**Applies to:** any type
**Purpose:** Feature-based conditional schema inclusion`,
    },
  },
];
