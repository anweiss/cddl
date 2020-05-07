import { MarkupContent } from 'vscode-languageserver';

interface ItemDetail {
	label: string;
	detail: string;
	documentation?: string | MarkupContent;
}

export const standardPrelude: ItemDetail[] = [
	{ label: "any", detail: "any data type" },
	{ label: "uint", detail: "unsigned integer data type" },
	{ label: "nint", detail: "negative integer data type" },
	{ label: "int", detail: "integer data type" },
	{ label: "bstr", detail: "byte string data type" },
	{ label: "bytes", detail: "byte string data type; alias for bstr" },
	{ label: "tstr", detail: "text string data type" },
	{ label: "text", detail: "text string data type; alias for tstr" },
	{ label: "tdate", detail: "date/time string data type" },
	{ label: "time", detail: "time in seconds relative to UNIX epoch data type" },
	{ label: "number", detail: "integer or floating point number data type" },
	{ label: "biguint", detail: "large unsigned integer encoded as a byte string data type" },
	{ label: "bignint", detail: "large negative integer encoded as a byte string data type" },
	{ label: "bigint", detail: "large integer encoded as a byte string data type" },
	{ label: "integer", detail: "alias for int or bigint data type" },
	{ label: "unsigned", detail: "alias for uint or biguint data type" },
	{ label: "decfrac", detail: "decimal fraction data type" },
	{ label: "bigfloat", detail: "bitfloat data type" },
	{ label: "eb64url", detail: "expected conversion of byte string to base64url-encoded text string data type" },
	{ label: "eb64legacy", detail: "expected conversion of byte string to base64-encoded text string data type" },
	{ label: "eb16", detail: "expected conversion of byte string to base16-encoded text string data type" },
	{ label: "encoded-cbor", detail: "embedded CBOR data item" },
	{ label: "uri", detail: "URI text string data type" },
	{ label: "b64url", detail: "base64url-encoded text string data type" },
	{ label: "b64legacy", detail: "base64-encoded text string data type" },
	{ label: "regexp", detail: "regular expression in PCRE or JavaScript syntax data type" },
	{ label: "mime-message", detail: "MIME message data type, including all headers" },
	{ label: "cbor-any", detail: "self-described CBOR data type" },
	{ label: "float16", detail: "16-bit floating point value" },
	{ label: "float32", detail: "32-bit floating point value" },
	{ label: "float64", detail: "64-bit floating point value" },
	{ label: "float16-32", detail: "16- or 32-bit floating point value" },
	{ label: "float32-64", detail: "32- or 64-bit floating point value" },
	{ label: "float", detail: "floating point value" },
	{ label: "false", detail: "false" },
	{ label: "true", detail: "true" },
	{ label: "bool", detail: "boolean" },
	{ label: "nil", detail: "null value" },
	{ label: "null", detail: "null value; alias for nil" },
	{ label: "undefined", detail: "undefined value" }
];

export const controlOperators: ItemDetail[] = [
	{ label: ".size", detail: "controls the size of the target in bytes by the control type" },
	{ label: ".bits", detail: "only the bits number by a number in the control type are allowed to be set in the target" },
	{ label: ".regexp", detail: "target text string must match XSD regex value in the control type" },
	{ label: ".pcre", detail: "target text string must match PCRE regex value in the control type" },
	{ label: ".cbor", detail: "target byte string carries a CBOR-encoded data item as indicated by the control type" },
	{ label: ".cborseq", detail: "target byte string carries a sequence of CBOR-encoded data items as indicated by the control type" },
	{ label: ".within", detail: "left-hand-side target type is a subset of the right-hand-side control type" },
	{ label: ".and", detail: "data item matches both the left-hand-side target type and the right-hand-side control type" },
	{
		label: ".lt",
		detail: "left-hand-side target type is less than the right-hand-side control value",
		documentation: `left-hand-side target type is less than the right-hand-side control value

only supports numeric types`
	},
	{
		label: ".le",
		detail: "left-hand-side target type is less than or equal to the right-hand-side control value",
		documentation: `left-hand-side target type is less than or equal to the right-hand-side control value

only supports numeric types`
	},
	{
		label: ".gt",
		detail: "left-hand-side target type is greater than the right-hand-side control value",
		documentation: `left-hand-side target type is greater than the right-hand-side control value

only supports numeric types`
	},
	{
		label: ".ge",
		detail: "left-hand-side target type is greater than or equal to the right-hand-side control value",
		documentation: "only supports numeric types"
	},
	{
		label: ".ne",
		detail: "left-hand-side target type not equal to the right-hand-side control value",
		documentation: `left-hand-side target type not equal to the right-hand-side control value

inequality determined as follows:

- bytewise/byte string unidentical
- arrays that don't have the same number of elements and there is unordered pairwise inequalily
- maps that don't have the same number of key/value pairs and there is pairwise inequality between the key/value pairs`
	},
];