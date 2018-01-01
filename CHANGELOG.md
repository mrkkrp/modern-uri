## Modern URI 0.1.2.1

* Allow Megaparsec 6.4.0.

## Modern URI 0.1.2.0

* Fixed handling of `+` in query strings. Now `+` is parsed as space and
  serialized as `%2b` as per RFC 1866 (paragraph 8.2.1). White space in
  query parameters is serialized as `+`.

## Modern URI 0.1.1.1

* Fixed implementation of `Text.URI.Lens.queryParam` traversal.

## Modern URI 0.1.1.0

* Derived `NFData` for `ParseException`.

* Adjusted percent-encoding in renders so it's only used when absolutely
  necessary. Previously we percent-escaped a bit too much, which, strictly
  speaking, did not make the renders incorrect, but that didn't look nice
  either.

## Modern URI 0.1.0.1

* Updated the readme to include “Quick start” instructions and some
  examples.

## Modern URI 0.1.0.0

* Changed the type of `uriAuthority` from `Maybe Authority` to `Either Bool
  Authority`. This allows to know if URI path is absolute or not without
  duplication of information, i.e. when the `Authority` component is present
  the path is necessarily absolute, otherwise the `Bool` value tells if it's
  absolute (`True`) or relative (`False`).

* Added `isPathAbsolute` in `Text.URI` and the corresponding getter in
  `Text.URI.Lens`.

## Modern URI 0.0.2.0

* Added the `renderStr` and `renderStr'` functions for efficient rendering
  to `String` and `ShowS`.

* Added the `parserBs` that can consume strict `ByteString` streams.

## Modern URI 0.0.1.0

* Initial release.
