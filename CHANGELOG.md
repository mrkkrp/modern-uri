## Modern URI 0.1.0.0

* Changed the type of `uriAuthority` from `Maybe Authority` to `Either Bool
  Authority`. This allows to know if URI path is absolute or not without
  duplication of information, i.e. when the `Authority` component is present
  the path is necessarily absolute, otherwise the `Bool` value tells if it's
  absolute (`True`) or relative (`False`).

* Added `isPathAbsolute` in `Text.URI` and corresponding getter in
  `Text.URI.Lens`.

## Modern URI 0.0.2.0

* Added the `renderStr` and `renderStr'` functions for efficient rendering
  to `String` and `ShowS`.

* Added the `parserBs` that can consume strict `ByteString` streams.

## Modern URI 0.0.1.0

* Initial release.
