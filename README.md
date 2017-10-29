# Modern URI

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/modern-uri.svg?style=flat)](https://hackage.haskell.org/package/modern-uri)
[![Stackage Nightly](http://stackage.org/package/modern-uri/badge/nightly)](http://stackage.org/nightly/package/modern-uri)
[![Stackage LTS](http://stackage.org/package/modern-uri/badge/lts)](http://stackage.org/lts/package/modern-uri)
[![Build Status](https://travis-ci.org/mrkkrp/modern-uri.svg?branch=master)](https://travis-ci.org/mrkkrp/modern-uri)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/modern-uri/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/modern-uri?branch=master)

This is a modern library for working with URIs in Haskell as per RFC 3986:

https://tools.ietf.org/html/rfc3986

## Motivation

There are already at least three libraries for working with URIs:
[`uri`](https://hackage.haskell.org/package/uri),
[`network-uri`](https://hackage.haskell.org/package/network-uri), and
[`uri-bytestring`](https://hackage.haskell.org/package/uri-bytestring). Why
write one more?

Let's see first about the `uri` and `network-uri` packages (they are quite
similar):

* It uses `String` instead of `Text` or `ByteString`, it is thus
  inefficient.
* The types are not very precise. Query string is represented as `Maybe
  String` for example.
* Uses Parsec under the hood, however does not allow us to use its URI
  parser in a bigger Parsec parser.

Now what about `uri-bytestring`?

* Works with `ByteString`, which totally makes sense because a URI can have
  only ASCII characters in it. However sometimes a URI is a part of a bigger
  document that can contain Unicode characters and so we may need to parse a
  URI from `Text` or render it to `Text`. Ideally, we would like to be able
  to parse from both `Text` and `ByteString` as well to render to both
  `Text` and `ByteString`.
* Does not allow to use its URI parser as part of a bigger parser.
* Provides `newtype` wrappers for different components of URI, but we could
  still put something incorrect inside.
* Absolute and relative URI references have different types, which may or
  may not be handy.
* Provides lenses, but does not provide e.g. traversal for working with
  query parameters selected by their names.

## Features

The `modern-uri` package features:

* Correct by construction `URI` data type. Correctness is ensured by
  guaranteeing that every sub-component of the `URI` record is by itself
  cannot be invalid. This boils down to careful use of types and a set of
  smart constructors for things like scheme, host, etc.
* Textual components in the `URI` data type represented as `Text` rather
  than `ByteString`, because they are percent-decoded and so they can
  contain characters outside of ASCII range (i.e. Unicode). This allows for
  easier manipulation of `URI`s, while encoding and decoding headaches are
  handled by the parsers and renders for you.
* Absolute and relative URIs differ only by the scheme component: if it's
  `Nothing`, then URI is relative, otherwise it's absolute.
* Megaparsec parser that can be used as a standalone smart constructor for
  the `URI` data type (see `mkURI`) as well as be seamlessly integrated into
  a bigger Megaparsec parser.
* The parser performs some normalization, for example it collapses
  consecutive slashes. Some smart constructors such as `mkScheme` and
  `mkHost` also perform normalization. So in a sense URIs are also
  “normalized by construction” to some extent.
* Fast rendering to strict `Text` and `ByteString` as well as to their
  respective `Builder` types.
* Extensive set of lensy helpers for easier manipulation of the nested data
  types (see `Text.URI.Lens`).
* Quasi-quoters for compile-time construction of the `URI` data type and
  refined text types (see `Text.URI.QQ`).

TODO:

* Provide parser that can parse URIs from `ByteString`s.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/modern-uri/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2017 Mark Karpov

Distributed under BSD 3 clause license.
