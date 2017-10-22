# Modern URI

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/modern-uri.svg?style=flat)](https://hackage.haskell.org/package/modern-uri)
[![Stackage Nightly](http://stackage.org/package/modern-uri/badge/nightly)](http://stackage.org/nightly/package/modern-uri)
[![Stackage LTS](http://stackage.org/package/modern-uri/badge/lts)](http://stackage.org/lts/package/modern-uri)
[![Build Status](https://travis-ci.org/mrkkrp/modern-uri.svg?branch=master)](https://travis-ci.org/mrkkrp/modern-uri)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/modern-uri/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/modern-uri?branch=master)

This is a modern library for working with URIs in Haskell. Compared to the
`uri` package:

* Uses `Text` instead of `String`.
* More type-safe data types that make invalid data unrepresentable.
* Megaparsec for parsing instead of Parsec.
* Parsing and rendering are tuned for performance.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/modern-uri/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2017 Mark Karpov

Distributed under BSD 3 clause license.
