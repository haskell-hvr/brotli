# brotli Haskell library

[Brotli](http://brotli.org)
([RFC7932](https://tools.ietf.org/html/rfc7932))
is a generic-purpose lossless compression algorithm suitable for
[HTTP compression](https://en.wikipedia.org/wiki/HTTP_compression)
that compresses data using a combination of a modern variant of the
LZ77 algorithm, Huffman coding and 2nd order context modeling.

This package provides a pure interface for compressing and
decompressing Brotli streams of data represented as lazy
`ByteString`s. A monadic incremental interface is provided as well.
This package relies on Google's C implementation.

The following packages are based on this package and provide
API support for popular streaming frameworks:

  * [brotli-streams](https://hackage.haskell.org/package/brotli-streams)
    for [io-streams](https://hackage.haskell.org/package/io-streams)

  * [pipes-brotli](https://hackage.haskell.org/package/pipes-brotli)
    for [pipes](https://hackage.haskell.org/package/pipes)

  * [brotli-conduit](https://hackage.haskell.org/package/brotli-conduit)
    for [conduit](https://hackage.haskell.org/package/conduit)
