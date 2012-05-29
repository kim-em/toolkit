The net.tqft.toolkit.arithmetic package provides a few basic arithmetic operations missing from the Scala libraries,

* "mathematician's" `mod`, which returns answers in the range 0, ..., n-1, even for negative arguments
* binomial coefficients
* factorials

as well as fast integer factorization. The `Factor` object just uses trial division for numbers up to 40 million, then switches over to the elliptic curve method.
The `Primes` object provides a `Stream` of primes, based on an implementation of Eratosthenes's sieve by Daniel Sobral posted on [stackoverflow](http://stackoverflow.com/a/6824828/82970).
The elliptic curve method is an adaption of the Java applet by Dario Alejandro Alpern accessible at <http://www.alpertron.com.ar/ECM.HTM>. It automatically parallelizes, and runs faster that Mathematica for most numbers. If you're trying to find factors with more than about 25 digits, you'd be advised to use an implementation of the general number field sieve; I don't know of a JVM implementation.
