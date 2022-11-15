# `untyped-nbe`

A simple implementation of untyped lambda calculus using Normalization by Evaluation (NbE) in Haskell.

We use De Bruijn _indices_ for terms and De Bruijn _levels_ for values. In combination with NbE, this completely avoids the need for term reindexing, manual substitution, alpha equivalence, and alpha conversion.

 - The NbE implementation: [`src/Lib.hs`](src/Lib.hs).
 - The unit tests: [`test/Spec.hs`](test/Spec.hs).
