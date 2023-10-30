## Introduction ##

This is version 4 of the Structure Mapping Engine from the Qualitative Reasoning Group at
Northwestern University, led by Ken Forbus.  See the [SME page at
QRG](https://www.qrg.northwestern.edu/software/sme4/index.html) for more information.

I am making some changes to it for my own purposes:

- Replacing its homegrown system-building facility with
  [ASDF](https://asdf.common-lisp.dev/).  This will make it loadable with Quicklisp; I
  plan to submit it to Zach for public accessibility.

- Building it on SBCL, ABCL, and possibly others.

- Making it play better with other Lisp code, e.g., so it doesn't add symbols to package
  `common-lisp-user`, or add nicknames for that package.  I have moved the functions and
  variables that were in `common-lisp-user` to a new package `sme-user`.

- I have added a new package `sme-data` for the vocabulary that SME reasons over, and
  moved the examples into that package; `sme-data` does not "use" (import external symbols
  from) any package, so in the vocabulary and examples files, symbols from any other
  package must be explicitly qualified, with the exception of `t` and `nil` which I have
  imported from `common-lisp`.  This change removes the uses of the non-portable syntax
  mentioned in footnote 1 of `using_sme_v4.pdf`; the code is now portable CL as far as I
  know.

- Replacing its homegrown set implementation, which has O(n) insertion, with FSet; this
  should make it faster, though I haven't analyzed it to see whether it improves the time
  complexity.

There may be more.  This is work in progress.

