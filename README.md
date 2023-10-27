## Introduction ##

This is version 4 of the Structure Mapping Engine from the Qualitative Reasoning Group at
Northwestern University, led by Ken Forbus.  See the [SME page at
QRG](https://www.qrg.northwestern.edu/software/sme4/index.html) for more information.

I am making some changes to it for my own purposes:

- Replacing its homegrown system-building facility with
  [ASDF](https://asdf.common-lisp.dev/).  This will make it loadable with Quicklisp; I
  plan to submit it to Zach for public accessibility.

- Making it play a little better with other Lisp code, e.g., so it doesn't add nicknames
  to package `common-lisp-user`.

- Replacing its homegrown set implementation, which has O(n) insertion, with FSet; this
  should make it faster, though I haven't analyzed it to see how large the sets get.

There may be more.  This is work in progress.
