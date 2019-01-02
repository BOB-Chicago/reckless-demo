---
version: alpha/1.1
---

Recklessly exploring micropayments
====

This project implements some simple paywalled functionality.  For the BOB 
meetup, the main use case is donations.  With lightning, microdonations of less 
than a cent are possible.  Besides donations, users can create and fill out 
surveys; and shop at a store.  The backend supports paying to store a blob at a 
daily rate, but it is not implemented in the UI yet.


Principles
----

* Users are opaque to the server.
* Clients can connect many servers, if they want to do the bookkeeping.


Build instructions
----

The software consists of a Haskell back end and a vanilla HTML/JS/CSS front 
end.  After cloning the repo, invoke

```
$ cabal v2-build
$ cabal v2-exec bob-services-exe
```

and you will see a very terse summary of the configuration options.   The 
program is meant to run with an instance of LND and makes calls to its REST 
api.
