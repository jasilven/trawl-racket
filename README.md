# trawl-racket
Simple in-memory message board webapp written in racket.

Messages are stored and served from simple fixed size in-memory "datastore" that
uses fifo method for cleaning old messages out of the way. Datastore size and message size
are configurable.

Messages are never persisted anywhere! After messages are posted they will stay in memory and
in the message board until new incoming messages pushes them away or when the server terminates.

All posts are posted anonymously with randomly generated userid that is valid until user ends
the session or decides to abandon the userid.

Uses only racket (v7.0) standard library!

## build
```
$ raco exe server.rkt
```
## run
```
$ ./server
Your Web application is running at http://localhost:8000/posts.
```
## screenshot

![screenshot](screenshot.png)
