#lang racket/base

(require racket/list
         racket/function
         racket/bool
         file/md5)

(provide (struct-out post)
         uid
         *post-max-len*
         id->post
         new-post
         all-post-ids)

(struct post (id posted from body comments) #:mutable #:transparent)
(struct db (ids posts semaphore) #:mutable #:transparent)

;; create new db
(define (db-new) (db '() (make-hash) (make-semaphore 1)))
;; reset/clear postsdb
(define (db-reset [first-time? #f])
  (if first-time? (db-new) (set! *db* (db-new))))
;; max number of posts/comments
(define *db-max-size* 500)
;; max length of post/comment
(define *post-max-len* 1000)
;; in-memory database for posts
(define *db* (db-reset #t))

;; list of all top-level post ids
(define (all-post-ids)
  (db-ids *db*))

;; list of all post ids for thread
(define (post-thread-ids id [result '()])
  (define ids (if (list? id) id (list id)))
  (cond
    [(empty? ids) result]
    [else
     (define current-id (first ids))
     (define comments (post-comments (id->post current-id)))
     (post-thread-ids (append (rest ids) comments)
                      (append result (list current-id)))]))

;; post for given post-id or #f if not found
(define (id->post id)
  (hash-ref (db-posts *db*) id #f))

;; generic uid of length len
(define (uid len)
  (define a (number->string (random 100000000)))
  (define b (number->string (current-milliseconds)))
  (substring (bytes->string/utf-8
              (md5 (string-append a b)))
             0 len))

;; unique id for new post
(define (uniq-post-uid f v)
  (define id (f v))
  (if (hash-has-key? (db-posts *db*) id)
      (uniq-post-uid f v)
      id))

;; create and return post and remove oldest if dull db
(define (new-post from body [parent-id #f])
  (define abody (substring body
                           0 (min (string-length body) *post-max-len*)))
  (define id (uniq-post-uid uid 8))
  (define apost (post id (current-seconds) from abody '()))
  (define (add-comment a)
    (struct-copy post a [comments (append (post-comments a) (list id))]))
  (call-with-semaphore
   (db-semaphore *db*)
   (thunk
    (hash-set! (db-posts *db*) id apost)
    (if (false? parent-id) ;top level post
        (set-db-ids! *db* (append (list id) (db-ids *db*)))
        (hash-update! (db-posts *db*) parent-id add-comment))
    (when (> (hash-count (db-posts *db*))
             *db-max-size*)
      (set-db-ids! *db* (drop-right (db-ids *db*) 1))
      (for ([id (in-list (post-thread-ids (last (db-ids *db*))))])
        (hash-remove! (db-posts *db*) id)))))
  apost)

;; tests
(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "all tests"
    (test-suite "stress test"
                #:before db-reset
                #:after db-reset
                (check-not-exn
                 (thunk
                  (for ([i (in-range (* 10 *db-max-size*))])
                    (define a (new-post (format "from-~a" (number->string i))
                                        (format "body-~a" (number->string i))))
                    (post-id (id->post (post-id a))))))
                (check-true (<= (length (all-post-ids))
                                *db-max-size*)))
    (test-suite "basic tests"
                #:before db-reset
                #:after db-reset
                (test-case "new-post and id->post"
                  (define a (new-post "from" "body"))
                  (define b (id->post (post-id a)))
                  (check-equal? a b "new-post and id->post")))
    (test-suite "more basic tests"
                #:before db-reset
                #:after db-reset
                (test-case "new-post and all-post-ids"
                  (define a (sort (for/list ([_ (in-range 10)])
                                    (post-id (new-post "from" "body")))
                                  string<?))
                  (define b (sort (all-post-ids) string<?))
                  (check-equal? a b "all-posts-ids")))
    (test-suite "threads"
                #:before db-reset
                #:after  db-reset
                (test-case "post-thread-ids"
                  (define p1 (new-post "from" "body"))
                  (define p2 (new-post "from" "body" (post-id p1)))
                  (define p3 (new-post "from" "body" (post-id p2)))
                  (define p4 (new-post "from" "body" (post-id p3)))
                  (define p5 (new-post "from" "body" (post-id p2)))
                  (define a (sort (for/list ([p (in-list (list p1 p2 p3 p4 p5))])
                                    (post-id p))
                                  string<?))
                  (define b (sort (post-thread-ids (post-id p1)) string<?))
                  (check-equal? a b "post-thread-ids"))))))
