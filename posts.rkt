#lang racket

(require file/md5)

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
  (define str (md5 (string-append a b)))
  (substring (bytes->string/utf-8 str) 0 len))

;; unique id for new post
(define (uniq-post-uid f v)
  (define id (f v))
  (if (hash-has-key? (db-posts *db*) id)
      (uniq-post-uid f v)
      id))

;; create and return post and remove oldest if dull db
(define (new-post from body [parent-id #f])
  (define abody (substring body 0 (min (string-length body)
                                       *post-max-len*)))
  (define id (uniq-post-uid uid 8))
  (define apost (post id (current-seconds) from abody '()))
  (define add-comment
    (λ (p) (struct-copy post p
                        [comments (append (post-comments p) (list id))])))
  (call-with-semaphore
   (db-semaphore *db*)
   (λ ()
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
  (define (generate-many-posts)
    (for/list ([i (in-range (* 10 *db-max-size*))])
      (define p (new-post (string-append "test-"
                                         (number->string i))
                          (string-append "test body for post: "
                                         (number->string i))))
      (post-id (id->post (post-id p)))))

  (run-tests
   (test-suite
    "all tests"
    (test-suite "stress test"
                #:before (λ () (db-reset))
                #:after (λ () (db-reset))
                (check-not-exn (λ () (generate-many-posts))))
    (test-suite "basic tests"
                #:before (λ () (db-reset))
                #:after (λ () (db-reset))
                (test-case "testing new-post and id->post"
                  (check-not-exn
                   (λ ()
                     (define p (new-post "from test" "body test"))
                     (define p2 (id->post (post-id p)))
                     (when (not (string=? (post-body p)
                                          "body test"
                                          (post-body p2)))
                       (raise "error: not matching")))))
                (test-false "non existing post" (id->post "not_valid_id"))
                (test-exn "comment to non-existing post"
                          exn:fail? (λ () (new-post "from test" "body test" "not_valid_id"))))
    (test-suite "more basic test"
                #:before (λ () (db-reset))
                #:after (λ () (db-reset))
                (test-case "testing new-post and all-post-ids"
                  (check-not-exn
                   (λ ()
                     (define p1 (new-post "from test" "body test"))
                     (define p2 (new-post "from test" "body test"))
                     (define p3 (new-post "from test" "body test"))
                     (define all-posts (list (post-id p3) (post-id p2) (post-id p1)))
                     (when (not (equal? (all-post-ids) all-posts))
                       (raise "error: not matching"))))))
    (test-suite "threads"
                #:before (λ () (db-reset))
                #:after (λ () (db-reset))
                (test-case "testing post-thread-ids"
                  (check-not-exn
                   (λ ()
                     (define p1 (new-post "from test" "body test"))
                     (define p2 (new-post "from test" "body test" (post-id p1)))
                     (define p3 (new-post "from test" "body test" (post-id p2)))
                     (define p4 (new-post "from test" "body test" (post-id p3)))
                     (define p5 (new-post "from test" "body test" (post-id p2)))
                     (define all-posts (list (post-id p1)
                                             (post-id p2)
                                             (post-id p3)
                                             (post-id p4)
                                             (post-id p5)))
                     (when (not (equal?
                                 (sort (post-thread-ids (post-id p1)) string<?)
                                 (sort all-posts string<?)))
                       (raise "error: not matching"))))))
    (test-suite "testing db cleaning"
                #:before (λ () (db-reset))
                #:after (λ () (db-reset))
                (test-case "generating posts to force cleanup"
                  (check-not-exn (λ () (generate-many-posts)))
                  (check-true (<= (length (all-post-ids))
                                  *db-max-size*))))
    ) 'verbose))
