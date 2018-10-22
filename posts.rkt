#lang racket

(require file/md5)

(provide (struct-out post)
         uid
         *post-max-len*
         post-id->post
         posts-new-post
         posts-all-ids)

(struct post (id posted from body comments) #:mutable #:transparent)
(struct db (root posts) #:mutable #:transparent)

;; max number of posts/comments
(define *db-max-size* 500)
;; max length of post/comment
(define *post-max-len* 1000)
;; in-memory database for posts
(define *db* (db '() (make-hash)))

;; reset postsdb
(define (postsdb-reset)
  (set! *db* (db '() (make-hash))))

;; returns list of all top-level post ids
(define (posts-all-ids)
  (db-root *db*))

;; returns all list of post ids for thread
(define (posts-thread-ids root [result '()])
  (define ids (if (list? root) root (list root)))
  (cond
    [(empty? ids) result]
    [else
     (define current-id (first ids))
     (define comments (post-comments (post-id->post current-id)))
     (posts-thread-ids (append (rest ids) comments)
                       (append result (list current-id)))]))

;; remove oldest thread from db until db size <= *db-max-size*
(define (posts-shrink-db)
  (for ([_ (in-range (- (hash-count (db-posts *db*))
                        *db-max-size*))])
    (define ids (posts-thread-ids (last (db-root *db*))))
    (set-db-root! *db* (drop-right (db-root *db*) 1))
    (for ([id (in-list ids)])
      (hash-remove! (db-posts *db*) id))))

;; return post for given post-id or #f if not found
(define (post-id->post post-id)
  (hash-ref! (db-posts *db*)
              post-id
             #f))

;; return generic uid of length len
(define (uid len)
  (define rand (random 100000000))
  (define ms (current-milliseconds))
  (define str (string-append (number->string rand)
                                (number->string ms)))
  (substring (bytes->string/utf-8 (md5 (string->bytes/utf-8 str)))
             0 len))

;; return id for new post that is not used already
(define (post-uid f v)
  (define id (f v))
  (if (hash-has-key? (db-posts *db*) id)
      (post-uid f v)
      id))

;; create and return new post or comment if parent-id given
(define (posts-new-post from body [parent-id #f])
  (define abody (if (> (string-length body) *post-max-len*)
                    (substring body 0 *post-max-len*)
                    body))
  (define id (post-uid uid 8))
  (define apost (post
                 id
                 (current-seconds)
                 from
                 abody
                 '()))
  (hash-set! (db-posts *db*) id apost)
   (define add-comment
     (λ (p) (struct-copy post p
                         [comments (append (post-comments p) (list id))])))
  (cond
    [(false? parent-id) ;top level post
     (set-db-root! *db* (append (list id) (db-root *db*)))]
    [else ;comment to existing post
     (hash-update! (db-posts *db*) parent-id add-comment)])
  (posts-shrink-db)
  apost)

;; tests
(module+ test
  (require rackunit
           rackunit/text-ui)
  (define (generate-many-posts)
    (for/list ([i (in-range (* 10 *db-max-size*))])
      (define p (posts-new-post (string-append "test-"
                                               (number->string i))
                                (string-append "test body for post: "
                                               (number->string i))))
      (post-id (post-id->post (post-id p)))))

  (run-tests
   (test-suite
    "all tests"
    (test-suite "stress test"
                #:before (λ () (postsdb-reset))
                #:after (λ () (postsdb-reset))
     (check-not-exn (λ () (generate-many-posts))))
    (test-suite "basic tests"
                #:before (λ () (postsdb-reset))
                #:after (λ () (postsdb-reset))
                (test-case "testing posts-new-post and post-id->post"
                  (check-not-exn
                   (λ ()
                     (define p (posts-new-post "from test" "body test"))
                     (define p2 (post-id->post (post-id p)))
                     (when (not (string=? (post-body p)
                                          "body test"
                                          (post-body p2)))
                       (raise "error: not matching")))))
                (test-false "non existing post" (post-id->post "not_valid_id"))
                (test-exn "comment to non-existing post"
                          exn:fail? (λ () (posts-new-post "from test" "body test" "not_valid_id"))))
    (test-suite "more basic test"
                #:before (λ () (postsdb-reset))
                #:after (λ () (postsdb-reset))
                (test-case "testing posts-new-post and posts-all-ids"
                  (check-not-exn
                   (λ ()
                     (define p1 (posts-new-post "from test" "body test"))
                     (define p2 (posts-new-post "from test" "body test"))
                     (define p3 (posts-new-post "from test" "body test"))
                     (define all-posts (list (post-id p3) (post-id p2) (post-id p1)))
                     (when (not (equal? (posts-all-ids) all-posts))
                       (raise "error: not matching"))))))
    (test-suite "threads"
                #:before (λ () (postsdb-reset))
                #:after (λ () (postsdb-reset))
                (test-case "testing posts-thread-ids"
                  (check-not-exn
                   (λ ()
                     (define p1 (posts-new-post "from test" "body test"))
                     (define p2 (posts-new-post "from test" "body test" (post-id p1)))
                     (define p3 (posts-new-post "from test" "body test" (post-id p2)))
                     (define p4 (posts-new-post "from test" "body test" (post-id p3)))
                     (define p5 (posts-new-post "from test" "body test" (post-id p2)))
                     (define all-posts (list (post-id p1)
                                             (post-id p2)
                                             (post-id p3)
                                             (post-id p4)
                                             (post-id p5)))
                     (when (not (equal?
                                 (sort (posts-thread-ids (post-id p1)) string<?)
                                 (sort all-posts string<?)))
                       (raise "error: not matching"))))))
    (test-suite "testing db cleaning"
                #:before (λ () (postsdb-reset))
                #:after (λ () (postsdb-reset))
                (test-case "generating posts to force cleanup"
                  (check-not-exn (λ () (generate-many-posts)))
                  (check-true (<= (length (posts-all-ids))
                                  *db-max-size*))))
    ) 'verbose))
