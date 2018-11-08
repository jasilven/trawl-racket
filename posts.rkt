#lang racket/base

(require file/md5
         racket/bool
         racket/function
         racket/list)

(provide (struct-out post)
         uid
         make-posts
         posts-values
         posts-ref
         posts-add)

(struct post (id posted from body comments) #:mutable #:transparent)
(struct pdb (max-size max-post-len ids posts semaphore) #:mutable #:transparent)

;; make new posts store
(define (make-posts [size 500] [max-post-len 1000])
  (pdb size max-post-len '() (make-hash) (make-semaphore 1)))

;; returns a list of all posts from ps
(define (posts-values ps)
  (call-with-semaphore
   (pdb-semaphore ps)
   (thunk
    (for*/list ([id (in-list (pdb-ids ps))])
      (let loop ([pid id])
        (define p (hash-ref (pdb-posts ps) pid))
        (define comments (post-comments p))
        (cond
          [(empty? comments) (list p)]
          [else (append (list p) (map loop comments))]))))))

;; find post for given id or #f if not found
(define (posts-ref ps id)
  (call-with-semaphore
   (pdb-semaphore ps)
   (thunk (hash-ref (pdb-posts ps) id #f))))

;; list of all post ids for thread
;; not thread safe!
(define (-posts-thread-ids ps id)
  (let loop ([ids (if (list? id) id (list id))]
             [result '()])
    (cond
      [(empty? ids) result]
      [else
       (define current-id (first ids))
       (define comments (post-comments (hash-ref (pdb-posts ps) current-id)))
       (loop (append (rest ids) comments)
             (append result (list current-id)))])))

;; create and return post, remove oldest post
(define (posts-add ps from body [parent-id #f])
  (define abody (substring body
                           0 (min (string-length body)
                                  (pdb-max-post-len ps))))
  (define id (posts-new-uid ps uid 8))
  (define apost (post id (current-seconds) from abody '()))
  (define (add-comment a)
    (struct-copy post a [comments (append (post-comments a) (list id))]))
  (call-with-semaphore
   (pdb-semaphore ps)
   (thunk
    (hash-set! (pdb-posts ps) id apost)
    (if (false? parent-id) ;top level post
        (set-pdb-ids! ps (append (list id) (pdb-ids ps)))
        (hash-update! (pdb-posts ps) parent-id add-comment))
    (when (> (hash-count (pdb-posts ps))
             (pdb-max-size ps))
      (for ([id (in-list (-posts-thread-ids ps (last (pdb-ids ps))))])
        (hash-remove! (pdb-posts ps) id))
      (set-pdb-ids! ps (drop-right (pdb-ids ps) 1)))))
  apost)

;; generic uid of length len
(define (uid len [c 0])
  (define a (number->string (random 100000000)))
  (define b (number->string (current-milliseconds)))
  (substring (bytes->string/utf-8
              (md5 (string-append a b (number->string c))))
             0 len))

;; unique id for new post
(define (posts-new-uid ps fn len)
  (call-with-semaphore
   (pdb-semaphore ps)
   (thunk
    (let loop ([count 1])
      (when (> count 10) (error "unable to generate unique post id"))
      (define id (fn len count))
      (if (hash-has-key? (pdb-posts ps) id)
          (loop (add1 count))
          id)))))



(define (jatest)
  (define pp (make-posts))
  (define p1 (posts-add pp "eka" "eka"))
  (define p2 (posts-add pp "toka" "toka"))
  (define p1ali (posts-add pp "kolmas" "kolmas" (post-id p1)))
  (define p4 (posts-add pp "nel" "nel"))
  (displayln (-posts-thread-ids pp (post-id p1)))
  (posts-values pp))


;; tests
(module+ test
  (require rackunit)
  (test-case "add many"
    (check-not-exn
     (thunk
      (define ps (make-posts))
      (for ([i (in-range (* 10 (pdb-max-size ps)))])
        (define p (posts-add ps (format "from-~a" (number->string i))
                             (format "body-~a" (number->string i))))
        (post-id (posts-ref ps (post-id p)))))))

  (test-case "add many 2 "
    (define ps (make-posts))
    (for ([i (in-range (* 10 (pdb-max-size ps)))])
      (posts-add ps (format "from-~a" (number->string i))
                 (format "body-~a" (number->string i))))
    (check-equal? (pdb-max-size ps) (length (pdb-ids ps))))

  (test-case "posts-add and posts-ref"
    (let ([ps (make-posts)])
      (define a (posts-add ps "from" "body"))
      (define b (posts-ref ps (post-id a)))
      (check-equal? a b)))

  (test-case "add post"
    (let ([ps (make-posts)])
      (define p1 (sort (for/list ([_ (in-range 10)])
                         (post-id (posts-add ps "from" "body")))
                       string<?))
      (define p2 (sort (pdb-ids ps) string<?))
      (check-equal? p1 p2)))

  (test-case "post-thread-ids"
    (let ([ps (make-posts)])
      (define p1 (posts-add ps "from" "body"))
      (define p2 (posts-add ps "from" "body" (post-id p1)))
      (define p3 (posts-add ps "from" "body" (post-id p2)))
      (define p4 (posts-add ps "from" "body" (post-id p3)))
      (define p5 (posts-add ps "from" "body" (post-id p2)))
      (define a (sort (for/list ([ps (in-list (list p1 p2 p3 p4 p5))])
                        (post-id ps))
                      string<?))
      (define b (sort (-posts-thread-ids ps (post-id p1)) string<?))
      (check-equal? a b))))

