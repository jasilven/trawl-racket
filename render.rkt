#lang racket

(require web-server/templates
         "posts.rkt")

(provide posts-render-tree
         posts-render-post
         posts-render-new
         posts-render-main
         post-render-posted
         post-render-subject)

;; extract subject from post
(define (post-render-subject p)
  (define body (post-body p))
  (substring body 0 (min 50 (string-length body))))

;; convert posted seconds to string
(define (post-render-posted p)
  (define current-secs (current-seconds))
  (define posted-secs (post-posted p))
  (define duration (- current-secs posted-secs))
  (cond
    [(< duration 60) (string-append (number->string duration) "s ago")]
    [(< duration 3600)  (string-append (number->string (truncate (/ duration 60))) "min ago")]
    [(< duration 86400)  (string-append (number->string (truncate (/ duration 3600))) "h ago")]
    [else  (string-append (number->string (truncate (/ duration 86400))) " days ago")]))

;; render main page
(define (posts-render-main userid [post-id ""])
  (let ([p (if (eq? post-id "" )
               #f
               (post-id->post post-id))])
    (include-template "templates/index.html")))

;; render single post with reply form
(define (posts-render-post p)
  (include-template "templates/post.html"))

;; render post form
(define (posts-render-new)
  (include-template "templates/new.html"))

;; render posts and comments as html tree view for given root
;; root can be id, post or list of ids. Default is all top-level posts
(define (posts-render-tree [root (posts-all-ids)])
  (define op (open-output-string))
  (define ids (cond
                [(list? root) root]
                [(post? root) (list (post-id root))]
                [else (list root)]))
  (for ([id (in-list ids)])
    (define p (post-id->post id))
    (fprintf op (include-template "templates/tree.html")))
  (get-output-string op))
