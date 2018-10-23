#lang racket

(require web-server/templates
         (prefix-in p: "posts.rkt"))

(provide render-tree
         render-post
         render-new
         render-main-page
         render-posted
         render-subject)

;; extract subject from post
(define (render-subject p)
  (define body (p:post-body p))
  (substring body 0 (min 50 (string-length body))))

;; convert posted seconds to string
(define (render-posted p)
  (define current-secs (current-seconds))
  (define posted-secs (p:post-posted p))
  (define duration (- current-secs posted-secs))
  (cond
    [(< duration 60) (string-append (number->string duration) "s ago")]
    [(< duration 3600)  (string-append (number->string (truncate (/ duration 60))) "min ago")]
    [(< duration 86400)  (string-append (number->string (truncate (/ duration 3600))) "h ago")]
    [else  (string-append (number->string (truncate (/ duration 86400))) " days ago")]))

;; render main page
(define (render-main-page userid [post-id ""])
  (let ([p (if (eq? post-id "" )
               #f
               (p:id->post post-id))])
    (include-template "templates/index.html")))

;; render single post with reply form
(define (render-post p)
  (include-template "templates/post.html"))

;; render post form
(define (render-new)
  (include-template "templates/new.html"))

;; render posts and comments as html tree view for given root
;; root can be id, post or list of ids. Default is all top-level posts
(define (render-tree [root (p:all-post-ids)])
  (define op (open-output-string))
  (define ids (cond
                [(list? root) root]
                [(p:post? root) (list (p:post-id root))]
                [else (list root)]))
  (for ([id (in-list ids)])
    (define p (p:id->post id))
    (fprintf op (include-template "templates/tree.html")))
  (get-output-string op))
