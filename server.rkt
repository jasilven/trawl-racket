#lang racket/base

(require racket/list
         web-server/dispatch
         web-server/http
         web-server/http/id-cookie
         web-server/servlet-env
         web-server/templates
         xml
         "posts.rkt")

;; anonymous user name
(define *anon-user* "anonymous")
;; secret salt for id-cookie
(define *cookie-salt* (string->bytes/utf-8 (uid 8)))
;; max length of post body
(define *post-max-len* 1000)
;; *posts* store
(define *posts* (make-posts 500 *post-max-len*))

;; duration in friendly format
(define (duration secs)
  (define dur (- (current-seconds) secs))
  (cond
    [(< dur 60) (string-append (number->string dur) "s ago")]
    [(< dur 3600)  (string-append (number->string (truncate (/ dur 60))) "min ago")]
    [(< dur 86400)  (string-append (number->string (truncate (/ dur 3600))) "h ago")]
    [else  (string-append (number->string (truncate (/ dur 86400))) " days ago")]))

;; render comments tree
(define (render-comment p)
  (include-template "templates/tree.html"))

;; return form binding value from post form or #f if not found
(define (binding-value req key)
  (with-handlers ([exn:fail? (λ (e) #f)])
    (define bindings (request-bindings/raw req))
    (define value (binding:form-value (bindings-assq key bindings)))
    (if value (bytes->string/utf-8 value) #f)))

;; get userid from cookie. if not found return "anonymous"
(define (user-from-cookie req)
  (define id-cookie (request-id-cookie "id" *cookie-salt* req))
  (if id-cookie id-cookie *anon-user*))

;; main request handler
(define (main req [p-id #f])
  (define user (user-from-cookie req))
  (define userid (if (eq? *anon-user* user) (uid 6) user))
  (define ps (posts-values *posts*))
  (define selected (if p-id (posts-ref *posts* p-id) #f))
  (response/full
   200 #"OK"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (cookie->header
          (make-id-cookie "id" *cookie-salt* userid #:path "/")))
   (list (string->bytes/utf-8
          (include-template "templates/main.html")))))


;; new post handler
(define (new req)
  (define body (binding-value req #"Body"))
  (define parent-id (binding-value req #"ReplyTo"))
  (unless (or (equal? body #f)
              (equal? body ""))
    (posts-add *posts*
               (user-from-cookie req)
               (xexpr->string body)
               (if parent-id
                   (xexpr->string parent-id)
                   #f)))
  (main req))

;; logout and invalidate id-cookie
(define (logout req)
  (response/xexpr
   #:cookies (list (logout-id-cookie "id" #:path "/"))
   `(html (head (title "Trawl"))
          (body (h3 "Game Over!!")
                (a [(href "/posts")] "Take me back")))))

;; router config
(define-values (dispatch dispatch-url)
  (dispatch-rules
   [("posts") #:method (or "get" "post") main]
   [("posts" (string-arg)) #:method (or "get" "post") main]
   [("logout") #:method (or "get" "post") logout]
   [("new") #:method "post" new]
   [("reply") #:method "post" new]))

;; run server
(serve/servlet dispatch
               #:servlet-regexp #rx""
               #:servlet-path (dispatch-url main)
               #:port 8000
               #:extra-files-paths (list (build-path (current-directory) "static")))

