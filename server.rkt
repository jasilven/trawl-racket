#lang racket

(require web-server/dispatch
         web-server/http
         web-server/http/id-cookie
         web-server/servlet-env
         xml
         "posts.rkt"
         "render.rkt")

;; secret salt for id-cookie
(define cookie-salt (string->bytes/utf-8 (uid 8)))

;; main response writer
(define (main req [post-id ""])
  (define user (user-from-cookie req))
  (define userid (if (eq? "anonymous" user) (uid 6) user))
  (response/full
   200 #"OK"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (cookie->header (make-id-cookie "id"
                                         cookie-salt
                                         userid
                                         #:path "/")))
   (list (string->bytes/utf-8
          (posts-render-main userid post-id)))))

;; extract form binding value from post form
(define (binding-value req key)
  (define bindings (request-bindings/raw req))
  (define value (binding:form-value (bindings-assq key bindings)))
  (bytes->string/utf-8 value))

;; get userid from cookie. if not found return "anonymous"
(define (user-from-cookie req)
  (define id-cookie
    (request-id-cookie "id" cookie-salt req))
  (if id-cookie id-cookie "anonymous"))

;; handle reply
(define (reply req)
  (posts-new-post (user-from-cookie req)
                  (xexpr->string (binding-value req #"Body"))
                  (xexpr->string (binding-value req #"ReplyTo")))
  (main req))

;; handle new post
(define (new req)
  (posts-new-post (user-from-cookie req)
                  (xexpr->string (binding-value req #"Body")))
  (main req))

;; logout and invalidate id-cookie
(define (logout req)
  (response/full
   200 #"OK"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (cookie->header (logout-id-cookie "id" #:path "/")))
   (list #"<html><body><h3>Game Over!</h3>"
         #"<a href=\"/posts\">Take me back</a>"
         #"</body></html>")))

;; router config
(define-values (dispatch dispatch-url)
  (dispatch-rules
   [("posts") #:method (or "get" "post") main]
   [("posts" (string-arg)) #:method (or "get" "post") main]
   [("logout") #:method (or "get" "post") logout]
   [("new") #:method "post" new]
   [("reply") #:method "post" reply]))

;; run server
(serve/servlet
 dispatch
 #:servlet-regexp #rx""
 #:servlet-path "/posts"
 #:port 8000
 #:extra-files-paths (list (build-path (current-directory) "static")))
