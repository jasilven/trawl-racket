#lang racket

(require web-server/dispatch
         web-server/http
         web-server/http/id-cookie
         web-server/servlet-env
         xml
         (prefix-in p: "posts.rkt")
         (prefix-in r: "render.rkt"))

;; secret salt for id-cookie
(define cookie-salt (string->bytes/utf-8 (p:uid 8)))

;; main response writer
(define (main req [post-id ""])
  (define user (user-from-cookie req))
  (define userid (if (eq? "anonymous" user) (p:uid 6) user))
  (response/full
   200 #"OK"
   (current-seconds) TEXT/HTML-MIME-TYPE
   (list (cookie->header (make-id-cookie "id"
                                         cookie-salt
                                         userid
                                         #:path "/")))
   (list (string->bytes/utf-8
          (r:render-main-page userid post-id)))))

;; return form binding value from post form or #f if not found
(define (binding-value req key)
  (with-handlers ([exn:fail? (λ (e) #f)]) 
    (define bindings (request-bindings/raw req))
    (define value (binding:form-value (bindings-assq key bindings)))
    (if (false? value) #f (bytes->string/utf-8 value))))

;; get userid from cookie. if not found return "anonymous"
(define (user-from-cookie req)
  (define id-cookie (request-id-cookie "id" cookie-salt req))
  (if id-cookie id-cookie "anonymous"))

;; handle new post
(define (new req)
  (define msg (binding-value req #"Body"))
  (define parent-id (binding-value req #"ReplyTo"))
  (unless (or (equal? msg #f)
              (equal? msg ""))
    (if (false? parent-id)
        (p:new-post (user-from-cookie req)
                    (xexpr->string msg))
        (p:new-post (user-from-cookie req)
                    (xexpr->string msg)
                    (xexpr->string parent-id))))
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
(serve/servlet
 dispatch
 #:servlet-regexp #rx""
 #:servlet-path "/posts"
 #:port 8000
 #:extra-files-paths (list (build-path (current-directory) "static")))
