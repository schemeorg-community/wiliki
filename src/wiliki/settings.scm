;;; The module
(define-module wiliki.settings
  (use rfc.base64)
  (use rfc.cookie)
  (use www.cgi)
  (use srfi-1)
  (use srfi-2)
  (export settings:register!
          settings:register-map
          settings:get
          settings:set!
          settings:fold
          settings:parse-cookies!
          cmd-settings))
(select-module wiliki.settings)

;;; The setting registry
(define *setting-table* (make-hash-table))
(define *setting-registry* '())

;;; THUNK should return an SXML expression equivalent to a TD.
(define (settings:register! name thunk parse)
  (set! *setting-registry*
        (cons (list name thunk parse)
              *setting-registry*)))

(define (settings:register-map proc)
  (map (lambda (args)
         (proc (list-ref args 0)
               (list-ref args 1)
               (list-ref args 2)))
       *setting-registry*))

(define (settings:clear)
  (set! *setting-table* (make-hash-table)))

(define (settings:get name)
  (hash-table-get *setting-table*
                  name
                  #f))

(define (settings:set! name value)
  (hash-table-put! *setting-table* name value))

(define (settings:fold proc kons knil)
  (hash-table-fold *setting-table* proc kons knil))

;;; The cookie parser, called for each request.
(define (settings:parse-cookies! name cookies cookie2s)
  (and-let* ((cookies (parse-cookies cookies cookie2s))
             (cookie (assoc name cookies))
             (value (cadr cookie))
             (data (with-error-handler
                       (lambda (e) #f)
                     (lambda ()
                       (read-from-string
                        (base64-decode-string value))))))
    (if (list? data)
        (for-each (lambda (setting)
                    (if (pair? setting)
                        (settings:set! (car setting)
                                       (cdr setting))))
                  data))))

(define (parse-cookies cookie cookie2)
  (append (if cookie2
              (parse-cookie-string cookie2 2)
              '())
          (if cookie
              (parse-cookie-string cookie 1)
              '())))

;;; The settings page
(define (cmd-settings wiliki param)
  (let* ((sc (cgi-get-parameter "sc" param :default #f))
         (cookie (cond
                  ((not sc)
                   '())
                  ((equal? sc "commit")
                   (settings:clear)
                   (param->cookie wiliki param))
                  ((equal? sc "clear")
                   (settings:clear)
                   (clear-cookie wiliki))
                  (else
                   '()))))
    (settings-page
     cookie
     (make (with-module wiliki <wiliki-page>)
       :title (string-append (ref wiliki 'title)
                             ": User Settings")
       :command "c=settings"
       :content (settings-form)))))

;;; Return a new cookie for this CGI parameter
(define (param->cookie wiliki param)
  (construct-cookie-string
   `((,(ref wiliki 'cookie-name)
      ,(base64-encode-string (param->value param))
      :domain ,(ref wiliki 'server-name)
      ; :port ,(x->string (ref wiliki 'server-port))
      ; :path ,(ref wiliki 'script-name)
      :expires ,(+ (sys-time) 31536000)))))

;;; Translate a param to the correspondig cookie value
(define (param->value param)
  (let ((s (open-output-string)))
    (write (delete #f
                   (settings:register-map
                    (lambda (name thunk parse)
                      (let ((val (parse param)))
                        (if val
                            (begin
                              (settings:set! name val)
                              (cons name val))
                            #f)))))
           s)
    (get-output-string s)))

;;; Clear the cookie
(define (clear-cookie wiliki)
  (construct-cookie-string
   `((,(ref wiliki 'cookie-name)
      ""
      :domain ,(ref wiliki 'server-name)
      ; :port ,(x->string (ref wiliki 'server-port))
      ; :path ,(ref wiliki 'script-name)
      :expires 0))))

;;; A new page for settings.
(define (settings-page cookie page)
  (let ((charset (with-module wiliki (output-charset))))
    `("Content-Style-Type: text/css\n"
      ,(cgi-header :content-type #`"text/html; charset=,charset"
                   :cookies cookie)
      ,(with-module wiliki (html-doctype :type :transitional))
      ,((with-module wiliki wiliki:sxml->stree)
        ((with-module wiliki wiliki:format-page)
         page)))))

;;; The form on the settings page.
(define (settings-form)
  (let ((action (with-module wiliki (cgi-name-of (wiliki)))))
    `((form (@ (class "settings-form")
               (method "GET")
               (action ,action))
            (input (@ (type "hidden") (name "c")  (value "settings")))
            (input (@ (type "hidden") (name "sc") (value "commit")))
            (p
             (table (@ (class "settings-table"))
                    ,@(apply append
                             (settings:register-map
                              (lambda (name thunk parse)
                                (thunk)))))
             (input (@ (type "submit")
                       (name "commit")
                       (value "Save")))))
      (h2 "Clear settings")
      (form (@ (class "settings-clear")
               (method "GET")
               (action ,action))
            (input (@ (type "hidden") (name "c")  (value "settings")))
            (input (@ (type "hidden") (name "sc") (value "clear")))
            (input (@ (type "submit")
                      (name "clear")
                      (value "Clear settings")))))))

(provide "wiliki/settings")
