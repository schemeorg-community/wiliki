(define-module wiliki.settings-user-name
  (use wiliki.settings)
  (use www.cgi)
  (export wiliki:register-settings-user-name
          wiliki:settings-user-name))
(select-module wiliki.settings-user-name)

(define (wiliki:register-settings-user-name)
  (settings:register! 'user-name
                      user-name-config
                      user-name-parse))

(define (wiliki:settings-user-name default)
  (let ((name (settings:get 'user-name)))
    (if (and name
             (> (string-length name) 0))
        name
        default)))

(define (user-name-parse param)
  (cgi-get-parameter "user-name" param :default #f))

(define (user-name-config)
  `((tr (@ (class "user-name-setting"))
        (td "User name: ")
        (td (input (@ (type "text")
                      (name "user-name")
                      (value ,(or (settings:get 'user-name)
                                  ""))))))))

(provide "wiliki/settings-user-name")
