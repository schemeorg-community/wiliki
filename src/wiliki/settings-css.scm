(define-module wiliki.settings-css
  (use rfc.cookie)
  (use www.cgi)
  (use wiliki.settings)
  (export wiliki:register-settings-css
          wiliki:settings-css-file))
(select-module wiliki.settings-css)

(define (wiliki:register-settings-css defaults)
  (settings:register! 'css
                      (lambda ()
                        (css-config defaults))
                      css-parse))

(define (wiliki:settings-css-file default)
  (or (settings:get 'css)
      default))

(define (css-parse param)
  (let ((css-select (cgi-get-parameter "css-select" param :default #f))
        (css-name (cgi-get-parameter "css-name" param :default #f)))
    (if (and (string? css-select)
             (not (string-ci=? css-select "file")))
        css-select
        css-name)))

(define (css-config defaults)
  (let ((current (or (settings:get 'css)
                     (ref (with-module wiliki (wiliki))
                          'style-sheet))))
    `((tr (@ (class "css-setting"))
          (td "CSS: ")
          (td (select (@ (name "css-select"))
                      (option (@ (value "file"))
                              "Use URL below")
                      ,@(map (lambda (name+file)
                               (let ((name (car name+file))
                                     (file (cadr name+file)))
                                 `(option (@ (value ,file)
                                             ,@(if (and current
                                                        (string=? current
                                                                  file))
                                                   '((selected #t))
                                                   '()))
                                          ,name)))
                             defaults))))
      (tr (@ (class "css-setting"))
          (td)
          (td "URL: "
              (input (@ (type "text")
                        (name "css-name")
                        (value ,(or current
                                    ""))))
              " (select File above)")))))

(provide "wiliki/settings-css")
