;;; wiliki/banned-content.scm -- Handling of banned content
;;; See http://www.emacswiki.org/cw/BannedContentDiscussion

(define-module wiliki.banned-content
  (use srfi-13)
  (use wiliki.db)
  (use wiliki)
  (export banned-content-page?
          banned-content-passphrase?
          banned-content))
(select-module wiliki.banned-content)

;;; Is this the banned content page?
(define (banned-content-page? wiliki pagename)
  (let ((bcp (ref wiliki 'banned-content-page)))
    (and bcp
         (string-ci=? pagename bcp))))

(define (banned-content-passphrase? wiliki logmsg)
  (let ((bcpp (ref wiliki 'banned-content-passphrase)))
    (and bcpp
         (string-prefix? bcpp logmsg))))

(define (banned-content wiliki content)
  (define (collect-regex line seed)
    (let ((in-pre? (car seed))
          (regexes (cdr seed)))
      (cond
       ((string=? "}}}" line)
        (cons #f regexes))
       ((and (not in-pre?)
             (string=? "{{{" line))
        (cons #t regexes))
       ((string=? "" line)
        seed)
       (in-pre?
        (cons #t
              (cons line regexes)))
       (else
        seed))))
  (let* ((name (ref wiliki 'banned-content-page))
         (page (and name
                    (wiliki-db-get name))))
    (and page
         (let loop ((rxs (cdr (wiliki:page-lines-fold page
                                                      collect-regex
                                                      (cons #f '())))))
           (cond
            ((null? rxs)
             #f)
            ((rxmatch (car rxs) content)
             (car rxs))
            (else
             (loop (cdr rxs))))))))
