#! /usr/bin/env gosh

(use util.list)
(use gauche.parseopt)

(define (main args)
  (let-args (cdr args)
      ((output "output|o=s")
       . rest)
    (define (make-entry c lc tc uc)
      (define (conv x)
        (if x
          (let ((chars (map (lambda (s) (ucs->char (string->number s 16)))
                            (filter (lambda (s) (not (equal? s "")))
                                    (string-split x #/\s+/)))))
            (if (pair? (cdr chars))
              (list->string chars)
              (car chars)))
          ""))
      (vector (string->number c 16) (conv lc) (conv tc) (conv uc)))
    (define (run)
      (let loop ((line (read-line))
                 (entries '()))
        (cond
          ((eof-object? line)
           (write (list->vector (sort entries (lambda (a b) (< (vector-ref a 0) (vector-ref b 0)))))))
          (else
           (let* ((fields
                   (map
                    (lambda (x) (if (equal? x "") #f x))
                    (string-split (regexp-replace #/\s*\#.*$/ line "") #\;)))
                  (c (list-ref fields 0 #f))
                  (lc (list-ref fields 1 #f))
                  (tc (list-ref fields 2 #f))
                  (uc (list-ref fields 3 #f))
                  (special (list-ref fields 4 #f)))
             ;;(warn "c: ~S lc: ~S tc: ~S uc: ~S" c lc tc uc)
             (if (and (or uc lc tc) (not special))
               (loop (read-line) (cons (make-entry c lc tc uc) entries))
               (loop (read-line) entries)))))))
    (define (read-loop)
      (if (pair? rest)
        (for-each (cut with-input-from-file <> run) rest)
        (run)))
    (if output (with-output-to-file output read-loop) (read-loop))
    0))
