#! /usr/bin/env gosh

(use gauche.parseopt)
(autoload binary.pack pack)

(define (main args)
  (let-args (cdr args)
      ((pretty? "pretty|p")
       (output "output|o=s")
       . rest)
    (define (run)
      (do ((line (read-line) (read-line)))
          ((eof-object? line))
        (let* ((fields
                (map
                 (lambda (x) (if (equal? x "") #f x))
                 (string-split (regexp-replace #/\s*\#.*$/ line "") #\;)))
               (uc (list-ref fields 12 #f))
               (lc (list-ref fields 13 #f))
               (tc (list-ref fields 14 #f)))
          (when (or uc lc tc)
            (let* ((i (string->number (car fields) 16))
                   (uci (string->number (or uc (car fields)) 16))
                   (lci (string->number (or lc (car fields)) 16))
                   (tci (string->number (or tc uc (car fields)) 16)))
              (if pretty?
                (format #t "~X (~A) ~X (~A) ~X (~A) ~X (~A)\n"
                        i (ucs->char i) uci (ucs->char uci)
                        lci (ucs->char lci) tci (ucs->char tci))
                (pack "L4" (list i uci lci tci))))))))
    (define (read-loop)
      (if (pair? rest)
        (for-each (cut with-input-from-file <> run) rest)
        (run)))
    (if output (with-output-to-file output read-loop) (read-loop))
    0))
