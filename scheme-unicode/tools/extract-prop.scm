#! /usr/bin/env gosh

(use srfi-1)
(use srfi-2)
(use srfi-13)
(use srfi-14)
(use gauche.parseopt)

(define (main args)

  (let-args (cdr args)
      ((output "output|o=s")
       (append-file "append|a=s")
       (default "default|d=s")
       (module "module|m=s")
       (no-header? "no-header|n")
       (derived "derive-from|derive|f=s")
       (gauche? "gauche|g")
       (include "include|i=s")
       (exclude "exclude|x=s")
       (autoload-from "autoload-from|A=s")
       (export-all? "export-all|e")
       . rest)

    (define tab (make-hash-table 'string=?))
    (define derivations (make-hash-table 'string=?))
    (define inheritance (make-hash-table 'string=?))
    (define current #f)
    (define base #f)
    (define (prop-name s)
      (regexp-replace-all #/_/ (string-downcase s) "-"))
    (define (keep? s)
      (and (or (not include) (rxmatch include (x->string s)))
           (or (not exclude) (not (rxmatch exclude (x->string s))))))

    ;; return the module name and exported symbols from a file
    (define (get-exports f)
      (define (find-form sym)
        (lambda (x) (and (pair? x) (eq? (car x) sym))))
      (define (defname x)
        (let ((rest (cdr x)))
          (if (pair? rest)
            (let ((form (car rest)))
              (if (pair? form) (car form) form))
            (begin (warn "invalid define: ~S" x) #f))))
      (call-with-input-file f
        (lambda (p)
          (and-let* ((code (port->sexp-list p))
                     (def (find (find-form 'define-module) code))
                     (modname (cadr def)))
            (let ((exports
                   (if (or (any (find-form 'export-all) (cddr def))
                           (any (find-form 'export-all) code))
                     (map defname (append (filter (find-form 'define) (cddr def))
                                          (filter (find-form 'define) code)))
                     (append-map cdr (append (filter (find-form 'export) (cddr def))
                                             (filter (find-form 'export) code))))))
              (cons modname exports))))))

    (define (run)
      (do ((line (read-line) (read-line)))
          ((eof-object? line))
        (rxmatch-case line
          (#/^\s*#\s*derived\s*property\s*:?\s*(\w+)/i (#f prop)
           (set! base #f)
           (set! current (prop-name prop)))
          ;; XXXX we assume the base class is the only one with greater
          ;; than 2 characters (currently true)
          (#/^\s*#\s*generated\s*from\s*(?:.*\+)?\s*(\w{3,})\s*(?:\+.*)?\s*$/i (#f prop)
           (when current (hash-table-put! inheritance current (prop-name prop)))
           (set! base (hash-table-get derivations (prop-name prop) #f)))
          (else
           (and-let*
               ((fields
                 (map
                  (lambda (x) (if (equal? x "") #f x))
                  (map
                   (cut string-trim-both <> char-set:whitespace)
                   (string-split
                    (regexp-replace #/\s*\#.*$/ line "") #\;))))
                (range-str (list-ref fields 0 #f))
                (range-ls (map (cut string->number <> 16)
                               (string-split range-str "..")))
                (range (list (car range-ls) (+ 1 (list-ref range-ls 1 (car range-ls)))))
                (prop (list-ref fields 1 #f)))
             (unless (and base
                          (char-set<=
                           (ucs-range->char-set (car range) (cadr range))
                           base))
               (hash-table-push! tab (prop-name prop) range)))))))

    (define (read-loop)
      ;; build table
      (if (pair? rest)
        (for-each (cut with-input-from-file <> run) rest)
        (run))
      ;; header
      (unless (or append-file no-header?)
        (format #t ";; auto-generated on ~A\n\n"
                (sys-strftime "%c" (sys-localtime (sys-time)))))
      ;; Gauche module start
      (let ((props (sort (filter keep? (hash-table-keys tab)))))
        (when module
          (format #t "(define-module ~A\n  (use srfi-14)\n" module)
          (let ((autoloads
                 (map get-exports
                      (if autoload-from
                        (filter (lambda (s) (not (string=? s "")))
                                (string-split autoload-from #/,/))
                        '()))))
            (for-each
             (lambda (a)
               (format #t "  (autoload ~S" (car a))
               (for-each (cut format #t " ~S" <>) (cdr a))
               (display ")\n"))
             autoloads)
            (cond
              (export-all?
               (display "  (export-all"))
              (else
               (display "  (export")
               (for-each (cut format #t " char-set:~A" <>) props)
               (for-each (cut format #t " ~A" <>) (append-map cdr autoloads))))
            (format #t "))\n(select-module ~A)\n\n" module)
            (format #t "(provide ~S)\n\n" (regexp-replace-all #/\./ module "/"))))
        ;; print char-sets
        (for-each
         (lambda (prop)
           (format #t "(define char-set:~A\n  (char-set-union\n" prop)
           (and-let* ((base (hash-table-get inheritance prop #f)))
             (format #t "   char-set:~A\n" base))
           (for-each
            (lambda (r)
              (if (and gauche? (= (+ 1 (car r)) (cadr r)))
                (format #t "   (char-set (ucs->char #x~X))\n" (car r))
                (format #t "   (ucs-range->char-set #x~X #x~X)\n" (car r) (cadr r))))
            (reverse (hash-table-get tab prop)))
           (display "   ))\n\n"))
         props)
        ;; print default char-set
        (when default
          (format #t "(define char-set:~A\n  (char-set-complement\n   (char-set-union\n"
                  default)
          (for-each
           (lambda (prop) (format #t "    char-set:~A\n" prop))
           (sort (hash-table-keys tab)))
          (display "    )))\n\n"))
        ))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; build derived charsets
    (when derived
      (with-input-from-file derived run)
      (hash-table-for-each
       tab
       (lambda (prop ls)
         (hash-table-put!
          derivations prop
          (apply
           char-set-union
           (map
            (lambda (r) (ucs-range->char-set (car r) (cadr r)))
            ls)))))
      (set! tab (make-hash-table 'string=?)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; run with appropriate output
    (cond (output (with-output-to-file output read-loop))
          (append-file (with-output-to-file append-file read-loop :if-exists :append))
          (else (read-loop)))

    0))
