;;;
;;; wiliki/macro.scm - macro handling (to be autoloaded)
;;;
;;;  Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
;;;
;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without restriction,
;;;  including without limitation the rights to use, copy, modify,
;;;  merge, publish, distribute, sublicense, and/or sell copies of
;;;  the Software, and to permit persons to whom the Software is
;;;  furnished to do so, subject to the following conditions:
;;;
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
;;;  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
;;;  OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;;;  IN THE SOFTWARE.
;;;
;;; $Id: macro.scm,v 1.27 2004/03/22 11:44:35 shirok Exp $

(define-module wiliki.macro
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use util.list)
  (use wiliki.format)
  (use wiliki.db)
  (extend wiliki)
  (export handle-reader-macro handle-writer-macro
          handle-virtual-page virtual-page?))
(select-module wiliki.macro)
(use srfi-19)

;; Macro alist

(define *reader-macro-alist* '())
(define *writer-macro-alist* '())
(define *virtual-page-alist* '())

;; 'Macro' is a procedure that takes arguments, and should return [SXML].
;; For backward compatibility, it is allowed to return Stree as well.

(define (wrap-macro-output output)
  (if (and (proper-list? output)
           (every (lambda (node)
                    (or (string? node)
                        (and (pair? node) (symbol? (car node)))))
                  output))
    output ;; it's likely an SXML list
    `((stree ,@output)))) ;;otherwise, wrap it by stree node

;;----------------------------------------------
;; API called from main WiLiKi system
;;

(define (handle-reader-macro name)
  (let1 args (string-tokenize name)
    (handle-expansion name
                      (lambda () (assoc (car args) *reader-macro-alist*))
                      (lambda (p) (apply (cdr p) (cdr args))))))

(define (handle-writer-macro name)
  (let1 args (string-tokenize name)
    (handle-expansion name
                      (lambda () (assoc (car args) *writer-macro-alist*))
                      (lambda (p) (apply (cdr p) (cdr args))))))

(define (handle-virtual-page name)
  (make <wiliki-page>
    :title name
    :content (handle-expansion name
                               (lambda () (get-virtual-page name))
                               (lambda (p) ((cdr p) name)))))

(define (handle-expansion name finder applier)
  (with-error-handler
      (lambda (e)
        (if (positive? (debug-level (wiliki)))
          `((pre (@ (class "macroerror"))
                 ,#`"Macro error in [[,|name|]]:\n"
                 ,(call-with-output-string
                    (cut with-error-to-port <>
                         (cut report-error e))))
            ,(unrecognized name))))
    (lambda ()
      (wrap-macro-output
       (cond ((finder) => applier)
             (else (unrecognized name)))))))

;;----------------------------------------------
;; Utility to define macros
;;

(define (unrecognized name)
  (list #`"[[,name]]"))

(define-syntax define-reader-macro
  (syntax-rules ()
    ((_ (name . args) . body)
     (set! *reader-macro-alist*
           (let ((sname (string-append "$$" (symbol->string 'name))))
             (acons sname
                    (lambda p
                      (if (arity-matches? p 'args)
                          (receive args (apply values p) . body)
                          (unrecognized sname)))
                    *reader-macro-alist*))))
    ))

(define-syntax define-writer-macro
  (syntax-rules ()
    ((_ (name . args) . body)
     (set! *writer-macro-alist*
           (let ((sname (string-append "$" (symbol->string 'name))))
             (acons sname
                    (lambda p
                      (if (arity-matches? p 'args)
                          (receive args (apply values p) . body)
                          (unrecognized sname)))
                    *writer-macro-alist*))))
    ))

(define-syntax define-virtual-page
  (syntax-rules ()
    ((_ (expr (var ...)) . body)
     (set! *virtual-page-alist*
	   (acons expr
		  (lambda p
		    (rxmatch-if (rxmatch expr (car p)) (var ...)
                      (receive args (apply values p) . body)
                      (unrecognized (regexp->string expr))))
		  *virtual-page-alist*)))
    ))

(define (get-virtual-page name)
  (find (lambda (e) (rxmatch (car e) name)) *virtual-page-alist*))

(define (virtual-page? name)
  (not (not (get-virtual-page name))))

(define (arity-matches? list formals)
  (cond ((null? list)
         (or (null? formals) (not (pair? formals))))
        ((null? formals) #f)
        ((pair? formals) (arity-matches? (cdr list) (cdr formals)))
        (else #t)))

;;----------------------------------------------
;; Writer macro definitions
;;

(define-writer-macro (date)
  (list (wiliki:format-time (sys-time))))

;; sample
(define-reader-macro (srfi n)
  `((a (@ (href ,(format "http://srfi.schemers.org/srfi-~a/srfi-~a.html" n n)))
       ,(format "srfi-~a" n))))

(define-reader-macro (r5rs s)
  (let ((entry (assoc s r5rs-index)))
    (if s
        `((a (@ (href ,(format "http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-~a"
                               (cadr entry))))
             ,s))
        (list s))))

(define r5rs-index
  '(("'" "7.html#%_idx_88")
    ("*" "9.html#%_idx_280")
    ("+" "9.html#%_idx_278")
    ("," "7.html#%_idx_156")
    (",@" "7.html#%_idx_158")
    ("-" "9.html#%_idx_282")
    ("..." "7.html#%_idx_186")
    ("/" "9.html#%_idx_284")
    (";" "5.html#%_idx_24")
    ("<" "9.html#%_idx_256")
    ("<=" "9.html#%_idx_260")
    ("=" "9.html#%_idx_254")
    ("=>" "7.html#%_idx_110")
    (">" "9.html#%_idx_258")
    (">=" "9.html#%_idx_262")
    ("`" "7.html#%_idx_160")
    ("abs" "9.html#%_idx_286")
    ("acos" "9.html#%_idx_326")
    ("and" "7.html#%_idx_118")
    ("angle" "9.html#%_idx_344")
    ("append" "9.html#%_idx_420")
    ("apply" "9.html#%_idx_556")
    ("asin" "9.html#%_idx_324")
    ("assoc" "9.html#%_idx_438")
    ("assq" "9.html#%_idx_434")
    ("assv" "9.html#%_idx_436")
    ("atan" "9.html#%_idx_328")
    ("#b" "9.html#%_idx_228")
    ("backquote" "7.html#%_idx_152")
    ("begin" "7.html#%_idx_136")
    ("binding" "6.html#%_idx_32")
    ("binding construct" "6.html#%_idx_34")
    ("boolean?" "6.html#%_idx_46")
    ("bound" "6.html#%_idx_40")
    ("caar" "9.html#%_idx_402")
    ("cadr" "9.html#%_idx_404")
    ("call" "7.html#%_idx_90")
    ("call by need" "7.html#%_idx_146")
    ("call-with-current-continuation" "9.html#%_idx_566")
    ("call-with-input-file" "9.html#%_idx_588")
    ("call-with-output-file" "9.html#%_idx_590")
    ("call-with-values" "9.html#%_idx_574")
    ("car" "9.html#%_idx_392")
    ("case" "7.html#%_idx_114")
    ("cdddar" "9.html#%_idx_406")
    ("cddddr" "9.html#%_idx_408")
    ("cdr" "9.html#%_idx_396")
    ("ceiling" "9.html#%_idx_304")
    ("char->integer" "9.html#%_idx_480")
    ("char-alphabetic?" "9.html#%_idx_470")
    ("char-ci<=?" "9.html#%_idx_466")
    ("char-ci<?" "9.html#%_idx_462")
    ("char-ci=?" "9.html#%_idx_460")
    ("char-ci>=?" "9.html#%_idx_468")
    ("char-ci>?" "9.html#%_idx_464")
    ("char-downcase" "9.html#%_idx_486")
    ("char-lower-case?" "9.html#%_idx_478")
    ("char-numeric?" "9.html#%_idx_472")
    ("char-ready?" "9.html#%_idx_620")
    ("char-upcase" "9.html#%_idx_484")
    ("char-upper-case?" "9.html#%_idx_476")
    ("char-whitespace?" "9.html#%_idx_474")
    ("char<=?" "9.html#%_idx_456")
    ("char<?" "9.html#%_idx_452")
    ("char=?" "9.html#%_idx_450")
    ("char>=?" "9.html#%_idx_458")
    ("char>?" "9.html#%_idx_454")
    ("char?" "6.html#%_idx_54")
    ("close-input-port" "9.html#%_idx_608")
    ("close-output-port" "9.html#%_idx_610")
    ("combination" "7.html#%_idx_94")
    ("comma" "7.html#%_idx_154")
    ("comment" "5.html#%_idx_22")
    ("complex?" "9.html#%_idx_242")
    ("cond" "7.html#%_idx_106")
    ("cons" "9.html#%_idx_390")
    ("constant" "6.html#%_idx_72")
    ("continuation" "9.html#%_idx_570")
    ("cos" "9.html#%_idx_320")
    ("current-input-port" "9.html#%_idx_596")
    ("current-output-port" "9.html#%_idx_598")
    ("#d" "9.html#%_idx_232")
    ("define" "8.html#%_idx_190")
    ("define-syntax" "8.html#%_idx_198")
    ("definition" "8.html#%_idx_188")
    ("delay" "7.html#%_idx_142")
    ("denominator" "9.html#%_idx_300")
    ("display" "9.html#%_idx_624")
    ("do" "7.html#%_idx_138")
    ("dotted pair" "9.html#%_idx_374")
    ("dynamic-wind" "9.html#%_idx_576")
    ("#e" "9.html#%_idx_236")
    ("else" "7.html#%_idx_108")
    ("empty list" "6.html#%_idx_64")
    ("eof-object?" "9.html#%_idx_618")
    ("eq?" "9.html#%_idx_216")
    ("equal?" "9.html#%_idx_218")
    ("equivalence predicate" "9.html#%_idx_208")
    ("eqv?" "9.html#%_idx_210")
    ("error" "4.html#%_idx_8")
    ("escape procedure" "9.html#%_idx_568")
    ("eval" "9.html#%_idx_578")
    ("even?" "9.html#%_idx_272")
    ("exact" "9.html#%_idx_212")
    ("exact->inexact" "9.html#%_idx_346")
    ("exact?" "9.html#%_idx_250")
    ("exactness" "9.html#%_idx_224")
    ("exp" "9.html#%_idx_314")
    ("expt" "9.html#%_idx_332")
    ("#f" "9.html#%_idx_356")
    ("false" "6.html#%_idx_68")
    ("floor" "9.html#%_idx_302")
    ("for-each" "9.html#%_idx_560")
    ("force" "9.html#%_idx_562")
    ("gcd" "9.html#%_idx_294")
    ("hygienic" "7.html#%_idx_176")
    ("#i" "9.html#%_idx_238")
    ("identifier" "5.html#%_idx_14")
    ("if" "7.html#%_idx_98")
    ("imag-part" "9.html#%_idx_340")
    ("immutable" "6.html#%_idx_76")
    ("implementation restriction" "4.html#%_idx_10")
    ("improper list" "9.html#%_idx_382")
    ("inexact" "9.html#%_idx_214")
    ("inexact->exact" "9.html#%_idx_348")
    ("inexact?" "9.html#%_idx_252")
    ("initial environment" "9.html#%_idx_200")
    ("input-port?" "9.html#%_idx_592")
    ("integer->char" "9.html#%_idx_482")
    ("integer?" "9.html#%_idx_248")
    ("interaction-environment" "9.html#%_idx_584")
    ("internal definition" "8.html#%_idx_194")
    ("keyword" "7.html#%_idx_166")
    ("lambda" "7.html#%_idx_96")
    ("lazy evaluation" "7.html#%_idx_144")
    ("lcm" "9.html#%_idx_296")
    ("length" "9.html#%_idx_418")
    ("let" "7.html#%_idx_124")
    ("let*" "7.html#%_idx_128")
    ("let-syntax" "7.html#%_idx_180")
    ("letrec" "7.html#%_idx_132")
    ("letrec-syntax" "7.html#%_idx_182")
    ("library" "4.html#%_idx_6")
    ("library procedure" "9.html#%_idx_204")
    ("list" "9.html#%_idx_416")
    ("list->string" "9.html#%_idx_528")
    ("list->vector" "9.html#%_idx_550")
    ("list-ref" "9.html#%_idx_426")
    ("list-tail" "9.html#%_idx_424")
    ("list?" "9.html#%_idx_414")
    ("load" "9.html#%_idx_630")
    ("location" "6.html#%_idx_70")
    ("log" "9.html#%_idx_316")
    ("macro" "7.html#%_idx_162")
    ("macro keyword" "7.html#%_idx_168")
    ("macro transformer" "7.html#%_idx_172")
    ("macro use" "7.html#%_idx_170")
    ("magnitude" "9.html#%_idx_342")
    ("make-polar" "9.html#%_idx_336")
    ("make-rectangular" "9.html#%_idx_334")
    ("make-string" "9.html#%_idx_492")
    ("make-vector" "9.html#%_idx_538")
    ("map" "9.html#%_idx_558")
    ("max" "9.html#%_idx_274")
    ("member" "9.html#%_idx_432")
    ("memq" "9.html#%_idx_428")
    ("memv" "9.html#%_idx_430")
    ("min" "9.html#%_idx_276")
    ("modulo" "9.html#%_idx_292")
    ("mutable" "6.html#%_idx_74")
    ("negative?" "9.html#%_idx_268")
    ("newline" "9.html#%_idx_626")
    ("not" "9.html#%_idx_368")
    ("null-environment" "9.html#%_idx_582")
    ("null?" "9.html#%_idx_410")
    ("number" "9.html#%_idx_220")
    ("number->string" "9.html#%_idx_350")
    ("number?" "6.html#%_idx_52")
    ("numerator" "9.html#%_idx_298")
    ("numerical types" "9.html#%_idx_222")
    ("#o" "9.html#%_idx_230")
    ("object" "4.html#%_idx_2")
    ("odd?" "9.html#%_idx_270")
    ("open-input-file" "9.html#%_idx_604")
    ("open-output-file" "9.html#%_idx_606")
    ("optional" "4.html#%_idx_4")
    ("or" "7.html#%_idx_120")
    ("output-port?" "9.html#%_idx_594")
    ("pair" "9.html#%_idx_372")
    ("pair?" "6.html#%_idx_48")
    ("peek-char" "9.html#%_idx_616")
    ("port" "9.html#%_idx_586")
    ("port?" "6.html#%_idx_60")
    ("positive?" "9.html#%_idx_266")
    ("predicate" "9.html#%_idx_206")
    ("procedure call" "7.html#%_idx_92")
    ("procedure?" "6.html#%_idx_62")
    ("promise" "7.html#%_idx_148")
    ("proper tail recursion" "6.html#%_idx_78")
    ("quasiquote" "7.html#%_idx_150")
    ("quote" "7.html#%_idx_86")
    ("quotient" "9.html#%_idx_288")
    ("rational?" "9.html#%_idx_246")
    ("rationalize" "9.html#%_idx_310")
    ("read" "9.html#%_idx_612")
    ("read-char" "9.html#%_idx_614")
    ("real-part" "9.html#%_idx_338")
    ("real?" "9.html#%_idx_244")
    ("referentially transparent" "7.html#%_idx_178")
    ("region" "6.html#%_idx_36")
    ("remainder" "9.html#%_idx_290")
    ("reverse" "9.html#%_idx_422")
    ("round" "9.html#%_idx_308")
    ("scheme-report-environment" "9.html#%_idx_580")
    ("set!" "7.html#%_idx_102")
    ("set-car!" "9.html#%_idx_398")
    ("set-cdr!" "9.html#%_idx_400")
    ("setcar" "10.html#%_idx_644")
    ("simplest rational" "9.html#%_idx_312")
    ("sin" "9.html#%_idx_318")
    ("sqrt" "9.html#%_idx_330")
    ("string" "9.html#%_idx_494")
    ("string->list" "9.html#%_idx_526")
    ("string->number" "9.html#%_idx_352")
    ("string->symbol" "9.html#%_idx_446")
    ("string-append" "9.html#%_idx_524")
    ("string-ci<=?" "9.html#%_idx_518")
    ("string-ci<?" "9.html#%_idx_514")
    ("string-ci=?" "9.html#%_idx_504")
    ("string-ci>=?" "9.html#%_idx_520")
    ("string-ci>?" "9.html#%_idx_516")
    ("string-copy" "9.html#%_idx_530")
    ("string-fill!" "9.html#%_idx_532")
    ("string-length" "9.html#%_idx_496")
    ("string-ref" "9.html#%_idx_498")
    ("string-set!" "9.html#%_idx_500")
    ("string<=?" "9.html#%_idx_510")
    ("string<?" "9.html#%_idx_506")
    ("string=?" "9.html#%_idx_502")
    ("string>=?" "9.html#%_idx_512")
    ("string>?" "9.html#%_idx_508")
    ("string?" "6.html#%_idx_56")
    ("substring" "9.html#%_idx_522")
    ("symbol->string" "9.html#%_idx_444")
    ("symbol?" "6.html#%_idx_50")
    ("syntactic keyword" "5.html#%_idx_18")
    ("syntax definition" "8.html#%_idx_196")
    ("syntax-rules" "7.html#%_idx_184")
    ("#t" "9.html#%_idx_354")
    ("tail call" "6.html#%_idx_80")
    ("tan" "9.html#%_idx_322")
    ("token" "10.html#%_idx_636")
    ("top level environment" "6.html#%_idx_42")
    ("transcript-off" "9.html#%_idx_634")
    ("transcript-on" "9.html#%_idx_632")
    ("true" "6.html#%_idx_66")
    ("truncate" "9.html#%_idx_306")
    ("type" "6.html#%_idx_44")
    ("unbound" "6.html#%_idx_38")
    ("unspecified" "4.html#%_idx_12")
    ("valid indexes" "9.html#%_idx_488")
    ("values" "9.html#%_idx_572")
    ("variable" "5.html#%_idx_16")
    ("vector" "9.html#%_idx_540")
    ("vector->list" "9.html#%_idx_548")
    ("vector-fill!" "9.html#%_idx_552")
    ("vector-length" "9.html#%_idx_542")
    ("vector-ref" "9.html#%_idx_544")
    ("vector-set!" "9.html#%_idx_546")
    ("vector?" "6.html#%_idx_58")
    ("Whitespace" "5.html#%_idx_20")
    ("with-input-from-file" "9.html#%_idx_600")
    ("with-output-to-file" "9.html#%_idx_602")
    ("write" "9.html#%_idx_622")
    ("write-char" "9.html#%_idx_628")
    ("#x" "9.html#%_idx_234")
    ("zero?" "9.html#%_idx_264")))

;;----------------------------------------------
;; Reader macro definitions
;;

(define-reader-macro (index prefix)
  `((ul
     ,@(map (lambda (key) `(li ,(wiliki:wikiname-anchor (car key))))
            (wiliki-db-search
             (lambda (k v) (string-prefix? prefix k))
             (lambda (a b)
               (string<? (car a) (car b))))))))

(define-reader-macro (cindex prefix . maybe-delim)
  (intersperse (get-optional maybe-delim " ")
               (map (lambda (key) (wiliki:wikiname-anchor (car key)))
                    (wiliki-db-search
                     (lambda (k v) (string-prefix? prefix k))
                     (lambda (a b)
                       (string<? (car a) (car b)))))))

(define-reader-macro (include page)
  (cond ((wiliki-db-get page) => wiliki:format-content)
        (else (list #`"[[$$include ,page]]"))))

(define-reader-macro (img url . maybe-alt)
  (define (alt) (if (null? maybe-alt) "[image]" (string-join maybe-alt " ")))
  (define (badimg) `((a (@ (href ,url)) ,(alt))))
  (let loop ((urls (image-urls-of (wiliki))))
    (if (pair? urls)
      (receive (pred action)
          (if (pair? (car urls))
            (values (caar urls) (cadar urls))
            (values (car urls) 'allow))
        (if (pred url)
          (if (eq? action 'allow)
            `((img (@ (src ,url) (alt ,(alt)))))
            (badimg))
          (loop (cdr urls))))
      (badimg))))

(define-reader-macro (toc . maybe-page)
  (let1 page (or (and-let* ((name (get-optional maybe-page #f)))
                   (wiliki-db-get name #f))
                 (wiliki:current-page))
    (if (not page)
      (if (pair? maybe-page)
        (list #`"[[$$toc ,(car maybe-page)]]")
        (list "[[$$toc]]"))
      (let1 pagename (and page (ref page 'key))

        ;; NB: hs is a _reverse_ ordered list of all headings (level . text).
        ;; Since it's reversed, we can scan forward to find the heading
        ;; nesting.
        (define (make-ul hs cur items cont)
          (cond ((null? hs)
                 (cont '() `(ul ,@items)))
                ((= (caar hs) cur)
                 (make-ul (cdr hs) cur
                          (cons (make-anchor (nestings hs)) items)
                          cont))
                ((< (caar hs) cur)
                 (cont hs `(ul ,@items)))
                (else
                 (make-ul hs (caar hs)'()
                          (lambda (hs ul)
                            (make-ul hs cur (cons ul items) cont))))))

        (define (nestings hs)
          (reverse!
           (cdr
            (fold (lambda (elt seed)
                    (let ((level (car elt))
                          (cur-level (car seed)))
                      (if (< level cur-level)
                        (list* level (cdr elt) (cdr seed))
                        seed)))
                  '(6)
                  hs))))

        (define (make-anchor headings)
          (let ((id (wiliki:calculate-heading-id headings)))
            `(li (a (@ (href ,#`",(wiliki:self-url \"~a\" pagename)#,id"))
                    ,@(wiliki:format-line-plainly (car headings))))))

        (let1 headings
            (wiliki:page-lines-fold
             page
             (lambda (l r)
               (cond ((#/^(\*{1,}) / l)
                      => (lambda (m)
                           (acons (string-length (m 1)) (m 'after) r)))
                     (else r)))
             '()
             :follow-includes? #t
             :skip-verbatim? #t)
          (make-ul headings 1 '() (lambda (_ ul) (list ul))))
        ))))

(define-reader-macro (testerr . x)
  (error (x->string x)))

;;----------------------------------------------
;; Virtual page definitions
;;

;; These are just samples.

(define-virtual-page (#/^RecentChanges$/ (_))
  `((table (@ (class "recent-changes"))
     ,@(map (lambda (p)
              `(tr (td ,(wiliki:format-time (wiliki-db-rc-mtime p)))
                   (td "(" ,(how-long-since (wiliki-db-rc-mtime p)) " ago)")
                   (td ,(wiliki:wikiname-anchor (wiliki-db-rc-key p)))))
            (wiliki-db-recent-changes)))))

(provide "wiliki/macro")
