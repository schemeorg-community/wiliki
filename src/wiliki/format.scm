;;;
;;; wiliki/format.scm - wiliki markup -> SXML converter
;;;
;;;  Copyright (c) 2003-2004 Shiro Kawai, All rights reserved.
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
;;; $Id: format.scm,v 1.34 2004/04/02 12:10:56 shirok Exp $

(define-module wiliki.format
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use srfi-13)
  (use text.html-lite)
  (use text.tree)
  (use text.tr)
  (use rfc.uri)
  (use util.list)
  (use util.queue)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.sequence)
  (use wiliki.format-scheme)
  (export <wiliki-formatter>
          <wiliki-page>
          wiliki:persistent-page?
          wiliki:transient-page?
          wiliki:format-wikiname
          wiliki:format-macro
          wiliki:format-time
          wiliki:format-content
          wiliki:formatter
          wiliki:page-stack
          wiliki:page-circular?
          wiliki:current-page
          wiliki:format-page-header
          wiliki:format-page-content
          wiliki:format-page-footer
          wiliki:format-page-body
          wiliki:format-head-elements
          wiliki:format-page
          wiliki:format-line-plainly
          wiliki:calculate-heading-id
          wiliki:sxml->stree
          wiliki:format-diff-pre
          wiliki:format-diff-line
          )
  )
(select-module wiliki.format)

;; This module implements a generic function that translates WiLiki
;; notation to SXML.   It is designed not to depend other parts of
;; WiLiKi so that it can be used for other applications that needs
;; wiki-like formatting capability.

;; A formatter base class.
;; The user can define her own formatter by subclassing this and
;; overloading some methods.

(define-class <wiliki-formatter> ()
  (;; The following slots are only for compatibility to the code
   ;; written with WiLiKi-0.5_pre2.
   ;; They won't be supported officially in future versions; use
   ;; subclassing & methods instead.
   (bracket       :init-keyword :bracket
                  :init-value (lambda (name) (list #`"[[,|name|]]")))
   (macro         :init-keyword :macro
                  :init-value (lambda (expr context)
                                `("##" ,(write-to-string expr))))
   (time          :init-keyword :time
                  :init-value (lambda (time) (x->string time)))
   (body          :init-keyword :body
                  :init-value (lambda (page opts) (fmt-body page opts)))
   (header        :init-keyword :header
                  :init-value (lambda (page opts) '()))
   (footer        :init-keyword :footer
                  :init-value (lambda (page opts) '()))
   (content       :init-keyword :content
                  :init-value (lambda (page opts) (fmt-content page)))
   (head-elements :init-keyword :head-elements
                  :init-value (lambda (page opts) '()))
   ))

;; Global context and the default formatter
(define the-formatter
  (make-parameter (make <wiliki-formatter>)))

(define fmt-context
  (make-parameter '()))

;; These are for convenience of internal use.
(define (fmt-wikiname name)
  (wiliki:format-wikiname (the-formatter) name))

(define (fmt-macro expr context)
  (wiliki:format-macro (the-formatter) expr context))

(define (fmt-time time)
  (wiliki:format-time (the-formatter) time))

;; Utilities

;; Expands tabs in a line.
(define expand-tab 
  (let ((pads #("        "
                " "
                "  "
                "   "
                "    "
                "     "
                "      "
                "       ")))
    (lambda (line)
      (let loop ((line   line)
                 (r      '())
                 (column 0))
        (receive (before after) (string-scan line #\tab 'both)
          (if before
              (let* ((newcol  (+ (string-length before) column))
                     (fill-to (inexact->exact (* (ceiling (/ newcol 8)) 8))))
                (loop after
                      (list* (vector-ref pads (- fill-to newcol)) before r)
                      fill-to))
              (reverse (cons line r))))))
    ))

;; similar to sxml:sxml->xml, but deals with stree node, which
;; embeds a string tree.

(define (wiliki:sxml->stree sxml)
  (define (sxml-node type body)
    (define (attr lis r)
      (cond ((null? lis) (reverse! r))
            ((not (= (length+ (car lis)) 2))
             (error "bad attribute in node: " (cons type body)))
            (else
             (attr (cdr lis)
                   (cons `(" " ,(html-escape-string (x->string (caar lis)))
                           "=\"" ,(html-escape-string (x->string (cadar lis)))
                           "\"")
                         r)))))
    (define (rest type lis)
      (if (and (null? lis)
               (memq type '(br area link img param hr input col base meta)))
        '(" />")
        (list* ">" (reverse! (fold node '() lis)) "</" type "\n>")))

    (if (and (pair? body)
             (pair? (car body))
             (eq? (caar body) '@))
      (list* "<" type (attr (cdar body) '()) (rest type (cdr body)))
      (list* "<" type (rest type body)))
    )

  (define (node n r)
    (cond
     ((string? n) (cons (html-escape-string n) r))
     ((and (pair? n) (symbol? (car n)))
      (if (eq? (car n) 'stree)
        (cons (cdr n) r)
        (cons (sxml-node (car n) (cdr n)) r)))
     (else
      ;; badly formed node.  we show it for debugging ease.
      (cons (list "<span class=\"wiliki-alert\">" 
                  (html-escape-string (format "~,,,,50:s" n))
                  "</span\n>")
            r))))

  (node sxml '()))

;;=================================================
;; Formatting: Wiki -> SXML
;;

(define (regexp-fold rx proc-nomatch proc-match seed line)
  (let loop ((line line)
             (seed seed))
    (cond ((string-null? line) seed)
          ((rx line)
           => (lambda (m)
                (let ((pre   (m 'before))
                      (post  (m 'after)))
                  (if (string-null? pre)
                    (loop post (proc-match m seed))
                    (loop post (proc-match m (proc-nomatch pre seed)))))))
          (else
           (proc-nomatch line seed)))
    ))

;; After "##(" is read, retrieve one expr from string.
;; Returns [Sexp, Str]
(define (read-macro-expr str)
  (with-error-handler
      (lambda (e) (values #f #f))
    (lambda ()
      (let* ((s (open-input-string str))
             (x (read-list #\) s)))
        (values x (get-remaining-input-string s))))))

;; A regex for uris. Even readable.
(define *uri-regex*
  (let ((schemes (string-append "(http|https|ftp|afs|news|nntp|mid|cid"
                                "|mailto|wais|prospero|telnet|gopher|irc)")))
    (string->regexp
     (string-append
      "(\\[(" schemes "\\S+) ([^\\]]+)\\])" ; [url text]
      "|"
      "(\\[(" schemes "\\S+)\\])" ; [url]
      "|"
      "("
      schemes
      ":(//)?"
      "[-a-zA-Z0-9/@=+$_~*.,;:?!\\'\"()&#%]+" ; Full url chars according to RFC
      "[-a-zA-Z0-9/@=+$_~*]" ; No punctuation at the end of the URL
      ")"
      ))))

;; Find wiki name in the line.
;; Correctly deal with nested "[[" and "]]"'s.
(define (fmt-line ctx line seed)
  ;; parse to next "[[" or "]]"
  (define (token s)
    (cond ((#/\[\[|\]\]/ s)
           => (lambda (m) (values (m 'before) (m) (m 'after))))
          (else (values s #f #f))))
  ;; return <str in paren> and <the rest of string to scan>
  (define (find-closer s level in)
    (receive (pre tok post) (token s)
      (cond ((not tok)
             (values #f (tree->string (cons "[[" (reverse (cons pre in))))))
            ((string=? tok "[[")
             (find-closer post (+ level 1) (list* "[[" pre in)))
            ((= level 0)
             (values (tree->string (reverse (cons pre in))) post))
            (else
             (find-closer post (- level 1) (list* "]]" pre in))))))
  (define (uri line seed)
    (regexp-fold
     *uri-regex*
     cons
     (lambda (match seed)
       (cond
        ((match 1) (cons `(a (@ (href ,(match 2))) ,(match 4))
                         seed))
        ((match 5) (cons `(a (@ (href ,(match 6))) ,(match 6))
                         seed))
        ((match 8) (cons `(a (@ (href ,(match 8))) ,(match 8))
                         seed))))
     seed line))
  (define (nl line seed)
    (regexp-fold
     #/~%/
     uri
     (lambda (match seed) (cons '(br) seed))
     seed line))
  ;; NB: we remove empty bold and italic, for backward compatibility
  (define (italic line seed)
    (regexp-fold
     #/''([^'].*?)?''/
     nl
     (lambda (match seed)
       (if (or (not (match 1)) (string-null? (match 1)))
         seed
         (cons `(em ,@(reverse! (nl (match 1) '()))) seed)))
     seed line))
  (define (bold line seed)
    (regexp-fold
     #/'''([^'].*?)?'''/
     italic
     (lambda (match seed)
       (if (or (not (match 1)) (string-null? (match 1)))
         seed
         (cons `(strong ,@(reverse! (nl (match 1) '()))) seed)))
     seed line))
  (define (teletype line seed)
    (regexp-fold
     #/{{{([^\}]+)}}}/
     bold
     (lambda (match seed)
       (if (or (not (match 1)) (string-null? (match 1)))
         seed
         (cons `(tt ,@(reverse! (nl (match 1) '()))) seed)))
     seed line))
  (define (bracket line seed)
    (if (string-null? line)
      seed
      (receive (pre post) (string-scan line "[[" 'both)
        (if pre
          (receive (wikiname rest) (find-closer post 0 '())
            (if wikiname
              (bracket rest
                       (append (reverse! (parameterize ((fmt-context ctx))
                                           (fmt-wikiname wikiname)))
                               (teletype pre seed)))
              (teletype rest (bold pre seed))))
          (teletype line seed)))))
  (define (smacro line seed)
    (if (string-null? line)
      seed
      (receive (pre post) (string-scan line "##(" 'both)
        (if pre
          (receive (expr rest) (read-macro-expr post)
            ;; NB: we should handle distinction of inline and block elements
            ;; here.  It requires some trick, so for now I leave it.
            (if expr
              (smacro rest
                      (append (reverse! (fmt-macro expr 'inline))
                              (bracket pre seed)))
              (smacro post (bracket (string-append pre "##(") seed))))
          (bracket line seed)))))
  ;; Main body
  (cons "\n" (smacro line seed)))

;; Utility to generate a (mostly) unique id for the headings.
;; Passes a list of heading string stack.
(define (wiliki:calculate-heading-id headings)
  (string-append "H-" (number->string (hash headings) 36)))

;; Read lines from generator and format them.  This is the main
;; parser/transformer of WiLiKi format.
(define (fmt-lines generator)

  (define (h-level m)
    (- (rxmatch-end m 1) (rxmatch-start m 1)))
  (define (l-level ctx)
    (count (cut memq <> '(ul ol)) ctx))

  (define (lex line ctx)
    (cond ((eof-object? line)                '(eof))
          ((string-null? line)               '(null))
          ((string=? "----" line)            '(hr))
          ((string=? "{{{" line)             '(open-verb))
          ((string=? "{{{scheme" line)       '(open-scheme))
          ((string=? "<<<" line)             '(open-quote))
          ((string-prefix? "<<<" line)       `(open-comment-quote . ,line))
          ((and (string=? ">>>" line)
                (memq 'blockquote ctx))      '(close-quote))
          ((string-prefix? " " line)         `(pre . ,line))
          ((rxmatch #/^(\*{1,}) / line)      => (cut cons 'heading <>))
          ((rxmatch #/^(--*) / line)         => (cut cons 'ul <>))
          ((rxmatch #/^(##*) / line)         => (cut cons 'ol <>))
          ((rxmatch #/^:(.*):([^:]*)$/ line) => (cut cons 'dl <>))
          ((rxmatch #/^\|\|(.*)\|\|$/ line)  => (cut cons 'table <>))
          (else                              `(p . ,line))))

  (define token-buffer #f)
  (define (next-token ctx) (or token-buffer (lex (generator) ctx)))
  (define (pushback-token tok) (set! token-buffer tok))
  (define (token-type tok) (car tok))
  (define (token-value tok) (cdr tok))

  (define (>> cont ctx seed)
    (lambda (tok ctx r) (cont tok ctx (cons r seed))))

  ;; Block-level loop
  (define (block tok ctx seed)
    (let loop ((tok tok) (seed seed) (p '()))
      (if (eq? (token-type tok) 'p)
        (loop (next-token ctx) seed
              (fmt-line ctx (token-value tok) p))
        (let1 seed (if (null? p) seed (cons `(p ,@(reverse! p)) seed))
          (case (token-type tok)
            ((eof)  (reverse! seed))
            ((null) (block (next-token ctx) ctx seed))
            ((hr)   (block (next-token ctx) ctx (cons '(hr) seed)))
            ((open-verb)
             (verb ctx (>> block ctx seed)))
            ((open-scheme)
             (scheme ctx (>> block ctx seed)))
            ((open-quote)
             (blockquote ctx (>> block ctx seed)))
            ((close-quote)
             (reverse! seed))
            ((open-comment-quote)
             (open-comment-quote (token-value tok) ctx (>> block ctx seed)))
            ((pre)
             (pre tok ctx (>> block ctx seed)))
            ((heading)
             (heading (token-value tok) ctx (>> block ctx seed)))
            ((ul ol)
             (list-item tok ctx (>> block ctx seed)))
            ((dl)
             (def-item tok ctx (>> block ctx seed)))
            ((table)
             (table tok ctx (>> block ctx seed)))
            (else
             (error "internal error: unknown token type?")))))))

  (define (open-comment-quote line ctx cont)
    (let* ((match (rxmatch #/^<<<(\[\[)?([^\]]*)(\]\])?$/ line))
           (name (if match
                     (rxmatch-substring match 2)
                     (substring line 3 (string-length line))))
           (new-ctx (list 'blockquote))
           (image (with-module wiliki
                    (wiliki:user-image name)))
           (r `(div (@ (class "comment"))
                    (div (@ (class "portrait"))
                         ,@(if image
                               `((img (@ (src ,image)
                                         (alt ""))))
                               '())
                         (p (a (@ (href ,(with-module wiliki
                                           (wiliki:user-page name))))
                               ,name)))
                    (div (@ (class "comment-text"))
                         ,@(block (next-token new-ctx) new-ctx '())))))
      (cont (next-token ctx) ctx r)))

  ;; Verbatim
  (define (verb ctx cont)
    (let loop ((line (generator)) (r '()))
      (if (or (eof-object? line)
              (equal? "}}}" line))
        (cont (next-token ctx) ctx `(pre ,@(reverse! r)))
        (loop (generator)
              (list* "\n" (tree->string (expand-tab line)) r)))))

  (define (scheme ctx cont)
    (let loop ((line (generator)) (r '()))
      (if (or (eof-object? line)
              (equal? "}}}" line))
        (cont (next-token ctx)
              ctx
              (format-scheme-code (reverse! r)
                                  (with-module wiliki
                                     (wiliki:scheme-keywords))))
        (loop (generator)
              (list* "\n" (tree->string (expand-tab line)) r)))))

  ;; Preformatted
  (define (pre tok ctx cont)
    (let loop ((tok tok) (r '()))
      (if (eq? (token-type tok) 'pre)
        (loop (next-token ctx)
              (fmt-line ctx (tree->string (expand-tab (token-value tok))) r))
        (cont tok ctx `(pre ,@(reverse! r))))))

  ;; Heading
  (define (heading m ctx cont)
    ;; extract headings from context
    (define (headings-context ctx)
      (reverse!
       (cdr
        (fold (lambda (elt seed)
                (if (not (and (pair? elt)
                              (memq (car elt) '(h2 h3 h4 h5 h6))))
                  seed
                  (let ((level (find-index (cute eq? (car elt) <>)
                                           '(h2 h3 h4 h5 h6)))
                        (cur-level (car seed)))
                    (if (< level cur-level)
                      (list* level (cdr elt) (cdr seed))
                      seed))))
              '(6)
              ctx))))
    ;; body of heading
    (let* ((h-lev (min (h-level m) 5))
           (elm   (ref '(_ h2 h3 h4 h5 h6) h-lev))
           (hstr  (m 'after))
           (new-ctx (acons elm hstr ctx))
           (id    (wiliki:calculate-heading-id (headings-context new-ctx))))
      (cont (next-token new-ctx)
            new-ctx
            `(,elm (@ (id ,id)) ,@(reverse! (fmt-line ctx hstr '()))))))

  ;; Table
  (define (table tok ctx cont)
    (let loop ((tok tok)
               (r '()))
      (if (eq? (token-type tok) 'table)
        (loop (next-token ctx) (cons (table-row ctx (token-value tok)) r))
        (cont tok ctx
              `(table (@ (class "inbody") (border 1) (cellspacing 0))
                      ,@(reverse! r))))))

  (define (table-row ctx m)
    `(tr (@ (class "inbody"))
         ,@(map (lambda (seq)
                  `(td (@ (class "inbody"))
                       ,@(reverse! (fmt-line ctx seq '()))))
                (string-split (m 1) "||"))))

  ;; Blockquote
  (define (blockquote ctx cont)
    (let* ((new-ctx (list 'blockquote))
           (r `(blockquote ,@(block (next-token new-ctx) new-ctx '()))))
      (cont (next-token ctx) ctx r)))

  ;; UL and OL
  (define (list-item tok ctx cont)
    (let* ((ltype  (token-type tok))
           (newctx (cons ltype ctx))
           (bottom (l-level newctx)))
      
      (define (wrap tok items ctx)
        (if (not (memq (token-type tok) '(ul ol)))
          (values tok `((,(car ctx) ,@(reverse! items))))
          (let ((new-level (h-level (token-value tok)))
                (cur-level (l-level ctx)))
            (cond ((< new-level bottom)
                   (values tok `((,(car ctx) ,@(reverse! items)))))
                  ((and (eq? (token-type tok) (car ctx))
                        (= new-level cur-level))
                   (fold-content tok ctx items))
                  ((> new-level cur-level)
                   (receive (nextok r)
                       (wrap tok '() (cons (token-type tok) ctx))
                     (wrap nextok
                           (cond
                            ((null? items) r)
                            ((eq? (caar items) 'li)
                             `((,(caar items) ,@(append (cdar items) r))
                               ,@(cdr items)))
                            (else (append r items)))
                           ctx)))
                  (else
                   (values tok
                           (if (null? items)
                             '()
                             `((,(car ctx) ,@(reverse! items)))))))
            )))

      (define (fold-content tok ctx items)
        (let loop ((tok (next-token ctx))
                   (ctx ctx)
                   (r (fmt-line ctx ((token-value tok) 'after) '())))
          (case (token-type tok)
            ((eof null hr heading ul ol close-quote)
             (wrap tok (cons `(li ,@(reverse! r)) items) ctx))
            ((open-quote) (blockquote ctx (>> loop ctx r)))
            ((open-verb) (verb ctx (>> loop ctx r)))
            ((table) (table tok ctx (>> loop ctx r)))
            ((dl) (def-item tok ctx (>> loop ctx r)))
            (else (loop (next-token ctx) ctx
                        (fmt-line ctx (token-value tok) r))))))

      ;; body of list-item
      (receive (tok elts) (wrap tok '() newctx)
        (cont tok ctx (car elts)))))

  ;; DL
  (define (def-item tok ctx cont)
    (receive (nextok r) (def-item-rec tok ctx '())
      (cont nextok ctx `(dl ,@(reverse! r)))))

  (define (def-item-rec tok ctx seed)
    (let ((dt (reverse! (fmt-line ctx ((token-value tok) 1) '())))
          (dd (fmt-line ctx ((token-value tok) 2) '())))
      (let loop ((tok (next-token ctx))
                 (p dd)
                 (r '()))
        (define (fold-p)
          (if (null? p) r (cons `(p ,@(reverse! p)) r)))
        (define (finish)
          `((dd ,@(reverse! (fold-p))) (dt ,@dt) ,@seed))
        (case (token-type tok)
          ((eof null hr heading)
           (values tok (finish)))
          ((dl)
           (def-item-rec tok ctx (finish)))
          ((p)
           (loop (next-token ctx)
                 (fmt-line ctx (token-value tok) p) r))
          ((pre)
           (pre tok ctx (lambda (tok ctx elt)
                          (loop tok '() (cons elt (fold-p))))))
          ((open-quote)
           (blockquote ctx (lambda (tok ctx elt)
                             (loop tok '() (cons elt (fold-p))))))
          ((open-verb)
           (verb ctx (lambda (tok ctx elt)
                       (loop tok '() (cons elt (fold-p))))))
          ((table)
           (table tok ctx (lambda (tok ctx elt)
                            (loop tok '() (cons elt (fold-p))))))
          ((ul ol)
           (if (> (h-level (token-value tok))
                  (l-level ctx))
             (list-item tok ctx (lambda (tok ctx elt)
                                  (loop tok '() (cons elt (fold-p)))))
             (values tok (finish))))
          (else
           (loop (next-token ctx) '()
                 (fmt-line ctx (token-value tok) p) r))
          ))))

  ;; Main body
  (let ((ctx (fmt-context)))
    (block (next-token ctx) ctx '()))
  )

;; Create a line scanner method
(define (make-line-scanner port)
  (define buf #f)       ;; buffer for a lookahead line
  (define verbatim #f)  ;; flag

  ;; Get a physical line
  (define (getline)
    (if buf (begin0 buf (set! buf #f)) (read-line port)))
  (define (ungetline line) (set! buf line))

  ;; Lexer body
  (lambda ()
    (let rec ((line (getline))
              (r    '()))
      (cond ((eof-object? line)
             (if (null? r) line (string-concatenate-reverse r)))
            (verbatim
             (when (string=? "}}}" line) (set! verbatim #f))
             line)
            ((string-prefix? ";;" line)
             (rec (getline) r))
            ((or (string=? "{{{" line)
                 (string=? "{{{scheme" line))
             (if (null? r)
               (begin (set! verbatim #t) line)
               (begin (ungetline line) (string-concatenate-reverse r))))
            ((string-prefix? "~" line)
             (rec (getline) (cons (string-drop line 1) r)))
            (else
             (if (null? r)
               (rec (getline) (cons line r))
               (begin (ungetline line) (string-concatenate-reverse r))))
            )))
  )

;; utility : strips wiki markup and returns a plaintext line.
(define wiliki:format-line-plainly
  (let ((plain-formatter (make <wiliki-formatter> :bracket list)))
    (lambda (line)
      (parameterize ((the-formatter plain-formatter))
        (reverse! ((rec (tree-fold tree seed)
                     (cond ((string? tree)
                            (if (equal? tree "\n") seed (cons tree seed)))
                           ((and (pair? tree) (not (eq? (car tree) '@)))
                            (fold tree-fold seed (cdr tree)))
                           (else seed)))
                   `(x ,@(reverse! (fmt-line '() line '()))) '()))))
    ))
  
;; Page ======================================================

(define page-stack
  (make-parameter '()))

(define (current-page)
  (let1 hist (page-stack)
    (if (null? hist) #f (car hist))))

;; Class <wiliki-page> ---------------------------------------------
;;   Represents a page.
;;
;;   persistent page: a page that is (or will be) stored in DB.
;;         - has 'key' value.
;;         - if mtime is #f, it is a freshly created page before saved.
;;   transient page: other pages created procedurally just for display.
;;         - 'key' slot has #f.

(define-class <wiliki-page> ()
  (;; title - Page title.  For persistent pages, this is set to
   ;;         the same value as the database key.
   (title   :init-value #f :init-keyword :title)
   ;; key   - Database key.  For transient pages, this is #f.
   (key     :init-value #f :init-keyword :key)
   ;; command - A URL parameters to reproduce this page.  Only meaningful
   ;;           for transient pages.
   (command :init-value #f :init-keyword :command)
   ;; content - Either a wiliki-marked-up string or SXML.
   (content :init-value "" :init-keyword :content)
   ;; creation and modification times, and users (users not used now).
   (ctime   :init-value (sys-time) :init-keyword :ctime)
   (cuser   :init-value #f :init-keyword :cuser)
   (mtime   :init-value #f :init-keyword :mtime)
   (muser   :init-value #f :init-keyword :muser)
   ))

(define (fmt-content page)
  (define (do-fmt content)
    (call-with-input-string content
      (lambda (p)
        (with-port-locking p
          (cut fmt-lines (make-line-scanner p))))))
  (cond ((string? page) (do-fmt page))
        ((is-a? page <wiliki-page>)
         (if (wiliki:page-circular? page)
           ;; loop in $$include chain detected
           `(p ">>>$$include loop detected<<<")
           (parameterize
               ((page-stack (cons page (page-stack))))
             (if (string? (ref page 'content))
               (do-fmt (ref page 'content))
               (ref page 'content)))))
        (else page)))

(define (wiliki:page-circular? page)
  (member page (page-stack)
          (lambda (p1 p2)
            (and (ref p1 'key) (ref p2 'key)
                 (string=? (ref p1 'key) (ref p2 'key))))))

;; default page body formatter
(define (fmt-body page opts)
  `(,@(wiliki:format-page-header  page opts)
    ,@(wiliki:format-page-content page opts)
    ,@(wiliki:format-page-footer  page opts)))

;;;
;;; Exported functions
;;;

(define wiliki:formatter        the-formatter)
(define wiliki:page-stack       page-stack)
(define wiliki:current-page     current-page)

(define wiliki:format-content   fmt-content)

;; Default formatting methods.
;; Methods are supposed to return SXML nodeset.
;; NB: It is _temporary_ that these methods calling the slot value
;; of the formatter, just to keep the backward compatibility to 0.5_pre2.
;; Do not count on this implementation.  The next release will remove
;; all the closure slots of <wiliki-formatter> and the default behavior
;; will directly be embedded in these methods.

(define-method wiliki:format-wikiname ((fmt <wiliki-formatter>) name)
  ((ref fmt 'bracket) name))
(define-method wiliki:format-wikiname ((name <string>))
  (wiliki:format-wikiname (the-formatter) name))

(define-method wiliki:format-macro ((fmt <wiliki-formatter>) expr context)
  ((ref fmt 'macro) expr context))
(define-method wiliki:format-macro (expr context)
  (wiliki:format-macro (the-formatter) expr context))

(define-method wiliki:format-time ((fmt <wiliki-formatter>) time)
  ((ref fmt 'time) time))
(define-method wiliki:format-time (time)
  (wiliki:format-time (the-formatter) time))

(define-method wiliki:format-page-content ((fmt  <wiliki-formatter>)
                                           page  ;; may be a string
                                           . options)
  ((ref fmt 'content) page options))
(define-method wiliki:format-page-content (page . opts)
  (apply wiliki:format-page-content (the-formatter) page opts))

(define-method wiliki:format-page-body ((fmt  <wiliki-formatter>)
                                        (page <wiliki-page>)
                                        . opts)
  `(,@(apply wiliki:format-page-header  page opts)
    ,@(apply wiliki:format-page-content page opts)
    ,@(apply wiliki:format-page-footer  page opts)))
(define-method wiliki:format-page-body ((page <wiliki-page>) . opts)
  (apply wiliki:format-page-body (the-formatter) page opts))

(define-method wiliki:format-page-header ((fmt  <wiliki-formatter>)
                                          (page <wiliki-page>)
                                          . options)
  ((ref fmt 'header) page options))
(define-method wiliki:format-page-header ((page <wiliki-page>) . opts)
  (apply wiliki:format-page-header (the-formatter) page opts))
  
(define-method wiliki:format-page-footer ((fmt  <wiliki-formatter>)
                                          (page <wiliki-page>)
                                          . options)
  ((ref fmt 'footer) page options))
(define-method wiliki:format-page-footer ((page <wiliki-page>) . opts)
  (apply wiliki:format-page-footer (the-formatter) page opts))

(define-method wiliki:format-head-elements ((fmt  <wiliki-formatter>)
                                            (page <wiliki-page>)
                                            . options)
  ((ref fmt 'head-elements) page options))
(define-method wiliki:format-head-elements ((page <wiliki-page>) . opts)
  (apply wiliki:format-head-elements (the-formatter) page opts))

(define-method wiliki:format-page ((fmt  <wiliki-formatter>)
                                   (page <wiliki-page>)
                                   . opts)
  `(html
    (head ,@(apply wiliki:format-head-elements fmt page opts))
    (body ,@(apply wiliki:format-page-body fmt page opts))))
(define-method wiliki:format-page ((page <wiliki-page>) . opts)
  (apply wiliki:format-page (the-formatter) page opts))

(define (wiliki:persistent-page? page)
  (not (wiliki:transient-page? page)))
(define (wiliki:transient-page? page)
  (not (ref page 'key)))

;; NB: these should also be a generics.
(define (wiliki:format-diff-pre difflines)
  `(pre (@ (class "diff")
           (style "background-color:#ffffff; color:#000000; margin:0"))
        ,@(map wiliki:format-diff-line difflines)))

(define (wiliki:format-diff-line line)
  (define (aline . c)
    `(span (@ (class "diff_added")
              (style "background-color:#ffffff; color: #4444ff"))
           ,@c))
  (define (dline . c)
    `(span (@ (class "diff_deleted")
              (style "background-color:#ffffff; color: #ff4444"))
           ,@c))
  (cond ((string? line) `(span "  " ,line "\n"))
        ((eq? (car line) '+) (aline "+ " (cdr line) "\n"))
        ((eq? (car line) '-) (dline "- " (cdr line) "\n"))
        (else "???")))

(provide "wiliki/format")
