;;; format-scheme.scm --- Format scheme code for the WiLiKi wiki engine

;; Copyright (C)  2004  Jorgen Schäfer

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This is used to transform a textual representation of scheme code
;; into a SXML expression for WiLiKi to output.

;; TODO:
;; - Add SCHEME-KEYWORD-PAGE to <wiliki>
;;   - (wiliki-db-get (scheme-keyword-page (wiliki)) "")
;; - Symbol highlighting:
;;   - Known symbols should be linked to an URL (SCHEME-KEYWORD-URL-PAGE)
;;     keyword ...url...
;;   - Following DEFINE could be function-name and variable-name
;; => Make these two into a single page?

(define-module wiliki.format-scheme
  (use srfi-6)
  (use srfi-8)
  (use srfi-13)
  ;; (use srfi-23) ; Included by default
  ;; (use srfi-34) ; Included by default
  (export format-scheme-code))
(select-module wiliki.format-scheme)

;; The keywords we recognize to be highlighted
(define *default-scheme-keywords*
  (map (lambda (x)
         (list x #t #f))
       '(and begin begin0 call-with-current-continuation
             call-with-input-file call-with-output-file call/cc case
             case-lambda class cond define delay do else exit-handler field
             for-each if import inherit init-field interface lambda let
             let* let*-values let-values let-syntax let/ec letrec
             letrec-syntax map mixin opt-lambda or override protect
             provide public rename require require-for-syntax syntax
             syntax-case syntax-error syntax-rules unit/sig unless when
             with-syntax)))

;; These characters delimit a token in Scheme.
(define *scheme-token-delimiters*
  (list #\( #\) #\[ #\] #\' #\` #\" #\; #\, #\|))

;; The main entrance point. LINES is passed by WiLiKi as the lines in
;; the {{{scheme ... }}} code.
(define (format-scheme-code lines keywords)
  `(pre (@ (class "scheme"))
        " " ;; SPAN tags are added with a space, so we adjust for this
	    ;; in the first line.
        ,@(handle-parse-errors
           lines
           (lambda ()
             (read-scheme-sxml
              (open-input-string
               (string-join lines))
              (or keywords
                  *default-scheme-keywords*))))))

;; Some fancy error handling, especially for parse errors. We prepend
;; an error message to the LINES if an error happens.
(define (handle-parse-errors lines thunk)
  (call-with-current-continuation
   (lambda (return)
     (with-exception-handler
         (lambda (error)
           (return (if (and (pair? error)
                            (eq? 'parse-error (car error)))
                       `((b ";; Parse error: " ,(cdr error))
                         (br)
                         ,@lines)
                       `((b ";; Error in parsing: " ,error)
                         (br)
                         ,@lines))))
      thunk))))

;; Main parsing function. Return an SXML expression for the Scheme
;; code read from PORT.
(define (read-scheme-sxml port keywords)
  (parse-scheme (lex-scheme port)
                keywords))

;;;;;;;;;;;;;
;;; The lexer

;; The lexer is trivial, and only suitable for our tasks. It tokenizes
;; the input stream into the following tokens:
;;
;; - #(open-paren "(") or "[" or "#("
;; - #(close-paren ")") or "]"
;; - #(symbol "foo")
;; - #(string "\"bar\"")
;; - #(comment ";; foo")
;; - #(whitespace "   ")
;; - #(hash-expr "#t") or "#f" or "#\a"
;; - #(reader-macro ",") or ",@" or "`" or "'"

;; The main lexer entrance point.
(define (lex-scheme port)
  (let ((t (read-lex-token port)))
    (if (eof-object? t)
        '()
        (cons t
              (lex-scheme port)))))

;; Read a single token as described above from PORT
(define (read-lex-token port)
  (define (start port)
    (let ((c (read-char port)))
      (if (eof-object? c)
          c
          (case c
            ((#\( #\[) (vector 'open-paren (make-string 1 c)))
            ((#\) #\]) (vector 'close-paren (make-string 1 c)))
            ((#\#) (hash port))
            ((#\") (string port))
            ((#\;) (comment port))
            ((#\,) (if (char=? #\@ (peek-char port))
                       (begin (read-char port)
                              (vector 'reader-macro ",@"))
                       (vector 'reader-macro ",")))
            ((#\') (vector 'reader-macro "'"))
            ((#\`) (vector 'reader-macro "`"))
            (else  (if (char-whitespace? c)
                       (whitespace c port)
                       (symbol c port)))))))
  (define (hash port)
    (if (char=? #\( (peek-char port))
        (begin (read-char port)
               (vector 'open-paren "#("))
        (vector 'hash-expr (string-append "#" (read-token port)))))
  (define (string port)
    (vector 'string (string-append "\"" (read-string port))))
  (define (comment port)
    (vector 'comment (string-append ";" (read-line port))))
  (define (whitespace c port)
    (vector 'whitespace (string-append (make-string 1 c)
                                       (read-whitespace port))))
  (define (symbol c port)
    (vector 'symbol (string-append (make-string 1 c)
                                   (read-token port))))
  (start port))

;; Read a single token from PORT. A token is delimited by whitespace
;; or the characters in *SCHEME-TOKEN-DELIMITERS*.
(define (read-token port)
  (read-until (lambda (c)
                (or (memv c *scheme-token-delimiters*)
                    (char-whitespace? c)))
              port
              #t ; quote
              #f ; don't include delimiter
              ))

;; Read a scheme string from PORT, adhering to escapes.
(define (read-string port)
  (read-until (lambda (c)
                (char=? #\" c))
              port
              #t ; quote
              #t ; and include delimiter
              ))

;; Read a bunch of whitespace.
(define (read-whitespace port)
  (read-until (lambda (c)
                (not (char-whitespace? c)))
              port
              #f ; don't quote
              #f ; don't include delimiter
              ))

;; Read up to and including the next #\newline
(define (read-line port)
  (read-until (lambda (c)
                (char=? c #\newline))
              port
              #f
              #t))

;; The main delimited reading procedure. We read characters from PORT
;; until EOF or STOP? returns a true value for the character. That
;; character is added if INCLUDE? is not false, and characters after a
;; backslash aren't considered to stop the string of QUOTE? is not
;; false.
(define (read-until stop? port quote? include?)
  (let loop ((c (peek-char port))
             (l '()))
    (cond
     ((eof-object? c)
      (read-char port)
      (list->string (reverse l)))
     ((and quote?
           (char=? c #\\))
      (read-char port)
      (let ((c2 (read-char port)))
        (if (eof-object? c2)
            (list->string (reverse l))
            (loop (peek-char port)
                  (cons c2 (cons c l))))))
     ((stop? c)
      (if include?
          (begin (read-char port)
                 (list->string (reverse (cons c l))))
          (list->string (reverse l))))
     (else
      (read-char port)
      (loop (peek-char port)
            (cons c l))))))

;;;;;;;;;;;;;;
;;; The parser

;; The parser transforms the stream of lexer tokens into an SXML tree,
;; adding SPAN tags so later, CSS can add useful markup.

;; Simple accessor procedures.
(define (token-name token)
  (vector-ref token 0))
(define (token-string token)
  (vector-ref token 1))

;; The main parser entrance point.
(define (parse-scheme lex-tokens keywords)
  (let loop ((tokens lex-tokens)
             (lis '()))
    (if (null? tokens)
        (reverse lis)
        (receive (expr rest)
            (parse-scheme-expr tokens keywords)
          (loop rest
                (cons expr lis))))))

;; Remove a single expression from TOKENS, and return that parsed
;; expression and the rest of TOKENS.
;; This depends on never being called with an empty TOKENS list.
(define (parse-scheme-expr tokens keywords)
  (case (token-name (car tokens))
    ((open-paren)
     (parse-scheme-parens tokens keywords))
    ((close-paren)
     (raise (cons 'parse-error "Spurious closing paren found")))
    ((string comment)
     (values `(span (@ (class ,(symbol->string (token-name (car tokens)))))
                    ,(token-string (car tokens)))
             (cdr tokens)))
    ((symbol hash-expr reader-macro)
     (values (highlight-symbol (token-string (car tokens))
                               (token-name (car tokens))
                               keywords)
             (cdr tokens)))
    ((whitespace)
     (values (token-string (car tokens))
             (cdr tokens)))
    (else
     (error "Unknown lexem" (car tokens)))))

;; Remove the parenthised expression at the beginning of TOKENS, and
;; return as well as the rest of TOKENS.
(define (parse-scheme-parens tokens keywords)
  (let loop ((rest (cdr tokens))
             (l (list (token-string (car tokens)))))
    (cond
     ((null? rest)
      (raise (cons 'parse-error "Closing paren missing.")))
     ((eq? 'close-paren (token-name (car rest)))
      (values `(span (@ (class "paren"))
                     ,@(reverse (cons (token-string (car rest))
                                      l)))
              (cdr rest)))
     (else
      (receive (expr new-rest)
          (parse-scheme-expr rest keywords)
        (loop new-rest (cons expr l)))))))

;; Highlight a single symbol, SYM.
(define (highlight-symbol sym token-class keywords)
  (let* ((entry (assq (string->symbol (string-downcase sym))
                      keywords))
         (class (cond
                 ((eq? token-class 'hash-expr)
                  "hash-expr")
                 ((eq? token-class 'reader-macro)
                  "reader-macro")
                 ((and entry (cadr entry))
                  "keyword")
                 ((builtin? sym)
                  "builtin")
                 ((type? sym)
                  "type")
                 (else
                  #f)))
         (url (and entry
                   (caddr entry)))
         (link (if url
                   `(a (@ (href ,url)
                          (class "scheme-documentation"))
                       ,sym)
                   sym)))
    (if class
        `(span (@ (class ,class))
               ,link)
        link)))

;; Return a true value if we want to highlight SYM as a builtin, i.e.
;; an argument keyword.
(define (builtin? sym)
  (and (> (string-length sym) 0)
       (char=? #\: (string-ref sym 0))))

;; Return a true value if we want to highlight SYM as a type, e.g. the
;; commonly used <classname>.
(define (type? sym)
  (and (> (string-length sym) 2) ;; So we don't get <> from CUT
       (char=? #\< (string-ref sym 0))
       (char=? #\> (string-ref sym (- (string-length sym)
                                      1)))))
