
(define-module lang.case-map
  (use srfi-2)
  (use srfi-13)
  (use lang.char-set)
  (use gauche.uvector)
  (export char-upcase* char-downcase* char-titlecase*
          upcase downcase titlecase))
(select-module lang.case-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple case conversions

(define *char-case-file-1*
  (format "~A/case-map-1.dat" (gauche-site-library-directory)))

(define *char-case-table-1*
  (with-error-handler
      (lambda (err . args)
        (warn "couldn't load case-map-1.dat")
        (make-u32vector 0))
    (lambda ()
      (and-let* ((stat (sys-stat *char-case-file-1*))
                 (size (slot-ref stat 'size))
                 (vec (make-u32vector (quotient size 4))))
        (call-with-input-file *char-case-file-1*
          (cut read-block! vec <>))
        vec))))

(define *char-case-count-1*
  (- (quotient (u32vector-length *char-case-table-1*) 4) 1))

(define (char-case-index tab i)
  (if (zero? (u32vector-length tab))
    0
    (do ((j 0 (+ j 4)))
        ((>= (u32vector-ref tab j) i) (quotient j 4)))))

(define *index-2500* (char-case-index *char-case-table-1* #x2500))
(define *index-FF20* (char-case-index *char-case-table-1* #xFF20))

(define (char-case-search tab i off . opt)
  (let-optionals* opt ((min 0) (max *char-case-count-1*))
    (and
     (>= max min)
     (cond
       ((= i (u32vector-ref tab (* min 4)))
        (u32vector-ref (+ (* min 4) off)))
       ((= i (u32vector-ref tab (* max 4)))
        (u32vector-ref (+ (* max 4) off)))
       (else
        (let loop ((a min) (b max))
          (if (= a b)
            #f
            (let* ((mid (+ a (quotient (- b a) 2)))
                   (ind (* mid 4))
                   (val (u32vector-ref tab ind)))
              (cond ((< i val) (if (= mid b) #f (loop a mid)))
                    ((> i val) (if (= mid a) #f (loop mid b)))
                    (else (u32vector-ref tab (+ ind off))))))))))))

(define (char-map-single-case i off)
  (cond ((< i 128) #f)
        ((< i #x2500)
         (and-let* ((j (char-case-search *char-case-table-1*
                                         i off 0 *index-2500*)))
           (ucs->char j)))
        ((> i #xFF20)
         (and-let* ((j (char-case-search *char-case-table-1*
                                         i off *index-FF20*
                                         *char-case-count-1*)))
           (ucs->char j)))
        (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; special casing

(define *char-case-file-2*
  (format "~A/case-map-2.dat" (gauche-site-library-directory)))

(define *char-case-table-2*
  (with-error-handler
      (lambda (err . args) (warn "couldn't load case-map-2.dat") #())
    (cut with-input-from-file *char-case-file-2* read)))

(define *char-case-length-2* (vector-length *char-case-table-2*))

(define (char-map-multi-case i off)
  (let loop ((a 0) (b *char-case-length-2*))
    (if (= a b)
      #f
      (let* ((mid (+ a (quotient (- b a) 2)))
             (vec (vector-ref *char-case-table-2* mid))
             (val (vector-ref vec 0)))
        (cond ((< i val) (if (= mid b) #f (loop a mid)))
              ((> i val) (if (= mid a) #f (loop mid b)))
              (else (vector-ref vec off)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface

;; returns a single char
(define (char-upcase-single c)
  (let ((i (char->ucs c)))
    (if (< i 128)
      (char-upcase c)
      (or (char-map-single-case i 1) c))))
(define (char-downcase-single c)
  (let ((i (char->ucs c)))
    (if (< i 128)
      (char-downcase c)
      (or (char-map-single-case i 2) c))))
(define (char-titlecase-single c)
  (let ((i (char->ucs c)))
    (if (< i 128)
      (char-upcase c)
      (or (char-map-single-case i 3) c))))

;; may return a char or string
(define (char-downcase* c)
  (or (char-map-multi-case (char->ucs c) 1)
      (char-downcase-single c)))
(define (char-titlecase* c)
  (or (char-map-multi-case (char->ucs c) 2)
      (char-titlecase-single c)))
(define (char-upcase* c)
  (or (char-map-multi-case (char->ucs c) 3)
      (char-upcase-single c)))

(define (opt? pred opt)
  (and (pair? opt) (pred (car opt))))

(define (peek-char-is? pred)
  (let ((c (peek-char)))
    (and (not (eof-object? c))
         (if (char-set? pred)
           (char-set-contains? pred c)
           (pred c)))))

(define (with-string-io* s thunk)
  (with-output-to-string
    (lambda ()
      (with-input-from-port (if (string? s) (open-input-string s) s)
        thunk))))

;; takes an optional locale string
(define (upcase str . opt)
  (with-string-io* str
    (lambda ()
      (let loop ((c (read-char))
                 (prev #f))
        (unless (eof-object? c)
          (display
           (or
            (case c
              ;; Turkish and Azeri
              ((#\u0069) (if (opt? #/^(tr|az)/i opt) "\u0130" #\I))
              (else #f))
            (char-upcase* c)))
          (loop (read-char) c))))))

(define (downcase str . opt)
  (with-string-io* str
    (lambda ()
      (let loop ((c (read-char))
                 (prev #f))
        (unless (eof-object? c)
          (display
           (or
            (case c
              ;; Final Sigma
              ((#\u03A3) (if (not (peek-char-is? char-set:greek)) #\u03C2 #\u03C3))
              ;; Lithuanian (XXXX add More_Above logic)
              ((#\u00CC) (and (opt? #/^lt/i opt) "\u0069\u0307\u0300"))
              ((#\u00CD) (and (opt? #/^lt/i opt) "\u0069\u0307\u0301"))
              ((#\u0128) (and (opt? #/^lt/i opt) "\u0069\u0307\u0303"))
              ;; Turkish and Azeri
              ((#\u0130) (if (opt? #/^(tr|az)/i opt) #\u0069 "\u0069\u0307"))
              ((#\u0307) (and (opt? #/^(tr|az)/i opt) ""))
              ((#\u0049) (and (opt? #/^(tr|az)/i opt) #\u0131))
              (else #f))
            (char-downcase* c)))
          (loop (read-char) c))))))

;; Note: there are some characters which define case mappings (such as
;; the circled latin letters), but which unicode doesn't consider
;; alphabetic.  So the faster and more natural test for the alphabetic
;; property doesn't work, and we somewhat clumsily test whether or not
;; the characters are either upper or lowercase.
;;
;; An alternative approach is to explicitly compare the script property
;; of successive characters and start a new word when that property
;; changes.  So a consecutive string of Greek letters followed
;; immediately by Latin characters would result in the first Greek
;; letter and first Latin character being uppercased, as opposed to just
;; the first Greek letter as we do now.
(define (has-case? c)
  ;;(char-set-contains? char-set:alphabetic c)
  (or (char-set-contains? char-set:uppercase c)
      (char-set-contains? char-set:lowercase c)))

(define (titlecase str . opt)
  (with-string-io* str
    (lambda ()
      (let loop ((c (read-char))
                 (prev #f)
                 (in-word? #f))
        (unless (eof-object? c)
          (let ((letter? (has-case? c)))
            (display
             (if (and letter? (not in-word?))
               ;; start of word, titlecase
               (or
                (case c
                  ;; Turkish and Azeri
                  ((#\u0069) (if (opt? #/^(tr|az)/i opt) "\u0130" #\I))
                  (else #f))
                (char-titlecase* c))
               ;; non-word/in-word, lowercase
               (or
                (case c
                  ;; Final Sigma
                  ((#\u03A3) (if (not (peek-char-is? char-set:greek)) #\u03C2 #\u03C3))
                  ;; Lithuanian (XXXX add More_Above logic)
                  ((#\u00CC) (and (opt? #/^lt/i opt) "\u0069\u0307\u0300"))
                  ((#\u00CD) (and (opt? #/^lt/i opt) "\u0069\u0307\u0301"))
                  ((#\u0128) (and (opt? #/^lt/i opt) "\u0069\u0307\u0303"))
                  ;; Turkish and Azeri
                  ((#\u0130) (if (opt? #/^(tr|az)/i opt) #\u0069 "\u0069\u0307"))
                  ((#\u0307) (and (opt? #/^(tr|az)/i opt) ""))
                  ((#\u0049) (and (opt? #/^(tr|az)/i opt) #\u0131))
                  (else #f))
                (char-downcase* c))))
            (loop (read-char) c letter?)))))))

(provide "lang/case-map")
