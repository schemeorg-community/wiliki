#! /usr/bin/env gosh

(use gauche.test)

(test-start "case-map")

(add-load-path "..")
(add-load-path "../lib")
(use srfi-13)
(use lang.case-map)

(define-syntax let-list*
  (syntax-rules ()
    ((_ (var ...) ls . body) (let-optionals* ls ((var #f) ...) . body))))

(for-each
 (lambda (t)
   (let-list* (base down title up locale) t
     (test* (format "downcase ~S ~S" base locale) down
       (if locale (downcase base locale) (downcase base)))
     (test* (format "titlecase ~S ~S" base locale) title
       (if locale (titlecase base locale) (titlecase base)))
     (test* (format "upcase ~S ~S" base locale) up
       (if locale (upcase base locale) (upcase base)))
     ))
 '(;; Latin
   ("aBcI" "abci" "Abci" "ABCI")
   ;; Cyrillic
   ("яЗи" "язи" "Язи" "ЯЗИ")
   ;; Greek, checking final sigma
   ("πΣΜΣ" "πσμς" "Πσμς" "ΠΣΜΣ")
   ;; wide Latin
   ("ａＢｃ" "ａｂｃ" "Ａｂｃ" "ＡＢＣ")
   ;; Roman Numerals
   ("ⅰⅢⅧⅹ" "ⅰⅲⅷⅹ" "Ⅰⅲⅷⅹ" "ⅠⅢⅧⅩ")
   ;; using \u, my font doesn't have the uppercase versions :(
   ("ⓐ\u24B7ⓒ" "ⓐⓑⓒ" "\u24B6ⓑⓒ" "\u24B6\u24B7\u24B8")
   ;; German, with everyone's favorite es-zed
   ("ßhöÜ" "ßhöü" "Sshöü" "SSHÖÜ")
   ;; Turkish
   ("iİıI" "iiıı" "İiıı" "İİII" "tr")
   ("iİıI" "i\u0069\u0307ıi" "I\u0069\u0307ıi" "IİII") ;; ick, denormalized
   ;;("" "" "" "")
   ))

(test-end)

