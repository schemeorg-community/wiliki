#! /usr/bin/env gosh-euc

(use gauche.parseopt)

;; make a balanced binary tree from a sorted list
(define (make-binary-tree ls)
  (if (null? ls)
    #f
    (let loop ((hare ls) (tortoise ls) (res '()))
      (if (or (null? hare) (null? (cdr hare)))
        (list (car tortoise)
              (make-binary-tree (reverse res))
              (make-binary-tree (cdr tortoise)))
        (loop (cddr hare) (cdr tortoise) (cons (car tortoise) res))))))

(define (main args)
  (define geta (char->integer (ucs->char 41646)))
  (define ranges '())
  (let-args (cdr args) ((pretty? "p|pretty")
                        (inclusive? "i|inclusive|inclusive-only")
                        (exclusive? "e|exclusive|exclusive-only")
                        . rest)
    (if (eq? (gauche-character-encoding) 'utf-8)
      (errorf "~A: cannot be run in a utf-8 Gauche" (car args)))
    (if (or inclusive? (not exclusive?))
      (let loop ((i 128) (e -2) (k 0))
        (when (< i #xFFFF)
          (let* ((c (ucs->char i))
                 (e2 (char->integer c)))
            (cond
              ((= 1 (- e2 e))
               (loop (+ i 1) e2 (+ k 1)))
              (else
               (when (> k 3)
                 (if pretty?
                   (format #t " u: ~4,'0X-~4,'0X => j: ~4,'0X-~4,'0X (~2D)\n"
                           (- i k 1) (- i 1) (- e k) e (+ k 1))
                   (push! ranges (list (- i k 1) (- i 1) (- e k)))))
               (loop (+ i 1) e2 0)))))))
    (if (or exclusive? (not inclusive?))
      (let loop ((i 128) (k 0))
        (when (< i #xFFFF)
          (let* ((c (ucs->char i))
                 (e (char->integer c)))
            (cond
              ((= e geta)
               (loop (+ i 1) (+ k 1)))
              (else
               (when (> k 9)
                 (if pretty?
                   (format #t " u: ~4,'0X-~4,'0X != j (~2D)\n"
                           (- i k 1) (- i 1) (+ k 1))
                   (push! ranges (list (- i k 1) (- i 1)))))
               (loop (+ i 1) 0)))))))
    (unless pretty?
      (write (make-binary-tree (sort ranges (lambda (a b) (< (car a) (car b))))))
      (newline))
    0))
