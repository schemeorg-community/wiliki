;;; Copyright (c) 2004 by Jorgen Schaefer
;;; This code is in the public domain.
(define-module dbm.gdbm-ci
  (extend dbm.gdbm)
  (export <gdbm-ci>)
  ;(use srfi-13)
  (use lang.case-map) ;; use this lib for real case-insensitivity
  )
(select-module dbm.gdbm-ci)

(define-class <gdbm-ci-meta> (<dbm-meta>)
  ())

(define-class <gdbm-ci> (<dbm>)
  ((gdbm :accessor gdbm-ci-gdbm
         :init-keyword :gdbm
         :init-form #f)
   (sync      :init-keyword :sync   :initform #f)
   (nolock    :init-keyword :nolock :initform #f)
   (bsize     :init-keyword :bsize  :initform 0))
  :metaclass <gdbm-ci-meta>)

(define-method dbm-open ((self <gdbm-ci>))
  (next-method)
  (let ((gdbm (make <gdbm>)))
    (for-each (lambda (field)
                (slot-set! gdbm
                           field
                           (slot-ref self field)))
              '(path rw-mode sync nolock bsize file-mode))
    (slot-set! self 'gdbm gdbm)
    (dbm-open gdbm))
  self)

(define-method dbm-close ((self <gdbm-ci>))
  (next-method)
  (dbm-close (gdbm-ci-gdbm self)))

(define-method dbm-closed? ((self <gdbm-ci>))
  (next-method)
  (dbm-closed? (gdbm-ci-gdbm self)))

(define-method dbm-put! ((self <gdbm-ci>) key value)
  (next-method)
  (dbm-put! (gdbm-ci-gdbm self) (downcase key) value))

(define-method dbm-get ((self <gdbm-ci>) key . args)
  (next-method)
  (apply dbm-get (gdbm-ci-gdbm self) (downcase key) args))

(define-method dbm-exists? ((self <gdbm-ci>) key)
  (next-method)
  (dbm-exists? (gdbm-ci-gdbm self) (downcase key)))

(define-method dbm-db-exists? ((class <gdbm-ci-meta>) name)
  (file-exists? name))

(define-method dbm-delete! ((self <gdbm-ci>) key)
  (next-method)
  (dbm-delete! (gdbm-ci-gdbm self) (downcase key)))

(define-method dbm-fold ((self <gdbm-ci>) proc knil)
  (next-method)
  (dbm-fold (gdbm-ci-gdbm self) proc knil))
