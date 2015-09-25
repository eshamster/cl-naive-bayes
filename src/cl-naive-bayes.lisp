(in-package :cl-user)
(defpackage cl-naive-bayes
  (:use :cl)
  (:import-from :anaphora
                :aif
                :it))
(in-package :cl-naive-bayes)

; hash = category -> word -> count
(defstruct learned-store
  (hash (make-hash-table :test #'equal))
  (num-word-kind 0))

(defun count-word-in-category (store word category)
  (aif (gethash category (learned-store-hash store))
       (aif (gethash word it)
            it
            0)
       0))

(defun count-category (store category)
  (aif (gethash category (learned-store-hash store))
       (let ((sum 0))
         (maphash #'(lambda (k v)
                      (declare (ignore k))
                      (incf sum v))
                  it)
         sum)
       0))

(defun count-word-kind (store)
  (learned-store-num-word-kind store))

; using Laplace Smoothing
(defun get-word-logged-likelihood (store word-lst category)
  (let ((denomi (max (+ (count-category store category)
                        (count-word-kind store))
                     1))
        (logged-numer (loop for word in word-lst
                         sum (log (1+ (count-word-in-category store word category))))))
    (- logged-numer (log denomi))))
