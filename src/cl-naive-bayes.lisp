(in-package :cl-user)
(defpackage cl-naive-bayes
  (:use :cl)
  (:import-from :anaphora
                :aif
                :it))
(in-package :cl-naive-bayes)

(defstruct word-category-count
  word
  category
  (count 0))

(defstruct learned-store
  lst
  (num-word-kind 0))

(defun count-word-in-category (store word category)
  (aif (find-if #'(lambda (elem)
                    (and (equal (word-category-count-word elem) word)
                         (equal (word-category-count-category elem) category)))
                (learned-store-lst store))
       (word-category-count-count it)
       0))

(defun count-category (store category)
  (loop for elem in (learned-store-lst store)
     when (equal (word-category-count-category elem) category)
     sum (word-category-count-count elem)))

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
