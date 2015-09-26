(in-package :cl-user)
(defpackage cl-naive-bayes
  (:use :cl)
  (:import-from :anaphora
                :aif
                :sif
                :slet
                :it)
  (:export :make-category-data
           :make-learned-store))
(in-package :cl-naive-bayes)

(cl-annot:enable-annot-syntax)

@export
(defstruct category-data
  (count 0)
  (word-count (make-hash-table :test #'equal)))

@export
(defstruct learned-store
  (category-data (make-hash-table :test #'equal))
  (num-document 0)
  (num-word-kind 0))

(defun count-word-in-category (store word category)
  (aif (gethash category (learned-store-category-data store))
       (aif (gethash word (category-data-word-count it))
            it
            0)
       0))

(defun count-category (store category)
  (aif (gethash category (learned-store-category-data store))
       (let ((sum 0))
         (maphash #'(lambda (k v)
                      (declare (ignore k))
                      (incf sum v))
                  (category-data-word-count it))
         sum)
       0))

(defun count-word-kind (store)
  (learned-store-num-word-kind store))

; using Laplace Smoothing
(defun calc-logged-likelihood (store word-lst category)
  (let ((denomi (max (+ (count-category store category)
                        (count-word-kind store))
                     1))
        (logged-numer (loop for word in word-lst
                         sum (log (1+ (count-word-in-category store word category))))))
    (- logged-numer (log denomi))))

(defun calc-logged-prior-prob (store category)
  (with-slots (category-data num-document) store
    (log (/ (category-data-count (gethash category category-data))
            num-document))))

@export
(defun sort-category-by-prob (store word-lst)
  (let (lst)
    (maphash #'(lambda (category v)
                 (declare (ignore v))
                 (push (cons category
                             (+ (calc-logged-prior-prob store category)
                                (calc-logged-likelihood store word-lst category)))
                       lst))
             (learned-store-category-data store))
    (sort lst #'> :key #'cdr)))

@export
(defun sort-category-with-post-prob (store word-lst)
  (let ((sorted (sort-category-by-prob store word-lst))
        (sum-likelihood 0))
    (dolist (elem sorted)
      (incf sum-likelihood (exp (cdr elem))))
    (mapcar #'(lambda (elem)
                (cons (car elem)
                      (/ (exp (cdr elem)) sum-likelihood)))
            sorted)))

(defun contains-word (store word)
  (maphash #'(lambda (k cat-data)
               (declare (ignore k))
               (aif (gethash word (category-data-word-count cat-data))
                    (return-from contains-word it)))
           (learned-store-category-data store))
  nil)

(defun 1+plus (x)
  (if (null x)
      1
      (1+ x)))

(define-modify-macro incf-plus () 1+plus)

@export
(defun learn-a-document (store word-lst category)
  (with-slots (category-data num-document num-word-kind) store
    (incf num-document)
    (slet (gethash category category-data)
      (if (null it)
          (setf it (make-category-data)))
      (incf (category-data-count it))
      (dolist (word word-lst)
        (if (not (contains-word store word))
            (incf num-word-kind))
        (incf-plus (gethash word (category-data-word-count it)))))))
