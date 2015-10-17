(in-package :cl-user)
(defpackage cl-naive-bayes
  (:nicknames :nbayes)
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
  (sum-word-count 0)
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
       (category-data-sum-word-count it)
       0))

(defun count-word-kind (store)
  (learned-store-num-word-kind store))

; using Laplace Smoothing
(defun calc-logged-likelihood (store word-lst category)
  (let ((denomi (max (+ (count-category store category)
                        (count-word-kind store))
                     1)))
    (loop for word in word-lst
       sum (log (/ (1+ (count-word-in-category store word category))
                   denomi)))))

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
        (sum-post-prob-no-norm 0)
        (first-log nil))
    (if (null sorted) (return-from sort-category-with-post-prob nil))
    (setf first-log (cdar sorted))
    ; Ex. (log x, log y, log z) -> (0, log y/x, log z/y)
    (dolist (elem sorted)
      (let ((post-prob-no-norm (exp (- (cdr elem) first-log))))
        (setf (cdr elem) post-prob-no-norm)
        (incf sum-post-prob-no-norm post-prob-no-norm)))
    ; normalize
    (dolist (elem sorted)
      (setf (cdr elem) (/ (cdr elem) sum-post-prob-no-norm)))
    sorted))

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
        (incf (category-data-sum-word-count it))
        (if (not (contains-word store word))
            (incf num-word-kind))
        (incf-plus (gethash word (category-data-word-count it)))))))
