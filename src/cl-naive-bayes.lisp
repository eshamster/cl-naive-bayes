(in-package :cl-user)
(defpackage cl-naive-bayes
  (:use :cl)
  (:import-from :anaphora
                :aif
                :sif
                :it))
(in-package :cl-naive-bayes)

(cl-annot:enable-annot-syntax)

@export
(defstruct learned-store
  (word-category-count (make-hash-table :test #'equal))
  (category-count (make-hash-table :test #'equal))
  (num-document 0)
  (num-word-kind 0))

(defun count-word-in-category (store word category)
  (aif (gethash category (learned-store-word-category-count store))
       (aif (gethash word it)
            it
            0)
       0))

(defun count-category (store category)
  (aif (gethash category (learned-store-word-category-count store))
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
(defun calc-logged-likelihood (store word-lst category)
  (let ((denomi (max (+ (count-category store category)
                        (count-word-kind store))
                     1))
        (logged-numer (loop for word in word-lst
                         sum (log (1+ (count-word-in-category store word category))))))
    (- logged-numer (log denomi))))

(defun calc-logged-prior-prob (store category)
  (with-slots (category-count num-document) store
    (log (/ (gethash category category-count)
            num-document))))

@export
(defun sort-category-by-prob (store word-lst)
  (let (lst)
    (maphash #'(lambda (category v)
                 (declare (ignore v))
                 (push (cons category
                             (* (calc-logged-prior-prob store category)
                                (calc-logged-likelihood store word-lst category)))
                       lst))
             (learned-store-category-count store))
    (sort lst #'> :key #'cdr)))

(defun contains-word (store word)
  (maphash #'(lambda (k word-hash)
               (declare (ignore k))
               (aif (gethash word word-hash)
                    (return-from contains-word it)))
           (learned-store-word-category-count store))
  nil)

@export
(defun learn-a-document (store word-lst category)
  (with-slots (word-category-count category-count num-document num-word-kind) store
    (incf num-document)
    (sif (gethash category category-count)
         (incf it)
         (progn (setf it 1)
                (setf (gethash category word-category-count) (make-hash-table :test #'equal))))
    (dolist (word word-lst)
      (if (not (contains-word store word))
          (incf num-word-kind))
      (sif (gethash word (gethash category word-category-count))
           (incf it)
           (setf it 1)))))
