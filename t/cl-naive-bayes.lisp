(in-package :cl-user)
(defpackage cl-naive-bayes-test
  (:use :cl
        :cl-naive-bayes
        :prove))
(in-package :cl-naive-bayes-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-naive-bayes)' in your Lisp.

(plan nil)

(defstruct test-document
  category
  word-lst)

(defparameter *documents*
  (list (make-test-document :category "A"
                            :word-lst '("a1" "a2" "a3" "a4" "ab"))
        (make-test-document :category "A"
                            :word-lst '("a3" "a4" "a5" "a6"))
        (make-test-document :category "B"
                            :word-lst '("b1" "b2" "b3" "b4" "ab"))))

(defun add-a-test-document (store index)
  (let ((doc (nth index *documents*)))
    (learn-a-document store
                      (test-document-word-lst doc)
                      (test-document-category doc))))

(defun is-num-document (store expected)
  (is (cl-naive-bayes::learned-store-num-document store) expected))

(defun is-num-word-kind (store expected)
  (is (cl-naive-bayes::learned-store-num-word-kind store) expected))

(defun is-count-category (store category expected)
  (is (cl-naive-bayes::category-data-count
       (gethash category
                (cl-naive-bayes::learned-store-category-data store)))
      expected))

(defun is-count-word (store category word expected)
  (is (cl-naive-bayes::count-word-in-category store word category)
      expected))

(subtest "Test learning"
  (let ((store (make-learned-store)))
    (subtest "Add the first document (category: A)"
      (add-a-test-document store 0)
      (is-num-document store 1)
      (is-num-word-kind store 5)
      (is-count-category store "A" 1)
      (is-count-word store "A" "a1" 1)
      (is-count-word store "A" "ab" 1)
      (is-count-word store "A" "none" 0)
      (is-count-word store "none" "ab" 0))
    (subtest "Add the second document (category: A)"
      (add-a-test-document store 1)
      (is-num-document store 2)
      (is-num-word-kind store 7)
      (is-count-category store "A" 2)
      (is-count-word store "A" "a1" 1)
      (is-count-word store "A" "a3" 2)
      (is-count-word store "A" "a5" 1))
    (subtest "Add the third document (category: B)"
      (add-a-test-document store 2)
      (is-num-document store 3)
      (is-num-word-kind store 11)
      (is-count-category store "A" 2)
      (is-count-category store "B" 1)
      (is-count-word store "A" "a1" 1)
      (is-count-word store "A" "ab" 1)
      (is-count-word store "A" "b1" 0)
      (is-count-word store "B" "b1" 1)
      (is-count-word store "B" "ab" 1))))

(finalize)
