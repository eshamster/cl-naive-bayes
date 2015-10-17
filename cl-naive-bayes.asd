#|
  This file is a part of cl-naive-bayes project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-naive-bayes-asd
  (:use :cl :asdf))
(in-package :cl-naive-bayes-asd)

(defsystem cl-naive-bayes
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:cl-annot
               :anaphora)
  :components ((:module "src"
                :components
                ((:file "cl-naive-bayes"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-naive-bayes-test))))
