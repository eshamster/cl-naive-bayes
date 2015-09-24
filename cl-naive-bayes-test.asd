#|
  This file is a part of cl-naive-bayes project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-naive-bayes-test-asd
  (:use :cl :asdf))
(in-package :cl-naive-bayes-test-asd)

(defsystem cl-naive-bayes-test
  :author "eshamster"
  :license ""
  :depends-on (:cl-naive-bayes
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-naive-bayes"))))
  :description "Test system for cl-naive-bayes"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
