# Cl-Naive-Bayes

Cl-Naive-Bayes is Common Lisp library to use Naive Bayes classifier by simple interface.

## Usage

Cl-Naive-Bayes accepts two kinds of data. 

- document: a list of strings
    - Ex '("This" "is" "the" "cl-naive-bayes" "repository")
- category: a string
    - Ex. "normal", "spam"

Learning:

```lisp
(defparameter *store* (nbayes:make-learned-store))

; (category word-lst)
(defparameter *documents*
  (list '("A" ("a1" "a2" "a3" "a4" "ab"))
        '("A" ("a3" "a4" "a5" "a6"))
        '("B" ("b1" "b2" "b3" "b4" "ab"))
        '("C" ("c1" "c2" "c3"))))
                            
(dolist (doc *documents*)
  (nbayes:learn-a-document *store* (cadr doc) (car doc)))
```

Classifying:

If you want only the result of sorting, use *sort-category-by-prob* as below.

```lisp
(nbayes:sort-category-by-prob *store* '("a1" "ab" "c1" "new"))
=> ("A" "C" "B") 
```

On the other hand, if you also want post probabilities of each category, use *sort-category-with-post-prob* as below. 

```lisp
(nbayes:sort-category-with-post-prob *store* '("a1" "ab" "c1" "new"))
=> (("A" . 0.4211471) ("C" . 0.3527683) ("B" . 0.22608456))
```

*Note: New words in sorting are smoothed using Laplace Smoothing.* 

## Installation

This library has not been submitted to quicklisp repository (at 2015/10/17). So please do "git clone" this to a proper directory. Then,  

```lisp
(ql:quickload :cl-naive-bayes)
```

or

```lisp
(asdf:load-system :cl-naive-bayes)
```

## Author

* eshamster (hamgoostar@gmail.com)

## Copyright

Copyright (c) 2015 eshamster (hamgoostar@gmail.com)

## License

Licensed under the LLGPL License. 