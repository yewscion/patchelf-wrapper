#!/usr/bin/env -S guile -e main -s
-e main -s
!#
(use-modules ((srfi srfi-64))) ;; For Unit Testing
(load "library-file.scm")

(define (basic-tests)
  ;; Initialize and give a name to a simple testsuite.
  (test-begin "basic-tests")
  ;; Test our Hello World Function's Output to be as expected.
  (test-equal "Hello World!\n" "Hello World!\n")
  ;; End the test.
  (test-end "basic-tests"))

(define (main args)
  (basic-tests))
