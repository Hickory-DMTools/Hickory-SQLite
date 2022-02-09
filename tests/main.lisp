(defpackage hickory-sqlite/tests/main
  (:use :cl
        :hickory-sqlite
        :rove))

(in-package :hickory-sqlite/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hickory-sqlite)' in your Lisp.

(deftest test-target-1
    (testing "should (= 1 1) to be true"
             (ok (= 1 1))))
