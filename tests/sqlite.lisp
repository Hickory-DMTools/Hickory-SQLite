(defpackage hickory-sqlite/tests/sqlite
  (:use :cl
        :sqlite
        :rove))

(in-package :hickory-sqlite/tests/sqlite)

;; NOTE: To run this test file, execute `(asdf:test-system :hickory-sqlite)' in your Lisp.

(deftest test-target-1
    (testing "should (= 1 1) to be true"
             (ok (= 1 1))))
