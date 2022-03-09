(defsystem "hickory-sqlite"
  :description "Another Common Lisp SQLite Interface"
  :author "Jonas Oliveira Rodrigues <jxonas@acm.org>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("cffi")
  :components ((:module "src"
                :components
                ((:file "sqlite-ffi")
                 (:file "sqlite"))))
  :in-order-to ((test-op (test-op "hickory-sqlite/tests"))))


(defsystem "hickory-sqlite/tests"
  :description "Test system for hickory-sqlite"
  :author "Jonas Oliveira Rodrigues <jxonas@acm.org>"
  :license "MIT"
  :depends-on ("hickory-sqlite" "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
