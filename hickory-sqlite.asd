(defsystem "hickory-sqlite"
  :description "Another Common Lisp SQLite Interface"
  :author "Jonas Rodrigues <jxonas@acm.org>"
  :license "UNLICENSE"
  :version "0.1.0"
  :depends-on ("cffi")
  :components ((:module "src"
                :components
                ((:file "libsqlite")
                 (:file "sqlite"))))
  :in-order-to ((test-op (test-op "hickory-sqlite/tests"))))


(defsystem "hickory-sqlite/tests"
  :description "Test system for hickory-sqlite"
  :author "Jonas Rodrigues <jxonas@acm.org>"
  :license "UNLICENSE"
  :depends-on ("hickory-sqlite" "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
