(defsystem "hickory-sqlite"
  :version "0.1.0"
  :author "Jonas Rodrigues <jxonas@acm.org>"
  :license "UNLICENSE"
  :depends-on ("cffi")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "hickory-sqlite/tests"))))

(defsystem "hickory-sqlite/tests"
  :author "Jonas Rodrigues <jxonas@acm.org>"
  :license "UNLICENSE"
  :depends-on ("hickory-sqlite"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for hickory-sqlite"
  :perform (test-op (op c) (symbol-call :rove :run c)))
