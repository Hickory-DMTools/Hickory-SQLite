(defsystem "hickory-sqlite"
  :description "Ergonomic wrapper for using SQLite from Common Lisp"
  :version "0.1.0"
  :author "Jonas Oliveira Rodrigues <jxonas@acm.org>"
  :license "MIT"
  :depends-on ("hickory-sqlite-bindings")
  :components ((:module "src"
                :components
                ((:file "supplement")
                 (:file "sqlite"))))
  :in-order-to ((test-op (test-op "hickory-sqlite/tests"))))


(defsystem "hickory-sqlite/wrapper"
  :description "Ergonomic wrapper for using SQLite from Common Lisp"
  :version "0.1.0"
  :author "Jonas Oliveira Rodrigues <jxonas@acm.org>"
  :license "MIT"
  :depends-on ("alexandria" "uiop" "cffi" "claw-utils" "claw")
  :serial t
  :components ((:file "src/claw")
               (:module :sqlite-includes :pathname "src/lib/build/")))


(defsystem "hickory-sqlite/tests"
  :description "Test system for hickory-sqlite"
  :author "Jonas Oliveira Rodrigues <jxonas@acm.org>"
  :license "MIT"
  :depends-on ("hickory-sqlite" "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
