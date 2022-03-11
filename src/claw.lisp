(uiop:define-package :hickory-sqlite
  (:use :cl))


(claw:defwrapper (:hickory-sqlite
                  (:system :hickory-sqlite/wrapper)
                  (:headers "sqlite3.h")
                  (:includes :sqlite-includes)
                  (:defines)
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :x86-64 :windows) "x86_64-w64-mingw32")
                            ((:and :x86-64 :darwin) "x86_64-apple-darwin-gnu"))
                  (:include-extra-values)
                  (:include-definitions "^SQLITE\\w+" "^sqlite3_\\w+")
                  (:persistent t :depends-on ()))
  :in-package :%sqlite
  :recognize-strings t
  :inline-functions nil
  :symbolicate-names (:in-pipeline
                      (:by-removing-prefixes "SQLITE_" "sqlite3_")))
