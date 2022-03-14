(uiop:define-package :hickory-sqlite
  (:use :cl))


(claw:defwrapper (:hickory-sqlite
                  (:system :hickory-sqlite/wrapper)
                  (:headers "sqlite3.h")
                  (:includes :sqlite-includes)
                  (:targets ((:and :x86-64 :linux) "x86_64-pc-linux-gnu")
                            ((:and :x86-64 :windows) "x86_64-w64-mingw32")
                            ((:and :x86-64 :darwin) "x86_64-apple-darwin-gnu"))
                  (:include-extra-values)
                  (:include-definitions "^SQLITE\\w+" "^sqlite3_\\w+")
                  (:exclude-definitions
                   ;; not relevant
                   "^SQLITE_API$"
                   "^SQLITE_APICALL$"
                   "^SQLITE_CALLBACK$"
                   "^SQLITE_CDECL$"
                   "^SQLITE_DEPRECATED$"
                   "^SQLITE_EXPERIMENTAL$"
                   "^SQLITE_EXTERN$"
                   "^SQLITE_STDCALL$"
                   "^SQLITE_SYSAPI$"
                   "^SQLITE3_H$"

                   ;; version has to be dinamically checked
                   "^SQLITE_VERSION$"
                   "^SQLITE_VERSION_NUMBER$"
                   "^SQLITE_SOURCE_ID$"

                   ;; manually created in supplement.lisp
                   "^sqlite3_column_text$"
                   "^SQLITE_STATIC$"
                   "^SQLITE_TRANSIENT$")
                  (:persistent t :depends-on ()))
  :in-package :%sqlite
  :recognize-strings t
  :inline-functions nil
  :symbolicate-names (:in-pipeline
                      (:by-removing-prefixes "SQLITE_" "sqlite3_")))
