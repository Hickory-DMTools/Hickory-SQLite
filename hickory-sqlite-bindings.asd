;; Generated by :claw at 2022-03-14T16:34:17.552700Z
(asdf:defsystem #:hickory-sqlite-bindings
  :description "Bindings generated by hickory-sqlite"
  :author "CLAW"
  :license "Public domain"
  :defsystem-depends-on (:trivial-features)
  :depends-on (:uiop :cffi)
  :components
  ((:file "bindings/x86_64-pc-linux-gnu" :if-feature (:and :x86-64 :linux))
   (:file "bindings/x86_64-w64-mingw32" :if-feature (:and :x86-64 :windows))
   (:file "bindings/x86_64-apple-darwin-gnu" :if-feature
    (:and :x86-64 :darwin))))
#-(:or (:and :x86-64 :darwin)(:and :x86-64 :windows)(:and :x86-64 :linux))
(warn "Current platform unrecognized or unsupported by hickory-sqlite-bindings system")