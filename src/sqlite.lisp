(defpackage :sqlite3
  (:use :cl)
  (:import-from #:%sqlite3
                #:*sqlite3
                #:*stmt
                #:column-value
                #:+null-pointer+)
  (:export #:sqlite-connection
           #:connect
           #:disconnect

           #:default-vfs-name))

(in-package :sqlite3)


(cffi:define-foreign-library libsqlite
  (t (:or (:default "libsqlite3")
          (:default "sqlite3"))))


;; load custom library if provided.
(let ((custom (uiop:getenv "HICKORY_SQLITE_LIBRARY")))
  (if custom
      (cffi:load-foreign-library custom)
      (cffi:use-foreign-library libsqlite)))


(defclass sqlite-connection ()
  ((filename :initarg :filename :reader connection-filename)
   (flags :initarg :flags :reader connection-flags)
   (vfs :initarg :vfs :reader connection-vfs)
   (handle :initarg nil)))


(defmacro with-ok (expr &body body)
  (alexandria:with-gensyms (g!code)
    `(let ((,g!code ,expr))
       (if (eq ,g!code %sqlite3:+ok+)
           (progn ,@body)
           (error (%sqlite3:errstr ,g!code))))))


(defmethod initialize-instance :after ((object sqlite-connection) &key)
  (cffi:with-foreign-object (handle-ptr '(:pointer *sqlite3))
    (with-slots (filename flags vfs handle) object
      (let* ((flags (cffi:foreign-bitfield-value '%sqlite3:open-flags flags))
             (result-code (%sqlite3:open-v2 filename handle-ptr flags vfs)))
        (if (eq result-code %sqlite3:+ok+)
            (setf handle (cffi:mem-ref handle-ptr '(:pointer *sqlite3)))
            (error (%sqlite3:errstr result-code))))))) ;; TODO: proper error handling


(defmacro with-connection-handle ((name conn) &body body)
  `(with-slots ((,name handle)) ,conn
     ,@body))


(defun prepare (conn sql)
  (cffi:with-foreign-object (handle-ptr '(:pointer *stmt))
    (with-connection-handle (db conn)
      (with-ok (%sqlite3:prepare-v2 db sql -1 handle-ptr +null-pointer+)
        (cffi:mem-ref handle-ptr '(:pointer *stmt))))))


(defun connect (&key (filename ":memory:") (flags '(:readwrite :create)) (vfs (%sqlite3:default-vfs-name)))
  (let ((conn (make-instance 'sqlite-connection :filename filename :flags flags :vfs vfs)))
    conn))


(defun disconnect (conn)
  (with-slots (handle) conn
    (%sqlite3:result-code->keyword
     (%sqlite3:close handle))))


(defmacro with-connection ((name path) &body body)
  `(let ((,name (connect ,path :flags '(:readonly :memory))))
     (unwind-protect
          (progn ,@body)
       (disconnect ,name))))


