(defpackage :sqlite
  (:use :cl :alexandria)
  (:import-from #:%sqlite
                #:*sqlite3
                #:*stmt
                #:column-value
                #:+null-pointer+)
  (:export #:sqlite-connection
           #:connect
           #:disconnect

           #:default-vfs-name))

(in-package :sqlite)


(cffi:define-foreign-library libsqlite
  (t (:or (:default "libsqlite3")
          (:default "sqlite3"))))


;; load custom library if provided.
(let ((custom (uiop:getenv "HICKORY_SQLITE_LIBRARY")))
  (if custom
      (cffi:load-foreign-library custom)
      (cffi:use-foreign-library libsqlite)))


(defun default-vfs-name ()
  (let ((ptr (%sqlite:vfs-find +null-pointer+)))
    (if (cffi:null-pointer-p ptr)
        (error "Should not be null!!!")
        (%sqlite:vfs-slot ptr z-name))))


(defclass sqlite-connection ()
  ((filename :initarg :filename :reader connection-filename)
   (flags :initarg :flags :reader connection-flags)
   (vfs :initarg :vfs :reader connection-vfs)
   (handle :initarg nil)))


(defmacro with-ok (expr &body body)
  (with-gensyms (g!code)
    `(let ((,g!code ,expr))
       (if (eq ,g!code %sqlite:+ok+)
           (progn ,@body)
           (error ,g!code)))))


(defmethod initialize-instance :after ((object sqlite-connection) &key)
  (cffi:with-foreign-object (handle-ptr '(:pointer *sqlite3))
    (with-slots (filename flags vfs handle) object
      (let* ((vfs-name (or vfs (default-vfs-name)))
             (result-code (%sqlite:open filename handle-ptr))) ;; FIXME: use open-v2
        (if (eq result-code %sqlite:+ok+)
            (setf handle (cffi:mem-ref handle-ptr '(:pointer *sqlite3))
                  vfs vfs-name)
            (error result-code))))))


(defmacro with-connection-handle ((name conn) &body body)
  `(with-slots ((,name handle)) ,conn
     ,@body))


(defun prepare (conn sql)
  (cffi:with-foreign-object (handle-ptr '(:pointer *stmt))
    (with-connection-handle (db conn)
      (with-ok (%sqlite:prepare-v2 db sql -1 handle-ptr +null-pointer+)
        (cffi:mem-ref handle-ptr '(:pointer *stmt))))))


(defun connect (&key (filename ":memory:") (flags '(:readwrite :create)) (vfs nil))
  (let ((conn (make-instance 'sqlite-connection :filename filename :flags flags :vfs vfs)))
    conn))


(defun disconnect (conn)
  (with-slots (handle) conn
    (%sqlite:extended-code->keyword
     (%sqlite:close handle))))


(defmacro with-connection ((name path) &body body)
  `(let ((,name (connect ,path :flags '(:readonly :memory))))
     (unwind-protect
          (progn ,@body)
       (disconnect ,name))))


