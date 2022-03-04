(cl:defpackage sqlite
  (:use :cl :alexandria)
  (:export #:sqlite-connection
           #:connect
           #:disconnect

           #:default-vfs-name))

(in-package :sqlite)


(cffi:define-foreign-library %sqlite
  (:darwin (:default "libsqlite3"))
  (:unix (:or "libsqlite3.so" "libsqlite3.so"))
  (t (:or (:default "libsqlite3")
          (:default "sqlite3"))))


(cffi:use-foreign-library %sqlite)


;;; Singleton to reduce consing.
(define-constant +null-pointer+ (cffi:null-pointer)
  :test #'cffi:pointer-eq)


(defmacro %vfs-slot (ptr name)
  `(cffi:foreign-slot-value
    ,ptr '(:struct %sqlite:sqlite3-vfs)
    ',(ensure-symbol name :%sqlite)))


(defun default-vfs-name ()
  (let ((ptr (%sqlite:sqlite3-vfs-find +null-pointer+)))
    (if (cffi:null-pointer-p ptr)
        (error "Should not be null!!!")
        (%vfs-slot ptr name))))


(defclass sqlite-connection ()
  ((filename :initarg :filename :reader connection-filename)
   (flags :initarg :flags :reader connection-flags)
   (vfs :initarg :vfs :reader connection-vfs)
   (handle :initarg nil)))


(defmethod initialize-instance :after ((object sqlite-connection) &key)
  (cffi:with-foreign-object (handle-ptr '(:pointer %sqlite:*sqlite3))
    (with-slots (filename flags vfs handle) object
      (let* ((vfs-name (or vfs (default-vfs-name)))
             (result-code (%sqlite:sqlite3-open-v2 filename handle-ptr flags vfs-name)))
        (if (eq result-code :ok)
            (setf handle (cffi:mem-ref handle-ptr '(:pointer %sqlite:*sqlite3))
                  vfs vfs-name)
            (error result-code))))))


(defun connect (&key (filename ":memory:") (flags '(:readwrite :create)) (vfs nil))
  (let ((conn (make-instance 'sqlite-connection :filename filename :flags flags :vfs vfs)))
    conn))


(defun disconnect (conn)
  (with-slots (handle) conn
    (%sqlite:sqlite3-close handle)))


(defmacro with-connection ((name path) &body body)
  `(let ((,name (connect ,path :flags '(:readonly :memory))))
     (unwind-protect
          (progn ,@body)
       (disconnect ,name))))


(defmacro with-connection-handle ((name conn) &body body)
  `(with-slots ((,name handle)) ,conn
     ,@body))
