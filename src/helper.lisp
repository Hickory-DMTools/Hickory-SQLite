(cl:in-package :hickory-sqlite-bindings~pristine)


(cffi:defcenum %sqlite::result-code
  (:ok         #.%sqlite:+ok+)
  (:error      #.%sqlite:+error+)
  (:internal   #.%sqlite:+internal+)
  (:perm       #.%sqlite:+perm+)
  (:abort      #.%sqlite:+abort+)
  (:busy       #.%sqlite:+busy+)
  (:locked     #.%sqlite:+locked+)
  (:nomem      #.%sqlite:+nomem+)
  (:readonly   #.%sqlite:+readonly+)
  (:interrupt  #.%sqlite:+interrupt+)
  (:ioerr      #.%sqlite:+ioerr+)
  (:corrupt    #.%sqlite:+corrupt+)
  (:notfound   #.%sqlite:+notfound+)
  (:full       #.%sqlite:+full+)
  (:cantopen   #.%sqlite:+cantopen+)
  (:protocol   #.%sqlite:+protocol+)
  (:empty      #.%sqlite:+empty+)
  (:schema     #.%sqlite:+schema+)
  (:toobig     #.%sqlite:+toobig+)
  (:constraint #.%sqlite:+constraint+)
  (:mismatch   #.%sqlite:+mismatch+)
  (:misuse     #.%sqlite:+misuse+)
  (:nolfs      #.%sqlite:+nolfs+)
  (:auth       #.%sqlite:+auth+)
  (:format     #.%sqlite:+format+)
  (:range      #.%sqlite:+range+)
  (:notadb     #.%sqlite:+notadb+)
  (:notice     #.%sqlite:+notice+)
  (:warning    #.%sqlite:+warning+)
  (:row        #.%sqlite:+row+)
  (:done       #.%sqlite:+done+))


(cffi:defcenum %sqlite::extended-code
  (:error-missing-collseq   #.%sqlite:+error-missing-collseq+)
  (:error-retry             #.%sqlite:+error-retry+)
  (:error-snapshot          #.%sqlite:+error-snapshot+)
  (:ioerr-read              #.%sqlite:+ioerr-read+)
  (:ioerr-short-read        #.%sqlite:+ioerr-short-read+)
  (:ioerr-write             #.%sqlite:+ioerr-write+)
  (:ioerr-fsync             #.%sqlite:+ioerr-fsync+)
  (:ioerr-dir-fsync         #.%sqlite:+ioerr-dir-fsync+)
  (:ioerr-truncate          #.%sqlite:+ioerr-truncate+)
  (:ioerr-fstat             #.%sqlite:+ioerr-fstat+)
  (:ioerr-unlock            #.%sqlite:+ioerr-unlock+)
  (:ioerr-rdlock            #.%sqlite:+ioerr-rdlock+)
  (:ioerr-delete            #.%sqlite:+ioerr-delete+)
  (:ioerr-blocked           #.%sqlite:+ioerr-blocked+)
  (:ioerr-nomem             #.%sqlite:+ioerr-nomem+)
  (:ioerr-access            #.%sqlite:+ioerr-access+)
  (:ioerr-checkreservedlock #.%sqlite:+ioerr-checkreservedlock+)
  (:ioerr-lock              #.%sqlite:+ioerr-lock+)
  (:ioerr-close             #.%sqlite:+ioerr-close+)
  (:ioerr-dir-close         #.%sqlite:+ioerr-dir-close+)
  (:ioerr-shmopen           #.%sqlite:+ioerr-shmopen+)
  (:ioerr-shmsize           #.%sqlite:+ioerr-shmsize+)
  (:ioerr-shmlock           #.%sqlite:+ioerr-shmlock+)
  (:ioerr-shmmap            #.%sqlite:+ioerr-shmmap+)
  (:ioerr-seek              #.%sqlite:+ioerr-seek+)
  (:ioerr-delete-noent      #.%sqlite:+ioerr-delete-noent+)
  (:ioerr-mmap              #.%sqlite:+ioerr-mmap+)
  (:ioerr-gettemppath       #.%sqlite:+ioerr-gettemppath+)
  (:ioerr-convpath          #.%sqlite:+ioerr-convpath+)
  (:ioerr-vnode             #.%sqlite:+ioerr-vnode+)
  (:ioerr-auth              #.%sqlite:+ioerr-auth+)
  (:ioerr-begin-atomic      #.%sqlite:+ioerr-begin-atomic+)
  (:ioerr-commit-atomic     #.%sqlite:+ioerr-commit-atomic+)
  (:ioerr-rollback-atomic   #.%sqlite:+ioerr-rollback-atomic+)
  (:ioerr-data              #.%sqlite:+ioerr-data+)
  (:ioerr-corruptfs         #.%sqlite:+ioerr-corruptfs+)
  (:locked-sharedcache      #.%sqlite:+locked-sharedcache+)
  (:locked-vtab             #.%sqlite:+locked-vtab+)
  (:busy-recovery           #.%sqlite:+busy-recovery+)
  (:busy-snapshot           #.%sqlite:+busy-snapshot+)
  (:busy-timeout            #.%sqlite:+busy-timeout+)
  (:cantopen-notempdir      #.%sqlite:+cantopen-notempdir+)
  (:cantopen-isdir          #.%sqlite:+cantopen-isdir+)
  (:cantopen-fullpath       #.%sqlite:+cantopen-fullpath+)
  (:cantopen-convpath       #.%sqlite:+cantopen-convpath+)
  (:cantopen-dirtywal       #.%sqlite:+cantopen-dirtywal+)
  (:cantopen-symlink        #.%sqlite:+cantopen-symlink+)
  (:corrupt-vtab            #.%sqlite:+corrupt-vtab+)
  (:corrupt-sequence        #.%sqlite:+corrupt-sequence+)
  (:corrupt-index           #.%sqlite:+corrupt-index+)
  (:readonly-recovery       #.%sqlite:+readonly-recovery+)
  (:readonly-cantlock       #.%sqlite:+readonly-cantlock+)
  (:readonly-rollback       #.%sqlite:+readonly-rollback+)
  (:readonly-dbmoved        #.%sqlite:+readonly-dbmoved+)
  (:readonly-cantinit       #.%sqlite:+readonly-cantinit+)
  (:readonly-directory      #.%sqlite:+readonly-directory+)
  (:abort-rollback          #.%sqlite:+abort-rollback+)
  (:constraint-check        #.%sqlite:+constraint-check+)
  (:constraint-commithook   #.%sqlite:+constraint-commithook+)
  (:constraint-foreignkey   #.%sqlite:+constraint-foreignkey+)
  (:constraint-function     #.%sqlite:+constraint-function+)
  (:constraint-notnull      #.%sqlite:+constraint-notnull+)
  (:constraint-primarykey   #.%sqlite:+constraint-primarykey+)
  (:constraint-trigger      #.%sqlite:+constraint-trigger+)
  (:constraint-unique       #.%sqlite:+constraint-unique+)
  (:constraint-vtab         #.%sqlite:+constraint-vtab+)
  (:constraint-rowid        #.%sqlite:+constraint-rowid+)
  (:constraint-pinned       #.%sqlite:+constraint-pinned+)
  (:constraint-datatype     #.%sqlite:+constraint-datatype+)
  (:notice-recover-wal      #.%sqlite:+notice-recover-wal+)
  (:notice-recover-rollback #.%sqlite:+notice-recover-rollback+)
  (:warning-autoindex       #.%sqlite:+warning-autoindex+)
  (:auth-user               #.%sqlite:+auth-user+)
  (:ok-load-permanently     #.%sqlite:+ok-load-permanently+)
  (:ok-symlink              #.%sqlite:+ok-symlink+))


(defun %sqlite::result-code->keyword (code)
  (cffi:foreign-enum-keyword '%sqlite::result-code code))

(defun %sqlite::keyword->result-code (keyword)
  (cffi:foreign-enum-value '%sqlite::result-code keyword))

(defun %sqlite::extended-code->keyword (code)
  (cffi:foreign-enum-keyword '%sqlite::extended-code code))

(defun %sqlite::keyword->extended-code (keyword)
  (cffi:foreign-enum-value '%sqlite::extended-code keyword))



(declaim (inline %sqlite::%text))

(defun %sqlite::%text (stmt i)
  (cffi:foreign-string-to-lisp
   (%sqlite:column-text stmt i)))


(declaim (inline %sqlite::%column-value))

(defun %sqlite::%column-value (stmt i)
  (ecase (%sqlite:column-type stmt i)
    (#.%sqlite:+integer+ (%sqlite:column-int stmt i))
    (#.%sqlite:+float+ (%sqlite:column-double stmt i))
    (#.%sqlite:+text+ (%sqlite::%text stmt i))
    (#.%sqlite:+blob+ (%sqlite:column-blob stmt i))
    (#.%sqlite:+null+ nil)))


(defun %sqlite::column-value (stmt i &optional cast)
  (if (not cast)
      (%sqlite::%column-value stmt i)
      (ecase cast
        (:integer (%sqlite:column-int stmt i))
        (:float (%sqlite:column-double stmt i))
        (:text (%sqlite::%text stmt i))
        (:blob (%sqlite:column-blob stmt i))
        (:null nil))))


(define-compiler-macro %sqlite::column-value (&whole whole stmt i &optional cast)
  (if (not cast)
      whole
      (ecase cast
        (:integer `(%sqlite:column-int ,stmt ,i))
        (:float `(%sqlite:column-double ,stmt ,i))
        (:text `(%sqlite:column-text ,stmt ,i))
        (:blob `(%sqlite:column-blob ,stmt ,i))
        (:null nil))))


(cffi:defctype %sqlite::*sqlite3 (:pointer (:struct %sqlite:sqlite3)))
(cffi:defctype %sqlite::*stmt (:pointer (:struct %sqlite:stmt)))


;;; Singleton to reduce consing.
(alexandria:define-constant %sqlite::+null-pointer+ (cffi:null-pointer)
  :test #'cffi:pointer-eq)


(defmacro %sqlite::vfs-slot (ptr name)
  `(cffi:foreign-slot-value
    ,ptr '(:struct %sqlite:vfs)
    ',(alexandria:ensure-symbol name :%sqlite)))


(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '%sqlite::result-code :%sqlite)
  (export '%sqlite::extended-code :%sqlite)
  (export '%sqlite::result-code->keyword :%sqlite)
  (export '%sqlite::keyword->result-code :%sqlite)
  (export '%sqlite::extended-code->keyword :%sqlite)
  (export '%sqlite::keyword->extended-code :%sqlite)
  (export '%sqlite::column-value :%sqlite)
  (export '%sqlite::*sqlite3 :%sqlite)
  (export '%sqlite::*stmt :%sqlite)
  (export '%sqlite::+null-pointer+ :%sqlite)
  (export '%sqlite::vfs-slot :%sqlite))