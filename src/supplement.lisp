(cl:in-package :hickory-sqlite-bindings~pristine)


(defmacro define-bitfield (name &body masks)
  (let ((name->value (alexandria:format-symbol :%sqlite3 "~a->VALUE" name))
        (value->name (alexandria:format-symbol :%sqlite3 "VALUE->~a" name)))
    `(progn
       (cffi:defbitfield ,name ,@masks)
       (declaim (inline ,name->value ,value->name))
       (defun ,name->value (flags) (cffi:foreign-bitfield-value ',name flags))
       (defun ,value->name (value) (cffi:foreign-bitfield-symbols ',name value))
       (export '(,name ,name->value ,value->name) :%sqlite3))))


(defmacro define-enum (name &body options)
  (let ((keyword->name (alexandria:format-symbol :%sqlite3 "KEYWORD->~a" name))
        (name->keyword (alexandria:format-symbol :%sqlite3 "~a->KEYWORD" name)))
    `(progn
       (cffi:defcenum ,name ,@options)
       (declaim (inline ,keyword->name ,name->keyword))
       (defun ,keyword->name (keyword) (cffi:foreign-enum-value ',name keyword))
       (defun ,name->keyword (value) (cffi:foreign-enum-keyword ',name value))
       (export '(,name ,keyword->name ,name->keyword) :%sqlite3))))


(defmacro define-constant-pointer (name value)
  `(progn
     (alexandria:define-constant ,name ,value :test #'cffi:pointer-eq)
     (export ',name :%sqlite3)))



;;; Manually create some omitted definitions in claw.lisp


(define-constant-pointer %sqlite3::+static+ (cffi:null-pointer))

(define-constant-pointer %sqlite3::+transient+
    (cffi:make-pointer (ldb (byte (* 8 (cffi:foreign-type-size :pointer)) 0) -1)))


(cffi:defcfun ("sqlite3_column_text" %sqlite3::column-text)
    :string
  (%sqlite3::arg0 (:pointer %sqlite3::stmt))
  (%sqlite3::i-col :int))



;;; Additional stuff


(define-enum %sqlite3::result-code
  (:ok         #.%sqlite3::+ok+)
  (:error      #.%sqlite3::+error+)
  (:internal   #.%sqlite3::+internal+)
  (:perm       #.%sqlite3::+perm+)
  (:abort      #.%sqlite3::+abort+)
  (:busy       #.%sqlite3::+busy+)
  (:locked     #.%sqlite3::+locked+)
  (:nomem      #.%sqlite3::+nomem+)
  (:readonly   #.%sqlite3::+readonly+)
  (:interrupt  #.%sqlite3::+interrupt+)
  (:ioerr      #.%sqlite3::+ioerr+)
  (:corrupt    #.%sqlite3::+corrupt+)
  (:notfound   #.%sqlite3::+notfound+)
  (:full       #.%sqlite3::+full+)
  (:cantopen   #.%sqlite3::+cantopen+)
  (:protocol   #.%sqlite3::+protocol+)
  (:empty      #.%sqlite3::+empty+)
  (:schema     #.%sqlite3::+schema+)
  (:toobig     #.%sqlite3::+toobig+)
  (:constraint #.%sqlite3::+constraint+)
  (:mismatch   #.%sqlite3::+mismatch+)
  (:misuse     #.%sqlite3::+misuse+)
  (:nolfs      #.%sqlite3::+nolfs+)
  (:auth       #.%sqlite3::+auth+)
  (:format     #.%sqlite3::+format+)
  (:range      #.%sqlite3::+range+)
  (:notadb     #.%sqlite3::+notadb+)
  (:notice     #.%sqlite3::+notice+)
  (:warning    #.%sqlite3::+warning+)
  (:row        #.%sqlite3::+row+)
  (:done       #.%sqlite3::+done+))


(define-enum %sqlite3::extended-code
  (:error-missing-collseq   #.%sqlite3::+error-missing-collseq+)
  (:error-retry             #.%sqlite3::+error-retry+)
  (:error-snapshot          #.%sqlite3::+error-snapshot+)
  (:ioerr-read              #.%sqlite3::+ioerr-read+)
  (:ioerr-short-read        #.%sqlite3::+ioerr-short-read+)
  (:ioerr-write             #.%sqlite3::+ioerr-write+)
  (:ioerr-fsync             #.%sqlite3::+ioerr-fsync+)
  (:ioerr-dir-fsync         #.%sqlite3::+ioerr-dir-fsync+)
  (:ioerr-truncate          #.%sqlite3::+ioerr-truncate+)
  (:ioerr-fstat             #.%sqlite3::+ioerr-fstat+)
  (:ioerr-unlock            #.%sqlite3::+ioerr-unlock+)
  (:ioerr-rdlock            #.%sqlite3::+ioerr-rdlock+)
  (:ioerr-delete            #.%sqlite3::+ioerr-delete+)
  (:ioerr-blocked           #.%sqlite3::+ioerr-blocked+)
  (:ioerr-nomem             #.%sqlite3::+ioerr-nomem+)
  (:ioerr-access            #.%sqlite3::+ioerr-access+)
  (:ioerr-checkreservedlock #.%sqlite3::+ioerr-checkreservedlock+)
  (:ioerr-lock              #.%sqlite3::+ioerr-lock+)
  (:ioerr-close             #.%sqlite3::+ioerr-close+)
  (:ioerr-dir-close         #.%sqlite3::+ioerr-dir-close+)
  (:ioerr-shmopen           #.%sqlite3::+ioerr-shmopen+)
  (:ioerr-shmsize           #.%sqlite3::+ioerr-shmsize+)
  (:ioerr-shmlock           #.%sqlite3::+ioerr-shmlock+)
  (:ioerr-shmmap            #.%sqlite3::+ioerr-shmmap+)
  (:ioerr-seek              #.%sqlite3::+ioerr-seek+)
  (:ioerr-delete-noent      #.%sqlite3::+ioerr-delete-noent+)
  (:ioerr-mmap              #.%sqlite3::+ioerr-mmap+)
  (:ioerr-gettemppath       #.%sqlite3::+ioerr-gettemppath+)
  (:ioerr-convpath          #.%sqlite3::+ioerr-convpath+)
  (:ioerr-vnode             #.%sqlite3::+ioerr-vnode+)
  (:ioerr-auth              #.%sqlite3::+ioerr-auth+)
  (:ioerr-begin-atomic      #.%sqlite3::+ioerr-begin-atomic+)
  (:ioerr-commit-atomic     #.%sqlite3::+ioerr-commit-atomic+)
  (:ioerr-rollback-atomic   #.%sqlite3::+ioerr-rollback-atomic+)
  (:ioerr-data              #.%sqlite3::+ioerr-data+)
  (:ioerr-corruptfs         #.%sqlite3::+ioerr-corruptfs+)
  (:locked-sharedcache      #.%sqlite3::+locked-sharedcache+)
  (:locked-vtab             #.%sqlite3::+locked-vtab+)
  (:busy-recovery           #.%sqlite3::+busy-recovery+)
  (:busy-snapshot           #.%sqlite3::+busy-snapshot+)
  (:busy-timeout            #.%sqlite3::+busy-timeout+)
  (:cantopen-notempdir      #.%sqlite3::+cantopen-notempdir+)
  (:cantopen-isdir          #.%sqlite3::+cantopen-isdir+)
  (:cantopen-fullpath       #.%sqlite3::+cantopen-fullpath+)
  (:cantopen-convpath       #.%sqlite3::+cantopen-convpath+)
  (:cantopen-dirtywal       #.%sqlite3::+cantopen-dirtywal+)
  (:cantopen-symlink        #.%sqlite3::+cantopen-symlink+)
  (:corrupt-vtab            #.%sqlite3::+corrupt-vtab+)
  (:corrupt-sequence        #.%sqlite3::+corrupt-sequence+)
  (:corrupt-index           #.%sqlite3::+corrupt-index+)
  (:readonly-recovery       #.%sqlite3::+readonly-recovery+)
  (:readonly-cantlock       #.%sqlite3::+readonly-cantlock+)
  (:readonly-rollback       #.%sqlite3::+readonly-rollback+)
  (:readonly-dbmoved        #.%sqlite3::+readonly-dbmoved+)
  (:readonly-cantinit       #.%sqlite3::+readonly-cantinit+)
  (:readonly-directory      #.%sqlite3::+readonly-directory+)
  (:abort-rollback          #.%sqlite3::+abort-rollback+)
  (:constraint-check        #.%sqlite3::+constraint-check+)
  (:constraint-commithook   #.%sqlite3::+constraint-commithook+)
  (:constraint-foreignkey   #.%sqlite3::+constraint-foreignkey+)
  (:constraint-function     #.%sqlite3::+constraint-function+)
  (:constraint-notnull      #.%sqlite3::+constraint-notnull+)
  (:constraint-primarykey   #.%sqlite3::+constraint-primarykey+)
  (:constraint-trigger      #.%sqlite3::+constraint-trigger+)
  (:constraint-unique       #.%sqlite3::+constraint-unique+)
  (:constraint-vtab         #.%sqlite3::+constraint-vtab+)
  (:constraint-rowid        #.%sqlite3::+constraint-rowid+)
  (:constraint-pinned       #.%sqlite3::+constraint-pinned+)
  (:constraint-datatype     #.%sqlite3::+constraint-datatype+)
  (:notice-recover-wal      #.%sqlite3::+notice-recover-wal+)
  (:notice-recover-rollback #.%sqlite3::+notice-recover-rollback+)
  (:warning-autoindex       #.%sqlite3::+warning-autoindex+)
  (:auth-user               #.%sqlite3::+auth-user+)
  (:ok-load-permanently     #.%sqlite3::+ok-load-permanently+)
  (:ok-symlink              #.%sqlite3::+ok-symlink+))


(define-bitfield %sqlite3::open-flags
  (:readonly      #.%sqlite3::+open-readonly+)
  (:readwrite     #.%sqlite3::+open-readwrite+)
  (:create        #.%sqlite3::+open-create+)
  (:deleteonclose #.%sqlite3::+open-deleteonclose+)
  (:exclusive     #.%sqlite3::+open-exclusive+)
  (:autoproxy     #.%sqlite3::+open-autoproxy+)
  (:uri           #.%sqlite3::+open-uri+)
  (:memory        #.%sqlite3::+open-memory+)
  (:main-db       #.%sqlite3::+open-main-db+)
  (:temp-db       #.%sqlite3::+open-temp-db+)
  (:transient-db  #.%sqlite3::+open-transient-db+)
  (:main-journal  #.%sqlite3::+open-main-journal+)
  (:temp-journal  #.%sqlite3::+open-temp-journal+)
  (:subjournal    #.%sqlite3::+open-subjournal+)
  (:super-journal #.%sqlite3::+open-super-journal+)
  (:nomutex       #.%sqlite3::+open-nomutex+)
  (:fullmutex     #.%sqlite3::+open-fullmutex+)
  (:sharedcache   #.%sqlite3::+open-sharedcache+)
  (:privatecache  #.%sqlite3::+open-privatecache+)
  (:wal           #.%sqlite3::+open-wal+)
  (:nofollow      #.%sqlite3::+open-nofollow+)
  (:exrescode     #.%sqlite3::+open-exrescode+))


(declaim (inline %sqlite3::%statement-value))

(defun %sqlite3::%statement-value (stmt i)
  (ecase (%sqlite3::column-type stmt i)
    (#.%sqlite3::+integer+ (%sqlite3::column-int stmt i))
    (#.%sqlite3::+float+ (%sqlite3::column-double stmt i))
    (#.%sqlite3::+text+ (%sqlite3::column-text stmt i))
    (#.%sqlite3::+blob+ (%sqlite3::column-blob stmt i))
    (#.%sqlite3::+null+ nil)))


(defun %sqlite3::statement-value (stmt i &optional cast)
  (if (not cast)
      (%sqlite3::%statement-value stmt i)
      (ecase cast
        (:integer (%sqlite3::column-int stmt i))
        (:float (%sqlite3::column-double stmt i))
        (:text (%sqlite3::column-text stmt i))
        (:blob (%sqlite3::column-blob stmt i))
        (:null nil))))


(define-compiler-macro %sqlite3::statement-value (&whole whole stmt i &optional cast)
  (if (not cast)
      whole
      (ecase cast
        (:integer `(%sqlite3::column-int ,stmt ,i))
        (:float `(%sqlite3::column-double ,stmt ,i))
        (:text `(%sqlite3::column-text ,stmt ,i))
        (:blob `(%sqlite3::column-blob ,stmt ,i))
        (:null nil))))


(cffi:defctype %sqlite3::*sqlite3 (:pointer (:struct %sqlite3::sqlite3)))
(cffi:defctype %sqlite3::*stmt (:pointer (:struct %sqlite3::stmt)))


;;; Singleton to reduce consing
(define-constant-pointer %sqlite3::+null-pointer+ (cffi:null-pointer))


(defmacro %sqlite3::vfs-slot (ptr name)
  `(cffi:foreign-slot-value
    ,ptr '(:struct %sqlite3::vfs)
    ',(alexandria:ensure-symbol name :%sqlite3)))


(defun %sqlite3::default-vfs-name ()
  (let ((ptr (%sqlite3::vfs-find %sqlite3::+null-pointer+)))
    (unless (cffi:null-pointer-p ptr)
      (%sqlite3::vfs-slot ptr z-name))))


(export
 '(%sqlite3::statement-value
   %sqlite3::*sqlite3
   %sqlite3::stmt
   %sqlite3::vfs-slot
   %sqlite3::default-vfs-name
   %sqlite3::column-text)
 :%sqlite3)
