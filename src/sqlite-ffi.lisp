(defpackage :sqlite-ffi
  (:use :cl :cffi)
  (:export #:sqlite3
           #:*sqlite3
           #:sqlite3-vfs
           #:sqlite3-open-v2
           #:sqlite3-close
           #:sqlite3-vfs-find
           #:sqlite3-stmt
           #:*sqlite3-stmt
           #:sqlite3-prepare-v2
           #:name))

(in-package :sqlite-ffi)


(defmacro define-constants/cenum (name &body codes)
  `(progn
     ,@(loop :for (key code doc) :in codes
             :for sym = (alexandria:format-symbol *package* "+~a+" key)
             :collect `(defconstant ,sym ,code ,doc))
     (cffi:defcenum ,name
       ,@(loop :for (key code doc) :in codes
               :collect (list key code)))))


(define-constants/cenum result-code
  (:ok           0    "Successful result")
  (:error        1    "Generic error")
  (:internal     2    "Internal logic error in sqlite")
  (:perm         3    "Access permission denied")
  (:abort        4    "Callback routine requested an abort")
  (:busy         5    "The database file is locked")
  (:locked       6    "A table in the database is locked")
  (:nomem        7    "A malloc() failed")
  (:readonly     8    "Attempt to write a readonly database")
  (:interrupt    9    "Operation terminated by sqlite3_interrupt()")
  (:ioerr       10    "Some kind of disk i/o error occurred")
  (:corrupt     11    "The database disk image is malformed")
  (:notfound    12    "Unknown opcode in sqlite3_file_control()")
  (:full        13    "Insertion failed because database is full")
  (:cantopen    14    "Unable to open the database file")
  (:protocol    15    "Database lock protocol error")
  (:empty       16    "Internal use only")
  (:schema      17    "The database schema changed")
  (:toobig      18    "String or blob exceeds size limit")
  (:constraint  19    "Abort due to constraint violation")
  (:mismatch    20    "Data type mismatch")
  (:misuse      21    "Library used incorrectly")
  (:nolfs       22    "Uses os features not supported on host")
  (:auth        23    "Authorization denied")
  (:format      24    "Not used")
  (:range       25    "2nd parameter to sqlite3_bind out of range")
  (:notadb      26    "File opened that is not a database file")
  (:notice      27    "Notifications from sqlite3_log()")
  (:warning     28    "Warnings from sqlite3_log()")
  (:row         100   "sqlite3_step() has another row ready")
  (:done        101  "sqlite3_step() has finished executing"))



(macrolet ((extended-codes (&body codes)
             `(define-constants/cenum extended-result-code
                ,@(loop :for (extended base i doc) :in codes
                        :collect (list extended
                                       (logior
                                        (foreign-enum-value 'result-code base)
                                        (ash i 8))
                                       doc)))))
  (extended-codes
   (:error-missing-collseq    :error        1  "")
   (:error-retry              :error        2  "")
   (:error-snapshot           :error        3  "")
   (:ioerr-read               :ioerr        1  "")
   (:ioerr-short-read         :ioerr        2  "")
   (:ioerr-write              :ioerr        3  "")
   (:ioerr-fsync              :ioerr        4  "")
   (:ioerr-dir-fsync          :ioerr        5  "")
   (:ioerr-truncate           :ioerr        6  "")
   (:ioerr-fstat              :ioerr        7  "")
   (:ioerr-unlock             :ioerr        8  "")
   (:ioerr-rdlock             :ioerr        9  "")
   (:ioerr-delete             :ioerr       10  "")
   (:ioerr-blocked            :ioerr       11  "")
   (:ioerr-nomem              :ioerr       12  "")
   (:ioerr-access             :ioerr       13  "")
   (:ioerr-checkreservedlock  :ioerr       14  "")
   (:ioerr-lock               :ioerr       15  "")
   (:ioerr-close              :ioerr       16  "")
   (:ioerr-dir-close          :ioerr       17  "")
   (:ioerr-shmopen            :ioerr       18  "")
   (:ioerr-shmsize            :ioerr       19  "")
   (:ioerr-shmlock            :ioerr       20  "")
   (:ioerr-shmmap             :ioerr       21  "")
   (:ioerr-seek               :ioerr       22  "")
   (:ioerr-delete-noent       :ioerr       23  "")
   (:ioerr-mmap               :ioerr       24  "")
   (:ioerr-gettemppath        :ioerr       25  "")
   (:ioerr-convpath           :ioerr       26  "")
   (:ioerr-vnode              :ioerr       27  "")
   (:ioerr-auth               :ioerr       28  "")
   (:ioerr-begin-atomic       :ioerr       29  "")
   (:ioerr-commit-atomic      :ioerr       30  "")
   (:ioerr-rollback-atomic    :ioerr       31  "")
   (:ioerr-data               :ioerr       32  "")
   (:ioerr-corruptfs          :ioerr       33  "")
   (:locked-sharedcache       :locked       1  "")
   (:locked-vtab              :locked       2  "")
   (:busy-recovery            :busy         1  "")
   (:busy-snapshot            :busy         2  "")
   (:busy-timeout             :busy         3  "")
   (:cantopen-notempdir       :cantopen     1  "")
   (:cantopen-isdir           :cantopen     2  "")
   (:cantopen-fullpath        :cantopen     3  "")
   (:cantopen-convpath        :cantopen     4  "")
   (:cantopen-dirtywal        :cantopen     5  "")
   (:cantopen-symlink         :cantopen     6  "")
   (:corrupt-vtab             :corrupt      1  "")
   (:corrupt-sequence         :corrupt      2  "")
   (:corrupt-index            :corrupt      3  "")
   (:readonly-recovery        :readonly     1  "")
   (:readonly-cantlock        :readonly     2  "")
   (:readonly-rollback        :readonly     3  "")
   (:readonly-dbmoved         :readonly     4  "")
   (:readonly-cantinit        :readonly     5  "")
   (:readonly-directory       :readonly     6  "")
   (:abort-rollback           :abort        2  "")
   (:constraint-check         :constraint   1  "")
   (:constraint-commithook    :constraint   2  "")
   (:constraint-foreignkey    :constraint   3  "")
   (:constraint-function      :constraint   4  "")
   (:constraint-notnull       :constraint   5  "")
   (:constraint-primarykey    :constraint   6  "")
   (:constraint-trigger       :constraint   7  "")
   (:constraint-unique        :constraint   8  "")
   (:constraint-vtab          :constraint   9  "")
   (:constraint-rowid         :constraint  10  "")
   (:constraint-pinned        :constraint  11  "")
   (:constraint-datatype      :constraint  12  "")
   (:notice-recover-wal       :notice       1  "")
   (:notice-recover-rollback  :notice       2  "")
   (:warning-autoindex        :warning      1  "")
   (:auth-user                :auth         1  "")
   (:ok-load-permanently      :ok           1  "")
   (:ok-symlink               :ok           2  "")))


(defcfun sqlite3-libversion :string)

(defcfun sqlite3-sourceid :string)

(defcfun sqlite3-libversion-number :int)


;;; sqlite3 - Database Connection Handle

(defcstruct sqlite3)

(defctype *sqlite3 (:pointer (:struct sqlite3)))

(defcfun sqlite3-busy-timeout result-code
  (db *sqlite3)
  (ms :int))

(defcfun sqlite3-changes :int
  (db *sqlite3))

(defcfun sqlite3-changes64 :int64
  (db *sqlite3))


;; Constructors: sqlite3_open(), sqlite3_open16(), sqlite3_open_v2()
;; Destructors: sqlite3_close(), sqlite3_close_v2()
;; Methods:
;;  - TODO sqlite3_autovacuum_pages
;;  - TODO sqlite3_blob_open
;;  - TODO sqlite3_busy_handler
;;  - TODO sqlite3_busy_timeout
;;  - TODO sqlite3_changes
;;  - TODO sqlite3_changes64
;;  - TODO sqlite3_collation_needed
;;  - TODO sqlite3_collation_needed16
;;  - TODO sqlite3_commit_hook
;;  - TODO sqlite3_create_collation
;;  - TODO sqlite3_create_collation16
;;  - TODO sqlite3_create_collation_v2
;;  - TODO sqlite3_create_function
;;  - TODO sqlite3_create_function16
;;  - TODO sqlite3_create_function_v2
;;  - TODO sqlite3_create_module
;;  - TODO sqlite3_create_module_v2
;;  - TODO sqlite3_create_window_function
;;  - TODO sqlite3_db_cacheflush
;;  - TODO sqlite3_db_config
;;  - TODO sqlite3_db_filename
;;  - TODO sqlite3_db_mutex
;;  - TODO sqlite3_db_readonly
;;  - TODO sqlite3_db_release_memory
;;  - TODO sqlite3_db_status
;;  - TODO sqlite3_drop_modules
;;  - TODO sqlite3_enable_load_extension
;;  - TODO sqlite3_errcode
;;  - TODO sqlite3_errmsg
;;  - TODO sqlite3_errmsg16
;;  - TODO sqlite3_error_offset
;;  - TODO sqlite3_errstr
;;  - TODO sqlite3_exec
;;  - TODO sqlite3_extended_errcode
;;  - TODO sqlite3_extended_result_codes
;;  - TODO sqlite3_file_control
;;  - TODO sqlite3_free_table
;;  - TODO sqlite3_get_autocommit
;;  - TODO sqlite3_get_table
;;  - TODO sqlite3_interrupt
;;  - TODO sqlite3_last_insert_rowid
;;  - TODO sqlite3_limit
;;  - TODO sqlite3_load_extension
;;  - TODO sqlite3_next_stmt
;;  - TODO sqlite3_overload_function
;;  - TODO sqlite3_prepare
;;  - TODO sqlite3_prepare16
;;  - TODO sqlite3_prepare16_v2
;;  - TODO sqlite3_prepare16_v3
;;  - TODO sqlite3_prepare_v2
;;  - TODO sqlite3_prepare_v3
;;  - TODO sqlite3_preupdate_blobwrite
;;  - TODO sqlite3_preupdate_count
;;  - TODO sqlite3_preupdate_depth
;;  - TODO sqlite3_preupdate_hook
;;  - TODO sqlite3_preupdate_new
;;  - TODO sqlite3_preupdate_old
;;  - TODO sqlite3_profile
;;  - TODO sqlite3_progress_handler
;;  - TODO sqlite3_rollback_hook
;;  - TODO sqlite3_set_authorizer
;;  - TODO sqlite3_set_last_insert_rowid
;;  - TODO sqlite3_system_errno
;;  - TODO sqlite3_table_column_metadata
;;  - TODO sqlite3_total_changes
;;  - TODO sqlite3_total_changes64
;;  - TODO sqlite3_trace
;;  - TODO sqlite3_trace_v2
;;  - TODO sqlite3_txn_state
;;  - TODO sqlite3_unlock_notify
;;  - TODO sqlite3_update_hook
;;  - TODO sqlite3_wal_autocheckpoint
;;  - TODO sqlite3_wal_checkpoint
;;  - TODO sqlite3_wal_checkpoint_v2
;;  - TODO sqlite3_wal_hook


;;; sqlite3_stmt - Prepared Statement Object

(defcstruct sqlite3-stmt)
(defctype *sqlite3-stmt (:pointer (:struct sqlite3-stmt)))

(defcfun sqlite3-prepare-v2 result-code
  (db *sqlite3)
  (sql :string)
  (bytes :int)
  (handle-ptr (:pointer *sqlite3-stmt))
  (tail :pointer))

;; Constructors: sqlite3_open(), sqlite3_open16(), sqlite3_open_v2()
;; Destructors:  sqlite3_close(), sqlite3_close_v2()
;; Methods:
;;  - TODO sqlite3_autovacuum_pages
;;  - TODO sqlite3_blob_open
;;  - TODO sqlite3_busy_handler
;;  - TODO sqlite3_changes
;;  - TODO sqlite3_changes64
;;  - TODO sqlite3_collation_needed
;;  - TODO sqlite3_collation_needed16
;;  - TODO sqlite3_commit_hook
;;  - TODO sqlite3_create_collation
;;  - TODO sqlite3_create_collation16
;;  - TODO sqlite3_create_collation_v2
;;  - TODO sqlite3_create_function
;;  - TODO sqlite3_create_function16
;;  - TODO sqlite3_create_function_v2
;;  - TODO sqlite3_create_module
;;  - TODO sqlite3_create_module_v2
;;  - TODO sqlite3_create_window_function
;;  - TODO sqlite3_db_cacheflush
;;  - TODO sqlite3_db_config
;;  - TODO sqlite3_db_filename
;;  - TODO sqlite3_db_mutex
;;  - TODO sqlite3_db_readonly
;;  - TODO sqlite3_db_release_memory
;;  - TODO sqlite3_db_status
;;  - TODO sqlite3_drop_modules
;;  - TODO sqlite3_enable_load_extension
;;  - TODO sqlite3_errcode
;;  - TODO sqlite3_errmsg
;;  - TODO sqlite3_errmsg16
;;  - TODO sqlite3_error_offset
;;  - TODO sqlite3_errstr
;;  - TODO sqlite3_exec
;;  - TODO sqlite3_extended_errcode
;;  - TODO sqlite3_extended_result_codes
;;  - TODO sqlite3_file_control
;;  - TODO sqlite3_free_table
;;  - TODO sqlite3_get_autocommit
;;  - TODO sqlite3_get_table
;;  - TODO sqlite3_interrupt
;;  - TODO sqlite3_last_insert_rowid
;;  - TODO sqlite3_limit
;;  - TODO sqlite3_load_extension
;;  - TODO sqlite3_next_stmt
;;  - TODO sqlite3_overload_function
;;  - TODO sqlite3_prepare
;;  - TODO sqlite3_prepare16
;;  - TODO sqlite3_prepare16_v2
;;  - TODO sqlite3_prepare16_v3
;;  - TODO sqlite3_prepare_v2
;;  - TODO sqlite3_prepare_v3
;;  - TODO sqlite3_preupdate_blobwrite
;;  - TODO sqlite3_preupdate_count
;;  - TODO sqlite3_preupdate_depth
;;  - TODO sqlite3_preupdate_hook
;;  - TODO sqlite3_preupdate_new
;;  - TODO sqlite3_preupdate_old
;;  - TODO sqlite3_profile
;;  - TODO sqlite3_progress_handler
;;  - TODO sqlite3_rollback_hook
;;  - TODO sqlite3_set_authorizer
;;  - TODO sqlite3_set_last_insert_rowid
;;  - TODO sqlite3_system_errno
;;  - TODO sqlite3_table_column_metadata
;;  - TODO sqlite3_total_changes
;;  - TODO sqlite3_total_changes64
;;  - TODO sqlite3_trace
;;  - TODO sqlite3_trace_v2
;;  - TODO sqlite3_txn_state
;;  - TODO sqlite3_unlock_notify
;;  - TODO sqlite3_update_hook
;;  - TODO sqlite3_wal_autocheckpoint
;;  - TODO sqlite3_wal_checkpoint
;;  - TODO sqlite3_wal_checkpoint_v2
;;  - TODO sqlite3_wal_hook


;;; TODO sqlite3_open()

(defbitfield open-flags
  (:readonly         #x00000001)  ;  ok for sqlite3_open_v2()
  (:readwrite        #x00000002)  ;  ok for sqlite3_open_v2()
  (:create           #x00000004)  ;  ok for sqlite3_open_v2()
  (:deleteonclose    #x00000008)  ;  vfs only
  (:exclusive        #x00000010)  ;  vfs only
  (:autoproxy        #x00000020)  ;  vfs only
  (:uri              #x00000040)  ;  ok for sqlite3_open_v2()
  (:memory           #x00000080)  ;  ok for sqlite3_open_v2()
  (:main_db          #x00000100)  ;  vfs only
  (:temp_db          #x00000200)  ;  vfs only
  (:transient_db     #x00000400)  ;  vfs only
  (:main_journal     #x00000800)  ;  vfs only
  (:temp_journal     #x00001000)  ;  vfs only
  (:subjournal       #x00002000)  ;  vfs only
  (:super_journal    #x00004000)  ;  vfs only
  (:nomutex          #x00008000)  ;  ok for sqlite3_open_v2()
  (:fullmutex        #x00010000)  ;  ok for sqlite3_open_v2()
  (:sharedcache      #x00020000)  ;  ok for sqlite3_open_v2()
  (:privatecache     #x00040000)  ;  ok for sqlite3_open_v2()
  (:wal              #x00080000)  ;  vfs only
  (:nofollow         #x01000000)  ;  ok for sqlite3_open_v2()
  (:exrescode        #x02000000)) ;  extended result codes


(defcfun sqlite3-open result-code
  (filename :string)
  (db (:pointer *sqlite3)))


(defcfun sqlite3-open-v2 result-code
  (filename :string)
  (db (:pointer *sqlite3))
  (flags open-flags)
  (vfs :string))


(defcfun sqlite3-errcode :int
  (db *sqlite3))


(defcfun sqlite3-extended-errcode :int
  (db *sqlite3))


(defcfun sqlite3-errmsg :string
  (db *sqlite3))


(defcfun sqlite3-errstr :string
  (code :int))


;;; TODO sqlite3_prepare()


;;; TODO sqlite3_bind()


;;; TODO sqlite3_step()


(defcfun sqlite3-step result-code
  (stmt *sqlite3-stmt))


;;; TODO sqlite3_column()


#+nil(define-constants/cenum datatype
       (:integer 1 "integer")
       (:float 2 "float")
       (:text 3 "text")
       (:blob 4 "blob")
       (:null 5 "null"))



(defun code->keyword (code)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (integer 1 5) code))
  (aref #(:integer :double :text :blob :null) (1- code)))


(defun keyword->code (keyword)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (ecase keyword
    (:integer 1)
    (:float 2)
    (:text 3)
    (:blob 4)
    (:null 5)))


(define-foreign-type datatype ()
  ()
  (:actual-type :int)
  (:simple-parser datatype))


(defmethod expand-to-foreign (value (type datatype))
  `(keyword->code ,value))

(defmethod expand-from-foreign (value (type datatype))
  `(code->keyword ,value))


(defcfun sqlite3-column-type :int
  (stmt *sqlite3-stmt)
  (i-column :int))

(defcfun sqlite3-column-text :string
  (stmt *sqlite3-stmt)
  (i-column :int))

(defcfun sqlite3-column-int :int
  (stmt *sqlite3-stmt)
  (i-column :int))

(defcfun sqlite3-column-int64 :int64
  (stmt *sqlite3-stmt)
  (i-column :int))

(defcfun sqlite3-column-double :double
  (stmt *sqlite3-stmt)
  (i-column :int))


(declaim (inline %statement-value))
(defun %statement-value (stmt i)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (ecase (sqlite3-column-type stmt i)
    (1 (sqlite3-column-int stmt i))
    (2 (sqlite3-column-double stmt i))
    (3 (sqlite3-column-text stmt i))
    (4 nil)
    (5 nil)))


(defmacro statement-value (stmt i &optional (cast nil cast-p))
  (if cast-p
      (ecase cast
        (:integer `(sqlite3-column-int ,stmt ,i))
        (:double `(sqlite3-column-double ,stmt ,i))
        (:text `(sqlite3-column-text ,stmt ,i))
        (:blob nil)
        (:null nil))
      `(%statement-value ,stmt ,i)))


;; - TODO sqlite3_column_blob
;; - TODO sqlite3_column_double
;; - TODO sqlite3_column_int
;; - TODO sqlite3_column_int64
;; - TODO sqlite3_column_text
;; - TODO sqlite3_column_text16
;; - TODO sqlite3_column_value
;; - TODO sqlite3_column_bytes
;; - TODO sqlite3_column_bytes16
;; - TODO sqlite3_column_type

;;; TODO sqlite3_finalize()


(defcfun sqlite3-finalize result-code
  (stmt *sqlite3-stmt))

;;; TODO sqlite3_close()

(defcfun sqlite3-close result-code
  (db *sqlite3))


;;; TODO sqlite3_exec()


(defcstruct sqlite3-vfs
  (version :int)
  (of-file-size :int)
  (max-pathname-length :int)
  (next (:pointer (:struct sqlite3-vfs)))
  (name :string)
  (app-data :pointer))

(defctype *sqlite3-vfs (:pointer (:struct sqlite3-vfs)))

(defcfun sqlite3-vfs-find *sqlite3-vfs
  (name :string))


