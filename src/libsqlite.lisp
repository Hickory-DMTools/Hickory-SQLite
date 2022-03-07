(defpackage :%sqlite
  (:use :cl :cffi)
  (:export #:sqlite3
           #:*sqlite3
           #:sqlite3-vfs
           #:sqlite3-open-v2
           #:sqlite3-close
           #:sqlite3-vfs-find
           #:name))

(in-package :%sqlite)


(in-package :sqlite.ffi)


(defmacro define-constants/cenum (name &body codes)
  `(progn
     #+nil ;; is it valuable to generate constants as well?
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



(defcstruct sqlite3)
(defctype *sqlite3 (:pointer (:struct sqlite3)))


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


(defcfun sqlite3-libversion :string)

(defcfun sqlite3-sourceid :string)

(defcfun sqlite3-libversion-number :int)


(defcfun sqlite3-open-v2 result-code
  (filename :string)
  (db (:pointer *sqlite3))
  (flags open-flags)
  (vfs :string))


(defcfun sqlite3-close result-code
  (db *sqlite3))


(defcfun sqlite3-busy-timeout result-code
  (db *sqlite3)
  (ms :int))


(defcstruct sqlite3-stmt)
(defctype *sqlite3-stmt (:pointer (:struct sqlite3-stmt)))


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


