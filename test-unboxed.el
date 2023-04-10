
(setq load-path (cons default-directory load-path))
(load "unboxed")

(setq tmp (unboxed--make-dbs-from-settings unboxed-areas))

(setq tmp-user-db (cdr (assq 'user tmp)))

(setq tmp2 (unboxed--unbox-package-list-in-db tmp-user-db '(async)))

(pp tmp2 (current-buffer))


(pp tmp-user-db (current-buffer))



