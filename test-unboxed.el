
(setq load-path (cons default-directory load-path))
(load "unboxed")
(custom-set-variables `(unboxed-user-area-pred ,#'unboxed-package-any-p))
(custom-set-variables `(unboxed-site-area-pred ,#'unboxed-package-any-p))


(setq tmp (unboxed--make-dbs-from-settings unboxed-areas))

(setq tmp-user-db (cdr (assq 'user tmp)))

(setq tmp2 (unboxed--unbox-package-list-in-db tmp-user-db '(async)))

(setq tmp-txt
      (let* ((al-fn (expand-file-name "~/.emacs.d/unboxed-autoloads.el"))
	     (txt (autoload-rubric al-fn nil 'unboxed-autoloads)))
	(message "Text\n%s" txt)
	(setq txt (replace-regexp-in-string
		   "[[:space:]]*no-byte-compile:[[:space:]]+t[[:space:]]*"
		   ""
		   txt))
	(message "Text\n%S" txt)
	txt))

(unboxed--ensure-autoloads-file (expand-file-name "~/.emacs.d/unboxed-autoloads.el"))
(unboxed--ensure-autoloads-file (expand-file-name "~/.emacs.d/lisp/unboxed-autoloads.el"))
(make-directory-autoloads "~/.emacs.d/lisp" "~/.emacs.d/lisp/unboxed-autoloads.el")

(pp tmp2 (current-buffer))

(let ((default-directory (file-name-concat user-emacs-directory "lisp/")))
  (setq bc-result (unboxed--async-byte-compile-file "async.el")))


(pp tmp-user-db (current-buffer))


(unboxed-library-p "async.el")
(unboxed-library-p 'async.el)
(unboxed-library-p "async-pkg.el")
(unboxed-library-p "async-autoloads.el")
(unboxed-library-p "async-autoload.el")
(unboxed-data-library-p "async.el")
(unboxed-library-p "foo/async.el")
(unboxed-data-library-p "foo/async.el")

(pp bc-result (current-buffer))
(("/home/owinebar/.emacs.d/lisp/async.elc" "/home/owinebar/.emacs.d/tmp/compile-log--async-lMM1WV" ""))


