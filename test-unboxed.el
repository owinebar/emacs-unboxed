
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

(unboxed--file-grep "load-file-name" (expand-file-name "test/files/should-rewrite.el"))
(unboxed--file-grep "load-file-name" (expand-file-name "test/files/should-not-rewrite.el"))


(defun sexpr-start ()
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point-max))
      (forward-sexp)
      (backward-sexp)
      (point))))

''foo					
'(foo)
,@foo
,foo
\,

'(quote \' \` \, \,@)

(pp (symbol-function 'unboxed--pcase-replace-sexpr-p) (current-buffer))

(pp (symbol-plist 'unboxed-user-data-directory-patterns) (current-buffer))

(makunbound 'unboxed-user-data-directory-patterns)
(pp unboxed-user-data-directory-patterns (current-buffer))

(unboxed--sexpr-rewriting-copy
 (expand-file-name "test/files/should-rewrite.el")
 (expand-file-name "test/files/should-rewrite-rewritten.el")
 (get 'unboxed-user-data-directory-patterns 'unboxed-rewriter))

(unboxed--sexpr-rewriting-copy
 (expand-file-name "test/files/should-not-rewrite.el")
 (expand-file-name "test/files/should-not-rewrite-rewritten.el")
 (get 'unboxed-user-data-directory-patterns 'unboxed-rewriter))

(unboxed--sexpr-rewriting-copy
 (expand-file-name "test/files/should-rewrite2.el")
 (expand-file-name "test/files/should-rewrite2-rewritten.el")
 (get 'unboxed-user-data-directory-patterns 'unboxed-rewriter))

(unboxed--sexpr-rewriting-copy
 (expand-file-name "test/files/should-rewrite3.el")
 (expand-file-name "test/files/should-rewrite3-rewritten.el")
 (get 'unboxed-user-data-directory-patterns 'unboxed-rewriter))

(pp tmp-user-db (current-buffer))


(pp package-alist (current-buffer))



