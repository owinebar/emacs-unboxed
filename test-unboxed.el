
(setq load-path (add-to-list 'load-path default-directory))
(load "unboxed")
(custom-set-variables `(unboxed-user-area-pred ,#'unboxed-package-any-p))
(custom-set-variables `(unboxed-site-area-pred ,#'unboxed-package-any-p))

(require 'package)

(package-load-all-descriptors)
(when (null package-activated-list)
  (package-initialize))
(setq package-activated-list (delete-dups package-activated-list))

(setq tmp (unboxed--make-dbs-from-settings unboxed-areas))
q
(setq tmp-user-db (cdr (assq 'user tmp)))
(setq tmp-user-active (unboxed--sexpr-db-active tmp-user-db))
(setq tmp-user-available-pkgs (unboxed--sexpr-db-available tmp-user-db))
(setq tmp-user-active-pkgs (unboxed--db-state-packages tmp-user-active))
(pp (unboxed--summarize-db-packages tmp-user-active-pkgs))
(pp (unboxed--summarize-db-packages tmp-user-available-pkgs))

(pp (unboxed--summarize-package-desc
     (unboxed--get-package-from-db-packages-by-name active-pkgs
						    p)

 (pp (unboxed--summarize-sexpr-db tmp-user-db))
 
(setq tmp-user-db2 (unboxed--catalog-packages-in-list tmp-user-db '(ztree async)))
(setq tmp-user-db2 (unboxed--catalog-packages-in-list tmp-user-db))

(setq tmp-pds (unboxed--get-db-active-package-descriptors tmp-user-db '(ztree)))
(setq tmp-pds (unboxed--get-db-active-package-descriptors tmp-user-db '(ac-c-headers)))

(pp (queue-all (queue-map #'unboxed--summarize-package-desc tmp-pds)))

(setq tmp2 (unboxed--unbox-package-list-in-db tmp-user-db '(async)))
(setq tmp2 (unboxed--unbox-package-list-in-db tmp-user-db '(yasnippet yasnippet-classic-snippets)))
(setq tmp2 (unboxed--unbox-package-list-in-db tmp-user-db '(company-coq)))
(setq tmp3 (unboxed--unbox-package-list-in-db tmp-user-db (mapcar #'car package-alist)))

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

(pp tmp-user-db (current-buffer))
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

(unboxed--file-grep "load-file-name" (expand-file-name "test/files/should-rewrite.el"))
(unboxed--file-grep "load-file-name" (expand-file-name "test/files/should-not-rewrite.el"))
(unboxed--file-grep "load-file-name" (expand-file-name "~/.emacs.d/lisp/sqlite3.el"))
(unboxed--sexpr-rewriting-copy
 (expand-file-name "~/.emacs.d/lisp/sqlite3.el")
 (expand-file-name "test/files/sqlite3-rewritten.el")
 (get 'unboxed-user-data-directory-patterns 'unboxed-rewriter))
(unboxed--sexpr-rewriting-copy
 (expand-file-name "~/.emacs.d/lisp/auto-complete-rst.el")
 (expand-file-name "test/files/auto-complete-rst-rewritten.el")
 (get 'unboxed-user-data-directory-patterns 'unboxed-rewriter))


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

(unboxed--sexpr-rewriting-copy
 (expand-file-name "/home/owinebar/.config/emacs/elpa-28/yasnippet-20200604.246/yasnippet.el")
 (expand-file-name "test/files/yasnippet-rewritten.el")
 (get 'unboxed-user-data-directory-patterns 'unboxed-rewriter))

(pp tmp-user-db (current-buffer))

(setq yasnippet-pd (cadr (assq 'yasnippet package-alist)))


;(require 'package)
;(package-initialize)
(pp package-alist (current-buffer))



