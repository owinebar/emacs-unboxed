;;; unboxed-rewrite-sexprs.el   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Onnie Winebarger

;; Author: Onnie Winebarger
;; Copyright (C) 2023 by Onnie Lynn Winebarger <owinebar@gmail.com>
;; Keywords: extensions, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code supports rewriting s-expressions in source with minimal disturbance
;; of surrounding text.

;;; Code:

;(require 'unboxed-custom)

(define-error 'unboxed-replace-sexpr "Bad sexpr parse")
(define-error 'unboxed-unrecognized-read-syntax
  "Unrecognized read syntax")


(defun unboxed--next-sexpr-start ()
  "Find the start of the *next* sexpr, which may begin at point but no
earlier"
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point-max))
      (forward-sexp)
      (backward-sexp)
      (point))))

(defun unboxed--this-sexpr-start (&optional min-start)
  "Find the start of the sexpr which point follows or is in"
  (save-excursion
    (save-restriction
      (when min-start
	(narrow-to-region min-start (point-max)))
      (condition-case nil
	  (forward-sexp -1)
	(error (goto-char min-start)))
      (point))))

;; assumes parse-sexp-ignore-comments is t
(defun unboxed--back-sexpr (v p0 &optional p2)
  (unless p2
    (setq p2 (point)))
  (let (p1)
    ;; check for named unicode character that is not handled well by
    ;; emacs-lisp-mode (forward-sexp -1) 
    (when (and (char-or-string-p v)
	       (not (stringp v))
	       (eq (preceding-char) ?\}))
      ;; assume a unicode character name cannot contain a comment
      (save-excursion
	(setq p1 (search-backward "{" p0 t))
	(when p1
	  ;; this is only valid if it occured as a named unicode character
	  (setq p1 nil)
	  (and (looking-back "\\?\\\\N")
	       (forward-char -3)
	       (setq p1 (point))))))
    ;; (forward-sexp -1) also does not deal well with ## (the empty symbol)
    (unless (or p1 (not (and (symbolp v) (eq (preceding-char) ?\#))))
      (forward-char -1)
      (and (> (point) p0)
	   (eq (preceding-char) ?\#)
	   (setq p1 (unboxed--this-sexpr-start p0))))
    ;; #N# references are dealt with directly when read error is detected
    ;; assume other cases fairly well-behaved
    (unless p1
      (goto-char (unboxed--this-sexpr-start p0))
      (cond
       ((recordp v)
	(when (and (>= (point) (+ p0 2))
		   (eq (preceding-char) ?s)
		   (eq (char-before (- (point) 1)) ?\#))
	  (forward-char -2)))
       (t nil))
      (setq p1 (point)))
    (goto-char p1)
    p1))
       
(defun unboxed--replace-text-in-region (start end new)
  "Utility function for replacing region with specified SEXP.
If point is inside the region, it will be at the end of the region
following the replacement" 
  (if (and (< (point) end) (> (point start)))
      (goto-char end))
  (save-excursion
    (goto-char end)
    (delete-region start end)
    (prin1 new (current-buffer))))

;; assumes preceding character is \#
(defun unboxed--check-invalid-read-graph-occurence (pos0)
  "Utility function for checking if reader failed due to encountering bare #N#"
  (let ((p2 (point))
	p0 p1 N retval)
    (save-excursion
      (forward-char -1)
      (setq p1 (point))
      (search-backward "#" pos0 t)
      (when (< (point) p1)
	(setq p0 (point))
	(forward-char 1)
	(ignore-error
	    (setq N (read (current-buffer))))
	(when (and N (natnump N) (>= (point) p1))
	  (setq retval p0))))
    retval))

;; assumes preceding character is either \) or \]
(defun unboxed--check-invalid-non-atomic (pos0)
  "Utility function for checking if reader failed due to encountering a non-atomic
sexpr containing a #N#."
  (let ((p2 (point))
	(close (preceding-char))
	open p1)
    (setq open
	  (if (eq close ?\])
	      "["
	    "("))
    (save-excursion
      (setq p1 (unboxed--back-sexpr nil pos0))
      ;; not infallibly correct, but good enough for non-pathological cases
      (unless (search-forward open p2 t)
	(setq p1 nil))
      p1)))

(defun unboxed--check-invalid-read (pos0)
  "Utility function to check for whether an invalid-read-syntax
error corresponded to a subexpression that should produce an
object, and dispatching accordingly, or something like `.' that is
purely syntactic"
  (let (p1)
    (cond
     ((eq (preceding-char) ?\#) ; check for #N#
      (setq p1 (unboxed--check-invalid-read-graph-occurence pos0)))
     ((or (eq (preceding-char) ?\))
	  (eq (preceding-char) ?\])) ; check for non-atomic
      (setq p1 (unboxed--check-invalid-non-atomic pos0))))
    p1))

(defun unboxed--symbol-read-syntax (sym)
  (pcase sym
    ('quote "'")
    ('function "#'")
    ('\` "`")
    ('\, ",")
    ('\,@ ",@")
    (_ nil)))
     
;; this handles situations where the reader returns a list but
;; the character at point is not a left parenthesis
(defun unboxed--read-syntax-end (pos1 sym)
  "Determine the length of the read syntax for the symbol at point"
  (let ((s (unboxed--symbol-read-syntax sym))
	p)
    (when s
      (setq p (+ pos1 (length s)))
      (unless (string= (buffer-substring-no-properties pos1 p) s)
	;; there can be whitespace between the special symbol and the
	;; rest of the sexp, in which case (forward-sexp -1) misses
	;; the special read syntax
	(setq p pos1)))
    (unless p
      (signal 'unboxed-unrecognized-read-syntax
	      `(,(current-buffer) ,pos1 ,sym)))
    p))

;;  unboxed--pcase-replace-next-sexpr calls read from point to skip
;;  any comments and get the value represented by the text for
;;  testing. 
;;  It then attempts to identify the beginning of the textual
;;  representation using unboxed--back-sexpr before testing for
;;  replacement.
;;  We assume the (possibly narrowed) current buffer contains a valid
;;  elisp program. Given that, read will still signal an error in two
;;  cases: 
;;  1) When the EOF is reached - read has no other mechanism for
;;     indicating all expressions have been read
;;  2) Due to syntax that is valid as a subexpression of some sexpr,
;;     but not as a stand-alone expression.  
;;     a) Graph notation #N# represents an *occurrence* of an object
;;        represented elsewhere.  Hence we treat the traversal of this
;;        notation as a successful reading of the corresponding
;;        expression, although this prevents pcase testing from being
;;        performed on any containing sexpr 
;;     b) Graph notation #N= defining a graph object.  In this case we
;;        attempt to read the following object since the #N= does not
;;        itself correspond to any object occurrence. 
;;     c) The dot in a dotted pair representation of a cons cell. In
;;        this case we attempt to read the following sexpr as the dot
;;        does not correspond to any constructed object.
;;     d) Any non-atomic object failing due to containing an undefined
;;        #N# notation, though such a definition must have occurred if
;;        the top-level expression was successfully read.
;;  Note the lack of recording graph notation values means that pcase
;;  testing is not truly done *inside* circular objects in the elisp
;;  source code. 
;;
;;  In this implementation, there are 3 main positions of concern:
;;  position 0 - the initial value of point before calling read
;;  position 2 - the position immediately following the last character
;;               of an expression
;;  position 1 - the position immediately preceding the first
;;               character of the text representing the value which is
;;               going to be tested and possibly replaced 
;;
;;  Every position 0 is either the beginning of the buffer or position
;;  2 of some processed value.

;;  FIXME - deal with quote, unquote and unquote-append syntaxes
;;          correctly, since they produce lists of length two
;;          Current implementation jumps into cadar of that pair
;;          but traverses that value assuming it is a sequence of
;;          length two.
;;          *** Should be fixed
(defun unboxed--pcase-replace-next-sexpr (sexpr-pred)
  "Perform matching/replacement in the region containing the first
  sexpr found in the current buffer following the point in the current
  buffer. The search will recurse if the sexpr is a list or vector."
  (let ((pos0 (point))
	(read-attempts 0)
	eof pos1 pos2 v m)
    (while (and (not pos2) (not eof) (< read-attempts 2))
      (condition-case nil
	  ;; this will error if there are no additional expressions found
	  (setq v (read (current-buffer))
		pos2 (point))
	(invalid-read-syntax
	 (let ((p1 (unboxed--check-invalid-read pos0)))
	   (when p1
	     (setq pos1 p1
		   pos2 (point)))))
	(error (setq eof t)))
      (cl-incf read-attempts))
    (when (and pos2 (not pos1))
      ;; could push this into the cond clause for a successful
      ;; match *if* matcher was guaranteed to have no side-effects
      (setq pos1 (unboxed--back-sexpr v pos0 pos2)
            ;; m (unboxed--pcase-replace-matching-sexpr v))
	    m (funcall sexpr-pred v))
      (goto-char pos2)
      (cond
       (m (unboxed--replace-text-in-region pos1 pos2 (car m)))
       ((atom v) nil)
       ((and (not (member (char-after pos1) '(?\( ?\[)))
	     (consp v)
	     (consp (cdr v))
	     (null (cddr v))
	     (symbolp (car  v)))
	(save-excursion
	  (goto-char (unboxed--read-syntax-end pos1 (car v)))
	  (unboxed--pcase-replace-next-sexpr sexpr-pred)))
       ((or (recordp v) (arrayp v))
	(save-excursion
	  (goto-char (scan-lists pos1 1 -1))
	  (unboxed--pcase-replace-in-seq sexpr-pred 0 (length v))
	  (when (<= (point) pos1)
	    (signal 'unboxed-replace-sexpr `[,v ,pos0 ,pos1 ,pos2 ,(point)]))))
       ((consp v)  
	(save-excursion
	  (goto-char (scan-lists pos1 1 -1))
	  (let ((ls v))
	    (while (consp ls)
	      (unboxed--pcase-replace-next-sexpr sexpr-pred)
	      (pop ls))
	    (when ls
	      (unboxed--pcase-replace-next-sexpr sexpr-pred)))
	  (when (<= (point) pos1)
	    (signal 'unboxed-replace-sexpr `[,v ,pos0 ,pos1 ,pos2 ,(point)]))))))
    (not eof)))

(defun unboxed--pcase-replace-in-seq (sexpr-pred i n)
  (while (< i n)
    (unboxed--pcase-replace-next-sexpr sexpr-pred)
    (cl-incf i)))

;; search text in current buffer for sexprs matching one of a supplied set of pcase patterns
;; if a match is found, replace the text with corresponding sexpr value
;; Replacement should not disturb relative position of the text surrounding the text
;; that produced the sexpr matching the supplied pcase pattern
(defun unboxed--pcase-replace-sexpr (sexpr-pred)
  "Simple search and replace on sexprs to match common expressions
   used in defining data directory variables for packages."
  (emacs-lisp-mode)
  (let ((parse-sexp-ignore-comments t))
    (save-excursion
      (goto-char (point-min))
      (while (unboxed--pcase-replace-next-sexpr sexpr-pred)))))


(provide 'unboxed-rewrite-sexprs)

;;; unboxed-rewrite-sexprs.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ajq-" . "async-job-queue-")("ub-" . "unboxed-"))
;; End:
;; 

