;;; icg.el --- Interface constrained generics   -*- lexical-binding: t; -*-

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

;; Support for specifying generic code based on interface constraints
;; Flavor of genericity similar to that found in .NET languages
;;
;; Generic methods in three stages
;; First, declare generic with interface symbols associated to parameters
;; Second, specialize the generic by declaring signatures of required methods
;;    associated with each interface and abstract slot names associated to each
;;    parameter (which are also part of the interface definition)
;;    Provide the generic body of the method with the method names bound in
;;    the function namespace and the slots bound as variables
;; Third, instantiate the generic by binding the interface methods and
;;    implementation types of the object parameters to the generic parameters
;;    


;;; Code:

(eval-and-compile
  (defun icg--make-type-descriptor (name)
    (record 'icg--type-descriptor name))
  (defun icg--make-interface-type-descriptor (name)
    (record (icg--make-type-descriptor 'icg--interface) name))
  (defun icg--argument-list-marker-p (sym)
    "Test whether SYM is an argument list marker"
    (memq sym '(&optional &rest &aux &key &context)))

  (defun icg--format-doc-variable (sym &optional interface)
    "Format a variable name SYM satisfying INTERFACE for appearance in a docstring."
    (let ((s (symbol-name sym))
	  (p (when interface
	       (symbol-name interface))))
      (when (= (aref s 0) ?_)
	(setq s (substring s 1)))
      (setq s (upcase s))
      (when interface
	(when (= (aref p 0) ?_)
	  (setq p (substring p 1)))
	(setq p (upcase p))
	(setq s (format "%s : %s" s p)))
      s))

  (defun icg--generic-operator-arg-table-line (spec-docstr)
    "Format a single argument table line for SPEC"
    (format "\n  %s" spec-docstr))

  (defun icg--generic-operator-arg-table-docstring (arg-docs)
    (apply #'concat
	   (mapcar #'icg--generic-operator-arg-table-line
		   arg-docs)))

  (defun icg--generic-operator-docstring (op arg-docs)
    "Make a docstring for generic operator OP with argument docstrings ARG-DOCS"
    (let ((arg-table
	   (when args
	     (icg--generic-operator-arg-table-docstring arg-docs))))
      (format "Define generic operator %s.%s"
	      (icg--format-doc-variable op)
	      (if arg-table
		  (concat "\nArguments:" arg-table)
		""))))
  
  (defun icg--accumulate-generic-operator-signature
      (spec docstrings interfaces parameters)
    "Accumulate components of SPECS.
Arguments:
  SPEC - generic operator spec
  DOCSTRINGS - queue of docstrings
  INTERFACES - queue of interfaces
  PARAMETERS - queue of parameters"
    (pcase spec
      (`(,interface ,name (and (cl-type string) doc))
       (queue-enqueue docstrings `(,name . ,doc))
       (queue-enqueue interfaces `(,interface . ,name))
       (queue-enqueue parameters name))
      ((or (pred keywordp)
	   (pred icg--argument-list-marker-p))
       (error "Argument markers not allowed for operators %s" spec))
      (`(,name (and (cl-type string) doc)) 
       (queue-enqueue docstrings `(,name . ,doc))
       (queue-enqueue parameters name))
      (`(,interface ,name)
       (queue-enqueue docstrings
		      `(,name . ,(icg--format-doc-variable name interface)))
       (queue-enqueue interfaces `(,interface . ,name))
       (queue-enqueue parameters name))
      ((cl-type symbol)
       (queue-enqueue docstrings
		      `(,spec . ,(icg--format-doc-variable spec)))
       (queue-enqueue parameters ,spec))
      (_
       (error "Unrecognized operator argument spec %s" spec)))
    nil)

  (defun icg--generic-operator-signature (specs)
    (let ((docstrings (make-queue))
	  (interfaces (make-queue))
	  (parameters (make-queue)))
      (while specs
	(icg--accumulate-generic-operator-signature
	 specs docstrings interfaces parameters))
      `(,(queue-all parameters)
	,(queue-all interfaces)
	,(queue-all docstrings)))))


;;;
;;;  The definition of generic operator and specialization
;;;  support a C#-ish flavor of genericity in which
;;;  interfaces are allowed to specify function signatures
;;;  and objects may specify and bind slots in specialized
;;;  implementations.  The generic operator names the
;;;  object and interface parameters.
;;;  These specialized forms are then instantiated
;;;  to specific cases by binding the elements of the
;;;  specified interfaces and objects.

(eval-and-compile
  (cl-defgeneric icg--expand-specialize-operator (op iface-sigs obj-sigs body)
    "Expand code to specialize OP with ifaces and objs specializers")
  (cl-defgeneric icg--expand-instantiate-operator (op iface-specs obj-specs)
    "Expand code to instantiate OP with ifaces and objs specializers"))

(defun icg--make-expand-specialize-operator (op iface-sigs obj-sigs body)
  (pcase-let ((`(,iface-specializing-args ,sp-op)
	       (icg--process-interface-signatures op ifaces))
	      (`(,obj-specializing-args ,obj-args ,field-args)
	       (icg--process-object-signatures op objs)))
    (let ((sp-op-expander (intern (formatter "icg--expand-%s" sp-op))))
      `(progn
	 (eval-and-compile
	   (cl-defmethod ,opexpander (ifaces objs)
	     `(cl-defmethod ,',sp-op
		(,@obj-args 
		 ,@field-specializing-arguments)
		,(format ,docstr-template ,@instantiating-fields)
		,@',body)))
	 ;; generic to implement for instantiated methods
	 (cl-defmethod ,gen-op
	   (,@iface-specializing-args
	    ,@obj-specializing-args)
	   ,generic-docstring
	   (,sp-op ,@obj-args
		 ,@specializing-field-args))
	 (cl-defgeneric ,sp-op
	     (,@obj-args ,@field-args)
	   ,generic-docstr)
	 (defmacro ,macro-name ,field-instantiation-specs
	   ,macro-docstr
	   `(cl-defmethod ,',specialized-generic
	      (,@op-args ; generic is gen-symed, do not check specialization twice
	       ,@field-specializing-arguments)
	      ,(format ,docstr-template ,@instantiating-fields)
	      ,@',body))))))

(defmacro icg-define-generic-operator (&rest specs)
  "Define  a generic operator OP with argument specs SPECS."
  (pcase-let ((`(,parameters ,interfaces ,docstrings)
	       (icg--generic-operator-signature specs)))
    (put op 'icg-generic-operator-parameters parameters)
    (put op 'icg-generic-operator-interfaces interfaces)
    (put op 'icg-generic-operator-docstrings docstrings)
    (let* ((op (car parameters))
	   (args (cdr parameters))
	   (decls (mapcar #'car interfaces))
	   (arg-docs (mapcar #'cdr docstrings))
	   (spec-decl-args
	    (mapcar (lambda (x) (cl-gensym (format "%s-arg-" x))) decls))
	   (spec-decl-arg-pattern
	    (mapcar (lambda (x) (cons '\, `(,x))) spec-decl-args))
	   (spec-expander (cl-gensym (format "icg--expand-spec-%s" op)))
	   (inst-expander (cl-gensym (format "icg--expand-inst-%s" op))))
      `(progn
	 (cl-defgeneric ,op (,@decls ,@args)
	   ,(icg--generic-operator-docstring op arg-docs))
	 (cl-defgeneric ,inst-expander (,@decls iface-sigs obj-sigs body)
	   "Expand op for given interface names")
	 (cl-defmethod icg--expand-specialize-operator (op iface-sigs obj-sigs body)
	   )
	 (cl-defmethod icdg--expand-instantiate-operator (op iface-specs obj-specs)
	   "Expand code to specialize OP with ifaces and objs specializers"
	   (pcase-let ((`(,',spec-decl-arg-pattern)
			(mapcar #'icg--get-iface-names iface-specs)))
	     (,inst-expander ,@spec-decl-args iface-specs obj-specs)))
	 (defmacro ,spec-expander (iface-sigs obj-sigs &rest body)
	   (pcase-let ((`(,spec-inst-expander
			  ,spec-op
			  ,iface-names
			  ,iface)
			(icg--process-operator-interface-signatures
			 op ,'decls iface-sigs))
		       (`(,obj)
			(icg--process-operator-object-signatures
			 op ,'args obj-sigs)))
	     `(progn
		(defmacro ,spec-inst-expander (iface-specs obj-specs)
		  (pcase-let ((`(,interface-bindings
				 ,slot-bindings
				 ,interface-specializers
				 ,arg-specializers
				 ,field-specializers
				 ,docstring
				 ,body)
			       (icg--process-instantiation-specs
				op specialized-op specs)
			       ))
		    `(cl-defmethod ,',specialized-op
		       (,@interface-specializers
			,@arg-specializers
			,@field-specializers)
		       ,docstring
		       (cl-flet ,interface-bindings
			 (cl-symbol-macrolet	,slot-bindings
			   ,@body))))))
))))))

(defmacro icg-specialize-generic-operator (op iface-sigs obj-sigs &rest body)
  (icg--expand-specialize-generic-operator op iface-sigs obj-sigs body))
(defmacro icg-instantiate-generic-operator (op iface-specs obj-specs)
  (icg--expand-instantiate-generic-operator op iface-specs obj-specs))

;;; (icg-specialize-generic-operator
;;;    op specialized-op
;;;    interface-signatures
;;;    object-signatures
;;;    . body)
;;; interface signature
;;;     (interface-parameter interface-name . method-signatures)
;;;  method signature
;;;     (method-name . argument-list)
;;;  object signature
;;;     (object-parameter object-type-name implementation-type  . slot-signatures)
;;;  slot signature
;;;     (slot-name implementation-type variable)
;;;

(defmacro icg-specialize-generic-operator (gen-op ifaces objs &rest body)
  (pcase-let ((`(,iface-specializing-args ,sp-op)
	       (icg--process-interface-signatures op ifaces))
	      (`(,obj-specializing-args ,obj-args ,field-args)
	       (icg--process-object-signatures op objs)))
    (let ((sp-op-expander (intern (formatter "icg--expand-%s" sp-op))))
      `(progn
	 (eval-and-compile
	   (cl-defmethod ,opexpander (ifaces objs)
	     `(cl-defmethod ,',sp-op
		(,@obj-args 
		 ,@field-specializing-arguments)
		,(format ,docstr-template ,@instantiating-fields)
		,@',body)))
	 ;; generic to implement for instantiated methods
	 (cl-defmethod ,gen-op
	   (,@iface-specializing-args
	    ,@obj-specializing-args)
	   ,generic-docstring
	   (,sp-op ,@obj-args
		 ,@specializing-field-args))
	 (cl-defgeneric ,sp-op
	     (,@obj-args ,@field-args)
	   ,generic-docstr)
	 (defmacro ,macro-name ,field-instantiation-specs
	   ,macro-docstr
	   `(cl-defmethod ,',specialized-generic
	      (,@op-args ; generic is gen-symed, do not check specialization twice
	       ,@field-specializing-arguments)
	      ,(format ,docstr-template ,@instantiating-fields)
	      ,@',body))))))
    w)

;;; (icg-instantiate-specialized-operator
;;;   specialized-op
;;;   interface-bindings
;;;   object-bindings)
;;;
;;;   interface-binding
;;;     (interface-name . method-bindings)
;;;   method-binding
;;;     (method-name function-expression)
;;;   object-binding
;;;     (object-type-name . slot-bindings)
;;;   slot-binding
;;;     (slot-name slot-accessor &optional slot-specializer)
;;; op-spec = (interface name doc)
;;; object-spec = (interface id impl-type impl-var . field-specs)
;;; field-spec = (slot variable &optional constraint)
;;;
;;;   defines generic method
;;;  subj-op ,@op-vars ,@field-vars
;;;  and a method implementing the original op specialized on the subject argument
;;;  (subj-op ,@op-args ,@field args)
;;;
;;;  And a helper macro with arguments filling in the object types and slots
;;;  that defines a method
;;;  specialized
;;;  with the generic body
;;; specialized according to the 


(eval-and-compile
  (defun icg--process-field-spec
      (lambda (obj-id generic-type obj-impl obj-impl-var field-spec)
	(pcase field-spec
	  (`(,slot ,id)
	   (let ((slot-var (cl-gensym (format "%s-var" slot))))
	     ;; list (macro-var method-args bindings
	     `(,macro-var
	       `(,',slot (eql ',,slot-var))
	       `((,',id
		  (,(intern (format "%s-%s"
				    ,obj-impl-var
				    ,macro-var))))))))
	  ((and (cl-type symbol) slot)
	   (let ((slot-var (cl-gensym (format "%s-var" slot))))
	     ;; list (macro-var method-args bindings
	     `(,macro-var
	       `(,',slot (eql ',,slot-var))
	       nil)))
	  (_ (error "Unrecognized object field spec %s" field-spec)))))
  
  (defun icg--process-interface-spec
      (lambda (obj-spec)
	(pcase obj-spec
	  (`(,obj-type-var ,obj-type ,obj-id . ,field-specs)
	   (let ((fields
		  (mapcar
		   (lambda (field-spec)
		     (icg--process-field-spec
		      obj-id generic-type obj-impl obj-impl-var
		      field-spec)))
		   #'icg--process-field-spec field-specs)))
	   `(,slot ,id)
	   (let ((slot-var (cl-gensym (format "%s-var" slot))))
	     ;; list (macro-var method-args bindings
	     `(,macro-var
	       `(,',slot (eql ',,slot-var))
	       `((,',id
		  (,(intern (format "%s-%s"
				    ,obj-impl-var
				    ,macro-var))))))))
	  ((and (cl-type symbol) slot)
	   (let ((slot-var (cl-gensym (format "%s-var" slot))))
	     ;; list (macro-var method-args bindings
	     `(,macro-var
	       `(,',slot (eql ',,slot-var))
	       nil))))))
  
  (defun icg--process-object-spec
      (lambda (obj-spec)
	(pcase obj-spec
	  (`(,obj-type-var ,obj-type ,obj-id . ,field-specs)
	   (let ((fields
		  (mapcar
		   (lambda (field-spec)
		     (icg--process-field-spec
		      obj-id generic-type obj-impl obj-impl-var
		      field-spec)))
		   #'icg--process-field-spec field-specs)))
	   `(,slot ,id)
	   (let ((slot-var (cl-gensym (format "%s-var" slot))))
	     ;; list (macro-var method-args bindings
	     `(,macro-var
	       `(,',slot (eql ',,slot-var))
	       `((,',id
		  (,(intern (format "%s-%s"
				    ,obj-impl-var
				    ,macro-var))))))))
	  ((and (cl-type symbol) slot)
	   (let ((slot-var (cl-gensym (format "%s-var" slot))))
	     ;; list (macro-var method-args bindings
	     `(,macro-var
	       `(,',slot (eql ',,slot-var))
	       nil)))
	  (_ (error "Unrecognized object field spec %s" field-spec))))))

(defun icg--process-instantiation-specs (sp-op specs)
  "Analyze instantiation specs SPECS for specialized generic operator SP-OP."
  (let ((interface-signatures (get sp-op 'icg-interface-signatures))
	(object-signatures (get sp-op 'icg-object-signatures))
	(docstrings (get sp-op 'icg-operator-docstrings))
	(body (get sp-op 'icg-operator-body)))
    
    ))

(defmacro icg--specializing-operator-macro
    (macro-name op specialized-op)
  `(defmacro ,macro-name (&rest specs)
     (pcase-let ((`(,interface-bindings
		    ,slot-bindings
		    ,interface-specializers
		    ,arg-specializers
		    ,field-specializers
		    ,docstring
		    ,body)
		  (icg--process-instantiation-specs op specialized-op specs)
		  ))
       `(cl-defmethod ,',specialized-op
	  (,@interface-specializers
	   ,@arg-specializers
	   ,@field-specializers)
	  ,docstring
	  (cl-flet ,interface-bindings
	    (cl-symbol-macrolet	,slot-bindings
	      ,@body))))))

(defmacro icg--specialize-generic-operator-final
    (op interfaces objects fields methods &rest body)
  "Specialize a generic operator OP."
  (pcase-let ((`(,op-name ,op-args) op)
	      (`(,subj-id ,subj-type) subject))
  `(progn
     ;; generic to implement for instantiated methods
     (cl-defmethod ,op-name
       ,operator-specializing-arguments
       ,generic-docstring
       (,specialized-generic ,@operator-args
			     ,@specializing-field-args))
     (cl-defgeneric ,specialized-generic
	 (,@operator-args ,@field-args)
       ,generic-docstr)
     (icg--specialized-operator
     (defmacro ,macro-name ,field-instantiation-specs
       ,macro-docstr
       `(cl-defmethod ,',specialized-generic
	  (,@op-args ; generic is gen-symed, do not check specialization twice
	   ,@field-specializing-arguments)
	  ,(format ,docstr-template ,@instantiating-fields)
	  ,@',body)))))

(icg--specialize-generic-operator
 icg--insert
 ((acc :interface set :type hash-table ) (elt :prop key))
 (let ((q (or (gethash key set)
	      (puthash key (make-queue) set)))
       (queue-enqueue q obj))
     set))

(icg--define-define-op
 remove (set hash-table set) ((type obj (id key)))
 (let ((q (gethash key set)))
   (if q
       (progn
	 (setq q (queue-remove q obj))
	 (if (queue-empty q)
	     (remhash key set)
	   (puthash key q set)))
     (remhash key set))))

;; This is ugly, but will reduce repetitive code substantially
(defmacro icg--define-define-op (op subject objects &rest body)
  "Define an operator OP on SUBJECT and OBJECTS with implementation BODY."
  (pcase-let ((`(,subj-id ,subj-method ,type0 ,impl0) subject)
	      (`((,obj-id ,type1-sym . ,obj-fields) . ,args) objects))
    (let ((generic-name
	   (intern (format "icg--%s-%s" type0 op)))
	  (macro-name
	   (intern (format "icg--define-%s-%s" type0 op)))
	  (generic-docstr
	   (format "%s %s to %s %s."
		   (capitalize (symbol-name op))
		   (upcase (symbol-name obj-id))
		   type0
		   (upcase (symbol-name subj-id))))
	  (docstr-template
	   (format "%s %%s %s to %s %s."
		   (capitalize (symbol-name op))
		   (upcase (symbol-name obj-id))
		   type0
		   (upcase (symbol-name subj-id))))
	  (docstr-macro
	   (format "Convenience macro for icg--%s-%s generic."
		   type0 op)))
      (pcase obj-fields
	(`((,id1-sym ,prop-sym))
	 (let ((type1-var
		(intern (format "%s-var" type1-sym)))
	       (id1-var
		(intern (format "%s-var" id1-sym))))
	   `(progn
	      (cl-defmethod ,generic-name
		((,subj-id ,subj-impl-type) ,@obj-args)
		(,subj-method ,subj-id ,@obj-args
			      ,@fld-args))
	      (cl-defgeneric ,subj-method (,subj-id ,@obj-args ,@fld-args)
		,generic-docstr)
	      (defmacro ,macro-name (,type1-var ,id1-var)
		,docstr-macro
		`(cl-defmethod ,',generic-name
		   ((,',type1-sym (eql ',,type1-var))
		    (,',id1-sym (eql ',,id1-var))
		    ,,(if (null impl0)
			  ``,',subj-id
			``(,',subj-id ,',impl0))
		    ,',obj-id ,@',args)
		   ,(format ,docstr-template
			    ',type1-sym)
		   (let ((,',prop-sym
			  (,(intern (format "%s-%s" ,type1-var ,id1-var))
			   ,',obj-id)))
		     ,@',body))))))
	(`((,id1-sym ,prop1-sym) (,id2-sym ,prop2-sym))
	 (let ((type1-var
		(intern (format "%s-var" type1-sym)))
	       (id1-var
		(intern (format "%s-var" id1-sym)))
	       (id2-var
		(intern (format "%s-var" id2-sym))))
	   `(progn
	      (defgeneric ,generic-name
		(,type1-sym ,id1-sym ,id2-sym ,subj-id ,obj-id ,@args)
		,generic-docstr)
	      (defmacro ,macro-name (,type1-var ,id1-var ,id2-var)
		,docstr-macro
		`(cl-defmethod ,',generic-name
		   ((,',type1-sym (eql ',,type1-var))
		    (,',id1-sym (eql ',,id1-var))
		    (,',id2-sym (eql ',,id2-var))
		    ,,(if (null impl0)
			  ``,',subj-id
			``(,',subj-id ,',impl0))
		    ,',obj-id ,@',args)
		   ,(format ,docstr-template
			    ',type1-sym
			    ',id1-sym)
		   (let ((,',prop1-sym
			  (,(intern (format "%s-%s" ,type1-var ,id1-var))
			   ,',obj-id))
			 (,',prop2-sym
			  (,(intern (format "%s-%s" ,type1-var ,id2-var))
			   ,',obj-id)))
		     ,@',body))))))))))

(icg-define-generic-operator icg--insert
				 (collection acc)
				 (item elt))

(icg-define-generic-operator icg--delete
				 (collection acc)
				 (item elt))

(icg-define-generic-operator icg--find
				 (collection acc)
				 (item elt))

(icg-define-generic-operator icg--associate
				 (map dict)
				 (index key)
				 value)

(icg-define-generic-operator icg--disassociate
				 (map dict)
				 (index key)
				 value)

(icg-define-generic-operator icg--lookup
				 (map dict)
				 (index key)
				 value)

(provide 'icg)
;;; icg.el ends here

