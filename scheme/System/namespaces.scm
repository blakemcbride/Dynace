(require-library "synrule.ss")

;
;  Namespace support routines for MzScheme Release 51
;  by Blake McBride (blake@edge.net) 6/11/98
;
;  These routines provide a convenient method for creating, using,
;  sharing, and disposing of an arbitrary number of independent and
;  named namespaces.
;
;  The following global variables are available in all namespaces:
;
;    global-namespaces            this is a list of all namespaces
;    current-namespace-name       the name of the currently active namespace
;    exports                      a list of symbols exportable by this namespace
;    namespace-stack              the namespace push/pop stack
;
;  The following is a list of the general use procedures:
;  (nsn is a unique symbol identifying the namespace)
;
;  (new-namespace nsn)            create a new namespace
;  (set-current-namespace nsn)    switch to namespace nsn - once executed all defines
;                                 and execution is relative to the selected namespace
;  (use-namespace nsn)            switch to namespace nsn - once executed all defines
;                                 and execution is relative to the selected namespace
;                                 If the namespace doesn't exist, create it.
;  (export 'symbol)               make symbol exportable from this namespace
;  (export '(a b c))              make a b and c exportable
;  (import nsn flg)               import exportable symbols from nsn into the
;                                 current namespace.  If flg is #t imported
;                                 symbols which already exist are overwritten.
;                                 If #f existing symbols are not overwritten.
;  (remove-namespace nsn)         removes a namespace
;  (push-namespace nsn)           create namespace nsn (if it doesn't already exist) and
;                                 switch to it while keeping the prior namespace on a stack
;  (pop-namespace)                switch back to the last namespace pushed
;
;
;  Other interesting routines:
;  (ns is the actual namespace object returned from get-namespace)
;  
;  (get-namespace nsn)            returns ns, returns #f if name doesn't exist
;  (namespace-eval ns exp...)     executes exp... in namespace ns
;                                 without switching the current namespace
;  (namespace-import to-ns from-ns lst force)
;                                 import specified symbols from the ns (from-ns)
;                                 to the to ns (to-ns).  lst is either a list of specific
;                                 symbols to import, the symbol 'all for all symbols, or
;                                 'exports which refers to the exportable symbols of that
;                                 namespace

(define global-namespaces (list (cons 'global (current-namespace))))
(define exports '())
(define current-namespace-name 'global)
(define namespace-stack '())

(define-macro namespace-eval
  (lambda (ns . sexp)
    `(parameterize ((current-namespace ,ns))
		   (eval (cons 'begin (quote ,sexp))))))

(define-syntax namespace-eval-with-name
  (syntax-rules ()
    ((_ nsn vals ...)
     (let* [(ns (get-namespace nsn))]
       (cond [ns
	      (namespace-eval ns vals ...)]
	     [else
	      (error (string-append "Can't evaluate in namespace.  Namespace ["
				    (symbol->string nsn)
				    "] does not exist."))])))))

(define-macro get-namespace
  (lambda (name)
    `(let ((ns (assq ,name global-namespaces)))
       (and ns (cdr ns)))))

(define-macro new-namespace
  (lambda (name)
    `(let ((old (get-namespace ,name)))
       (if old
	   old
	   (let ((ns (make-namespace)))
	     (set! global-namespaces (cons (cons ,name ns) global-namespaces))
	     (namespace-eval ns
			     (define current-namespace-name ,name)
			     (define exports '()))
	     (namespace-import (get-namespace 'global) ns '(global-namespaces) #t)
;  the following line causes all symbols defined in global to be auto imported
	     (namespace-import ns (get-namespace 'global) 'all #f)
	     ns)))))

(define namespace-import
  (lambda (to from lst force)
    (let ((fn (parameterize ((current-namespace from)) (eval '(make-global-value-list))))
	  (exp (parameterize ((current-namespace from)) (eval 'exports))))
      (parameterize ((current-namespace to))
		    (for-each
		     (lambda (gvp)
		       (and (or (not (defined? (car gvp)))
				(and (not (built-in-name (car gvp)))
				     (not (macro? (global-defined-value (car gvp))))
				     (not (syntax? (global-defined-value (car gvp))))))
			    (or (not (defined? (car gvp)))
				force)
			    (or (and (pair? lst) (member (car gvp) lst))
				(eq? lst 'all)
				(and (eq? lst 'exports) (member (car gvp) exp)))
			    (eval `(define ,(car gvp) (quote ,(cdr gvp))))))
		     fn)))))

(define import
  (lambda (ns flg)
    (namespace-import (current-namespace) (get-namespace ns) 'exports flg)))

(define-macro set-current-namespace
  (lambda (name)
    `(let ((ns (get-namespace ,name)))
       (if ns
	   (begin
	     (namespace-import ns (current-namespace)
			       '(global-namespaces new-namespace namespace-eval import
						   remove-namespace export namespace-import
						   get-namespace use-namespace namespace-stack
						   set-current-namespace push-namespace
						   pop-namespace)
			       #t)
	     (current-namespace ns)
	     #t)
	   #f))))

(define-macro use-namespace
  (lambda (name)
    `(begin
       (new-namespace ,name)
       (set-current-namespace ,name))))

(define export
  (lambda (names)
    (if (not (pair? names))
	(set! names (list names)))
    (for-each
     (lambda (n)
       (or (member n (global-defined-value 'exports))
	   (global-defined-value 'exports (cons n (global-defined-value 'exports)))))
     names)))

(define-macro remove-namespace
  (lambda (name)
    `(cond ((not (eq? ,name 'global))
	    (if (eq? ,name current-namespace-name)
		(use-namespace 'global))
	    (letrec ((rn  (lambda (name lst)
			    (if (pair? lst)
				(if (eq? name (caar lst))
				    (rn name (cdr lst))
				    (cons (car lst) (rn name (cdr lst))))
				lst)))
		     (gns (global-defined-value 'global-namespaces)))
	      (global-defined-value 'global-namespaces (rn ,name gns))
	      (for-each (lambda (x)
			  (and (not (eq? (car x) (global-defined-value 'current-namespace-name)))
			       (namespace-import (cdr x) (current-namespace) '(global-namespaces) #t)))
			gns))))))

(define-macro push-namespace
  (lambda (name)
    `(begin
       (set! namespace-stack (cons current-namespace-name namespace-stack))
       (use-namespace ,name))))

(define pop-namespace
  (lambda ()
    (and
     (pair? namespace-stack)
     (let* ((ns (car namespace-stack)))
       (global-defined-value 'namespace-stack (cdr namespace-stack))
       (use-namespace ns)))))

;  end of file
