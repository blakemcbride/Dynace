(require-library "namespaces.scm" "System")
(require-library "synrule.ss")

(define-macro defmacro
  (lambda (name args . body)
    `(define-macro ,name (lambda ,args ,@body))))

(define-macro NewGeneric
  (lambda (method . args)
    `(lambda (selfa ,@args)
       (send selfa ,method ,@args))))

(define-macro defclass
  (lambda (sc . args)
    `(class ,(if (eq? 'null sc) 'object% sc)
	    ,@args
	    (sequence (super-init)))))

(define NewInstance make-object)

(define-id-macro self 'this)

(define-syntax setq
  (syntax-rules ()
		((_ var val)
		 (begin
		   (set! var val)
		   var))))

(define-syntax while
  (syntax-rules ()
		((_ test body ...)
		 (do ()
		     ((not test) ())
		   body ...))))

(define-syntax do-times
  (syntax-rules ()
		((_ n body ...)
		 (do ((count n (- count 1)))
		     ((<= count 0) ())
		   body))))

(define-syntax for
  (syntax-rules ()
		((_ init wh incr body ...)
		 (begin
		   init
		   (while wh body ... incr)))))

(define-syntax +=
  (syntax-rules ()
		((_ var val)
		 (setq var (+ var val)))))

(define-syntax -=
  (syntax-rules ()
		((_ var val)
		 (setq var (- var val)))))

(define-syntax +x
  (syntax-rules ()
		((_ var)
		 (setq var (+ var 1)))))

(define-syntax -x
  (syntax-rules ()
		((_ var)
		 (setq var (- var 1)))))

(define-syntax x+
  (syntax-rules ()
		((_ var)
		 (let* ((hold var))
		   (set! var (+ var 1))
		   hold))))

(define-syntax x-
  (syntax-rules ()
		((_ var)
		 (let* ((hold var))
		   (set! var (- var 1))
		   hold))))

(define-syntax <>
  (syntax-rules ()
		((_ x y)
		 (not (= x y)))))

(define-syntax not-zero?
  (syntax-rules ()
		((_ x)
		 (not (zero? x)))))

(define-syntax div
  (syntax-rules ()
		((_ x y)
		 (if (and (integer? x) (integer? y))
		     (quotient x y)
		     (/ x y)))))

(define-syntax neq?
  (syntax-rules ()
		((_ x y)
		 (not (eq? x y)))))

(define-syntax EvalInNamespace
  (syntax-rules ()
		((_ ns fn parms ...)
		 (let* [(cmd   "(")
			(tmp   (list parms ...))
;			(flist (list fn parms ...))
			(func  nil)]

		   (set! cmd (string-append cmd (symbol->string fn)))
		   (while (pair? tmp)
			  (set! cmd (string-append cmd " " (value->string (car tmp) #t)))
			  (set! tmp (cdr tmp)))
		   
		   (set! cmd (string-append "(namespace-eval (get-namespace "
					    (value->string ns #t)
					    ") " cmd "))"))
		   (eval-string cmd)))))
;		   (set! func (list 'namespace-eval '(get-namespace ns) flist))
;		   (eval func)))))


(define-syntax sMakeList
  (syntax-rules ()
		((_ vals ...)
		 (sMakeListFun (list vals ...)))))

(define-syntax sGetValues
  (syntax-rules ()
		((_ lsobj vals ...)
		 (let* [(ls null)]
		   (set! ls (let mlist [(rls null)
					(seq (gSequence lsobj))]
			      (let* [(obj (gNext seq))]
		   
				(cond [(not-null? obj)
				       (set! rls (cons obj rls))
				       (set! rls (mlist rls seq))]))
			      rls))
		   (sGetValuesFromSchemeList (reverse ls) vals ...)))))

(define-syntax sGetValuesFromSchemeList
  (syntax-rules ()
		((_ ls val)
		 (let* [(lv (car ls))]
		   (cond [(not-zero? (gIsKindOf lv String))
			  (set! val (gStringValue lv))]
			 [(not-zero? (gIsKindOf lv DoubleFloat))
			  (set! val (gDoubleValue lv))]
			 [(not-zero? (gIsKindOf lv LongInteger))
			  (set! val (gLongValue lv))]
			 [(not-zero? (gIsKindOf lv ShortInteger))
			  (set! val (gShortValue lv))]
			 [else
			  (set! val lv)])))
		((_ ls val1 vals ...)
		 (let* [(lv (car ls))]
		   (cond [(not-zero? (gIsKindOf lv String))
			  (set! val1 (gStringValue lv))]
			 [(not-zero? (gIsKindOf lv DoubleFloat))
			  (set! val1 (gDoubleValue lv))]
			 [(not-zero? (gIsKindOf lv LongInteger))
			  (set! val1 (gLongValue lv))]
			 [(not-zero? (gIsKindOf lv ShortInteger))
			  (set! val1 (gShortValue lv))]
			 [else
			  (set! val1 lv)])
		   (sGetValuesFromSchemeList (cdr ls) vals ...)))))

(define-syntax require-file
  (syntax-rules ()
		((_ filename col)
		 (let* [(fn       (if col
				      (string-append (car (current-library-collection-paths))
						     "/" col "/" filename)
				      filename))
			(fix-name (lambda (n1)
				    (let* [(rval null)]
				      (let nlet [(ls (string->list n1))]
					(cond [(pair? ls)
					       (if (char=? (car ls) #\\)
						   (set! rval (cons #\/ rval))
						   (set! rval (cons (char-downcase (car ls)) rval)))
					       (nlet (cdr ls))]))
				      (list->string (reverse rval)))))
			(get-file-info
			 (lambda (fnstr)
			   (let* [(tls  null)
				  (fnm  "")
				  (path "")
				  (nmls null)
				  (dnm  "")
				  (znm  "")]

			     (let nlet1 [(fnls (reverse (string->list fnstr)))]
			       (cond [(and (pair? fnls)
					   (not (char=? (car fnls) #\.)))
				      (set! tls (cdr fnls))
				      (nlet1 tls)]))

			     (let nlet2 [(fnls tls)]
			       (cond [(and (pair? fnls)
					   (not (char=? (car fnls) #\/)))
				      (set! nmls (cons (car fnls) nmls))
				      (set! tls (cdr fnls))
				      (nlet2 tls)]))

			     (set! fnm (list->string nmls))
			     (set! path (list->string (reverse tls)))
      
			     (set! dnm (string-append path
						      "compiled/native/win32/i386/"
						      fnm
						      "dll"))
			       
			     (set! znm (string-append path
						      "compiled/"
						      fnm
						      "zo"))
			       
			     (list dnm znm))))
			(fullName (fix-name fn))
			(nvals    null)
			(flname   (string->symbol (string-append "*" (symbol->string current-namespace-name)
								 "-Require-File-FileList*")))
			(dllName  null)
			(zoName   null)
			(testName #f)
			(FileList null)
			(data     null)
			(rval     null)
			(ntl      #f)]

		   (if (not (defined? flname))
		       (global-defined-value flname null))
		   (set! FileList (eval flname))
		   (set! data (assoc fullName FileList))
		   (cond [(file-exists? fullName)
			  (set! testName fullName)]
			 [else
			  (set! nvals (get-file-info fullName))
			  (set! dllName (car nvals))
			  (set! zoName (cadr nvals))
			  (cond [(file-exists? dllName)
				 (set! testName dllName)]
				[(file-exists? zoName)
				 (set! testName zoName)]
				[else
				 (error 'require-file (string-append "File [" fullName "] does not exist."))])])
		   (if data
		       (let* [(size (caadr data))
			      (modd (cadadr data))]
			 (set! ntl (or (not (= size (file-size testName)))
				       (not (= modd (file-or-directory-modify-seconds testName))))))
		       (set! ntl #t))
		   (cond [ntl
			  (set! rval ((if (and (defined? 'USE-SCHEME-DEBUGGER) USE-SCHEME-DEBUGGER) debug-load load/use-compiled) fullName))
			  (cond [data
				 (set-car! (cadr data) (file-size testName))
				 (set-car! (cdadr data) (file-or-directory-modify-seconds testName))
				 (set-car! (cddadr data) rval)]
				[else
				 (set! FileList (cons (list fullName
							    (list (file-size testName)
								  (file-or-directory-modify-seconds testName)
								  rval))
						      FileList))])]
			 [data
			  (set! rval (car (cddadr data)))])
		   (global-defined-value flname FileList)
		   rval))
		((_ filename)
		 (require-file filename #f))))

(require-library "ClassInfo.scm" "System")

;(load (build-path (car (current-library-collection-paths))  "System" "ads.scm"))
