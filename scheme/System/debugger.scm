(if (string=? (version) "103")
    (require-library "synrule.ss"))

; (define defined?
;   (lambda (x)
;     #f))

(define HWND_TOP       0)
(define HWND_BOTTOM    1)

(define *context* '(((global-environment () ()))))

(define-syntax debug-set!
  (syntax-rules ()
    ((_ var val)
     (let ((res val))
       (set! var res)
       (debug-set-in-context 'var res)
       res))))

(define debug-set-in-context
  (lambda (sym val)
    (let ((found #f))
      (let loop ((lets (caddar (car *context*))))
        (cond ((pair? lets)
               (let loop ((vars (car lets)))
                 (if (pair? vars)
                     (cond ((eq? (caar vars) sym)
                            (set-car! (cdar vars) val)
                            (set! found #t))
                           (else
                            (loop (cdr vars))))))
               (if (not found)
                   (loop (cdr lets))))))
      (if (not found)
          (let loop ((vars (cadar (car *context*))))
            (if (pair? vars)
                (cond ((eq? (caar vars) sym)
                       (set-car! (cdar vars) val)
                       (set! found #t))
                      (else
                       (loop (cdr vars))))))))))

(define-syntax debug-define
  (syntax-rules (lambda)
    ((_ fun (lambda (arg1 ...) pgm ...))
     (define fun (debug-lambda fun (arg1 ...) pgm ...)))
    ((_ (fun arg1 ...) pgm ...)
     (debug-define fun (lambda (arg1 ...) pgm ...)))
    ((_ (fun . rest) pgm ...)
     (debug-define fun (lambda rest pgm ...)))
    ((_ fun (lambda arg pgm ...))
     (define fun (debug-lambda fun arg pgm ...)))
    ((_ var val)
     (define var val))))

(define-syntax debug-lambda
  (syntax-rules ()
    ((_ fun ((arg1 ...) (arg2 . rest)) pgm ...)
     (debug-lambda fun ((arg1 ... arg2) rest) pgm ...))
    ((_ fun ((arg1 ...) rest) pgm ...)
     (lambda (arg1 ... . rest)
       (set-car! *context* (cons
			    (list 'fun	;  function level
				  (list	;  arguments
				   (list 'arg1 arg1) ... ;  argument and value
				   (list 'rest rest))
				  '())	;  a place for let's
			    (car *context*)))
       (let ((res (let () pgm ...)))
	 (set-car! *context* (cdr (car *context*)))
	 res)))
    ((_ fun (arg1 ...) pgm ...)
     (lambda (arg1 ...)
       (set-car! *context* (cons 
			    (list 'fun	;  function level
				  (list	;  arguments
				   (list 'arg1 arg1) ...) ;  argument and value
				  '())	;  a place for let's
			    (car *context*)))
       (let ((res (let () pgm ...)))
	 (set-car! *context* (cdr (car *context*)))
	 res)))
    ((_ fun (arg1 . rest) pgm ...)
     (debug-lambda fun ((arg1) rest) pgm ...))
    ((_ fun arg pgm ...)
     (lambda arg
       (set-car! *context* (cons 
			    (list 'fun	;  function level
				  (list	;  arguments
				   (list 'arg arg)) ;  argument and value
				  '())	;  a place for let's
			    (car *context*)))
       (let ((res (let () pgm ...)))
	 (set-car! *context* (cdr (car *context*)))
	 res)))))


(define-syntax debug-let
  (syntax-rules ()
    ((_ ((var1 val1) ...) pgm ...)
     (let ((var1 val1) ...)
       (let ((statep (cddar (car *context*)))
	     (res '()))
	 (set-car! statep (cons 
			   (list	; new let context
			    (list 'var1 var1) ...) ; list of bindings within the let
			   (car statep)))
	 (set! res (let () pgm ...))
	 (set-car! statep (cdar statep))
	 res)))
    ((_ tag ((var1 val1) ...) pgm ...)
     (let tag ((var1 val1) ...)
       (let ((statep (cddar (car *context*)))
	     (res '()))
	 (set-car! statep (cons 
			   (list	; new let context
			    (list 'var1 var1) ...) ; list of bindings within the let
			   (car statep)))
	 (set! res (let () pgm ...))
	 (set-car! statep (cdar statep))
	 res)))))

(define-syntax debug-let*
  (syntax-rules ()
    ((_ ((var1 val1) ...) pgm ...)
     (let* ((var1 val1) ...)
       (let ((statep (cddar (car *context*)))
	     (res '()))
	 (set-car! statep (cons 
			   (list	; new let context
			    (list 'var1 var1) ...) ; list of bindings within the let
			   (car statep)))
	 (set! res (let () pgm ...))
	 (set-car! statep (cdar statep))
	 res)))))

(define-syntax debug-letrec
  (syntax-rules ()
    ((_ ((var1 val1) ...) pgm ...)
     (letrec ((var1 val1) ...)
       (let ((statep (cddar (car *context*)))
	     (res '()))
	 (set-car! statep (cons 
			   (list	; new let context
			    (list 'var1 var1) ...) ; list of bindings within the let
			   (car statep)))
	 (set! res (let () pgm ...))
	 (set-car! statep (cdar statep))
	 res)))))

(define-syntax debug-fluid-let
  (syntax-rules ()
    ((_ ((var1 val1) ...) pgm ...)
     (fluid-let ((var1 val1) ...)
       (let ((statep (cddar (car *context*)))
	     (res '()))
	 (set-car! statep (cons 
			   (list	; new let context
			    (list 'var1 var1) ...) ; list of bindings within the let
			   (car statep)))
	 (set! res (let () pgm ...))
	 (set-car! statep (cdar statep))
	 res)))))

; 'do' taken from The Scheme Programming Language by Kent Dybvig
; we can't use this next function until MZScheme 200
; (define-syntax debug-do
;   (lambda (x)
;     (syntax-case x ()
; 		 ((_ (binding ...) (test res ...) exp ...)
; 		  (with-syntax ((((var val update) ...)
; 				 (map (lambda (b)
; 					(syntax-case b ()
; 						     ((var val)
; 						      (syntax (var val var)))
; 						     ((var val update)
; 						      (syntax (var val update)))))
; 				      (syntax (binding ...)))))
; 			       (syntax (debug-let doloop ((var val) ...)
; 					 (if test
; 					     (begin (if #f #f) res ...)
; 					     (begin exp ... (doloop update ...))))))))))

; we have to use the next function for MZScheme 103.  Note that
; each initialized variable MUST have an update option.
(define-syntax debug-do
  (syntax-rules ()
    ((_ ((var val update) ...) (test res ...) exp ...)
     (debug-let doloop ((var val) ...)
		(cond (test
		       (if #f #f)
		       res ...)
		      (else
		       exp ...
		       (doloop update ...)))))))

(define-syntax break-if
  (syntax-rules ()
    ((_  tag condition)
     (if condition
	 (break tag)))
    ((_ condition)
     (if condition
	 (break)))))

(define exception-break 
  (lambda (exn)
    (display "Program error")
    (cond ((pair? (car *context*))
	   (if (eq? (caar (car *context*)) 'global-environment)
	       (display " in the ")
	       (display " in function "))
           (write (caar (car *context*)))))
    (newline)
    (display "exception = ")
    (write (exn-message exn))
    (newline)
    (break)))

(define-syntax value
  (syntax-rules ()
    ((_ var)
     (value-value (quote var)))))

(define value-value
  (lambda (sym)
    (let ((found #f)
	  (val '()))
      (let loop ((lets (caddar (car *context*))))
	(cond ((pair? lets)
	       (let loop ((vars (car lets)))
		 (if (pair? vars)
		     (cond ((eq? (caar vars) sym)
			    (set! val (cadar vars))
			    (set! found #t))
			   (else
			    (loop (cdr vars))))))
	       (if (not found)
		   (loop (cdr lets))))))
      (if (not found)
	  (let loop ((vars (cadar (car *context*))))
	    (if (pair? vars)
		(cond ((eq? (caar vars) sym)
		       (set! val (cadar vars))
		       (set! found #t))
		      (else
		       (loop (cdr vars)))))))
      (if found
	  val
	  '*undefined*))))
    

(current-exception-handler exception-break)

(define break
  (lambda tag
    
    (define backtrace
      (lambda ()
        (let loop ((v (car *context*)))
          (cond ((pair? v)
                 (write (car v))
                 (newline)
                 (loop (cdr v)))))))
    
    (define fctrace
      (lambda ()
        (let loop ((v (car *context*)))
          (cond ((and (pair? v)
		      (pair? (cdr v)))
                 (write (caar v))
                 (newline)
                 (loop (cdr v)))))))
    
    (define display-value
      (lambda (sym)
        (let ((found #f)
              (val '()))
          (let loop ((lets (caddar (car *context*))))
            (cond ((pair? lets)
                   (let loop ((vars (car lets)))
                     (if (pair? vars)
                         (cond ((eq? (caar vars) sym)
                                (set! val (cadar vars))
                                (set! found #t))
                               (else
                                (loop (cdr vars))))))
                   (if (not found)
                       (loop (cdr lets))))))
          (if (not found)
              (let loop ((vars (cadar (car *context*))))
                (if (pair? vars)
                    (cond ((eq? (caar vars) sym)
                           (set! val (cadar vars))
                           (set! found #t))
                          (else
                           (loop (cdr vars)))))))
          (cond (found
                 (display sym)
                 (display " = ")
                 (write val)
                 (newline))
                (else
                 (display sym)
                 (display " not found")
                 (newline))))))
    
    (cond [(pair? tag)
	   (newline)
	   (display "User break at ")
	   (display (car tag))
	   (newline)
	   (newline)])
    
    (let loop ((val '()))
      (display "debug> ")
      (set! val (read))
      (cond ((eq? val 'c)    ; continue
	     (if (and (defined? 'Dynace)
		      (defined? '*display-window*)
		      (dynace-object? *display-window*)
		      (IsObj *display-window*))
		 (gSetZOrder *display-window* HWND_BOTTOM)))
	    ((eq? val 'k)    ; continue
	     (if (and (defined? 'Dynace)
		      (defined? '*display-window*)
		      (dynace-object? *display-window*)
		      (IsObj *display-window*))
		 (gDispose *display-window*)))
            ((eq? val 'bt)   ; back trace
             (backtrace))
            ((eq? val 'fc)   ; function calls
             (fctrace))
            ((eq? val 'sf)   ; stack frame
             (cond ((pair? (car *context*))
		    (if (eq? (caar (car *context*)) 'global-environment)
			(display "Environment = ")
			(display "Function = "))
                    (display (caar (car *context*)))
                    (newline)
                    (display "Arguments = ")
                    (write (cadar (car *context*)))
                    (newline)
                    (display "Additional bindings = ")
                    (write (caddar (car *context*)))
                    (newline)
                    (newline))
                   (else
                    (display "No context")
                    (newline))))
            ((eq? val 'val)
             (display-value (read)))
            ((eq? val 'help)
             (newline)
             (display "c            continue")(newline)
	     (cond [(defined? 'Dynace)
		    (display "k            continue and kill debug window")
		    (newline)])
             (display "bt           back trace - all stack frames")(newline)
             (display "sf           current stack frame")(newline)
             (display "fc           list function calls")(newline)
             (display "val x        display the value of x")(newline)
	     (display "(value var)  returns the value of var")(newline)
             (display "other arguments are evaluated")
             (newline)
             (newline))
            (else
             (with-handlers ((not-break-exn? 
                              (lambda (exn) 
                                (display "I don't understand.")
                                (newline))))
               (write (eval val))
               (newline))))
      (if (and (not (eq? val 'c))
	       (not (eq? val 'k)))
          (loop val)))))


; The following function is the correct function

; (define debug
;   (lambda (fun)

;     (define debug2
;       (lambda (fun prv)
;         (cond ((pair? fun)
;                (cond ((eq? 'lambda (car fun))
;                       (cons (debug2 (car fun) prv)
;                             (cons prv
;                                   (debug2 (cdr fun) (car fun)))))
;                      (else
;                       (cons (debug2 (car fun) prv)
;                             (debug2 (cdr fun) (car fun))))))
;               (else 
;                (cond ((eq? fun 'define)
;                       'debug-define)
;                      ((eq? fun 'lambda)
;                       'debug-lambda)
;                      ((eq? fun 'let)
;                       'debug-let)
;                      ((eq? fun 'let*)
;                       'debug-let*)
;                      ((eq? fun 'letrec)
;                       'debug-letrec)
;                      ((eq? fun 'fluid-let)
;                       'debug-fluid-let)
;                      ((or (eq? fun 'set!)
;                           (eq? fun 'setq))
;                       'debug-set!)
;                      ((eq? fun 'do)
;                       'debug-do)
; 		     ((eq? fun 'load/use-compiled)
; 		      'debug-load)
; 		     ((eq? fun 'load)
; 		      'debug-load)
;                      (else fun))))))
;     (debug2 fun null)))


; The following function is used with MzScheme 103 to get around the fact that dotted pair argument lists are not handled correctly

(define debug
  (lambda (fun)

    (define  improper-list
      (lambda (x)
	(if (pair? x)
	    (let name ((y (cdr x)))
	      (if (pair? y)
		  (name (cdr y))
		  (not (null? y))))
	    #f)))
   
    (define debug2
      (lambda (fun prv il)
        (cond ((pair? fun)
               (cond ((eq? 'lambda (car fun))
		      (if (improper-list (cadr fun))
			  (cons (debug2 (car fun) prv #t)
				(debug2 (cdr fun) (car fun) #f))
			  (cons (debug2 (car fun) prv #f)
				(cons prv
				      (debug2 (cdr fun) (car fun) #f)))))
 		     ((eq? 'define (car fun))
 		      (cond ((pair? (cadr fun))
 			     (cons (debug2 (car fun) prv (improper-list (cadr fun)))
				   (debug2 (cdr fun) (car fun) #f)))
 			    ((and (pair? (caddr fun))
 				  (eq? 'lambda (caaddr fun)))
 			     (cons (debug2 (car fun) prv (improper-list (car (cdaddr fun))))
				   (debug2 (cdr fun) (car fun) #f)))
 			    (else
 			     (cons (debug2 (car fun) prv #f)
				   (debug2 (cdr fun) (car fun) #f)))))
                     (else
                      (cons (debug2 (car fun) prv #f)
                            (debug2 (cdr fun) (car fun) #f)))))
              (else 
               (cond ((and (eq? fun 'define) (not il))
                      'debug-define)
                     ((and (eq? fun 'lambda) (not il))
                      'debug-lambda)
                     ((eq? fun 'let)
                      'debug-let)
                     ((eq? fun 'let*)
                      'debug-let*)
                     ((eq? fun 'letrec)
                      'debug-letrec)
                     ((eq? fun 'fluid-let)
                      'debug-fluid-let)
                     ((or (eq? fun 'set!)
                          (eq? fun 'setq))
                      'debug-set!)
                     ((eq? fun 'do)
                      'debug-do)
		     ((eq? fun 'load/use-compiled)
		      'debug-load)
		     ((eq? fun 'load)
		      'debug-load)
                     (else fun))))))
   
    (debug2 fun null #f)))

(define debug-eval
  (lambda (e)
    (eval (debug e))))

(define debug-file
  (lambda (from-file to-file)

    (define delete-file2
      (lambda (f)
	(with-handlers ([exn:i/o:filesystem? 
			 (lambda (exn) #f)])
		       (if (chmod-write f)
			   (begin
			     (delete-file f)
			     #t)))))
    (delete-file2 to-file)
    
    (let ((in (open-input-file from-file))
	  (out (open-output-file to-file)))
      (let loop ((exp (read in)))
	(cond ((not (eof-object? exp))
	       (write (debug exp) out)
               (newline out)
	       (loop (read in)))))
      (close-input-port in)
      (close-output-port out))))

(define debug-load
  (lambda (from-file)
    (let ((in (open-input-file from-file)))
      (let loop ((exp (read in)))
	(cond ((not (eof-object? exp))
               (eval (debug exp))
	       (loop (read in)))))
      (close-input-port in))))
