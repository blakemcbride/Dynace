(let* [(debug-flag (Getenv "USE_SCHEME_DEBUGGER"))]
  (cond [(and (string-ci=? debug-flag "Y")
	      (file-exists? "scheme/system/display.scm")
	      (file-exists? "scheme/system/debugger.scm"))
	 (global-defined-value 'USE-SCHEME-DEBUGGER #t)
	 (require-library "display.scm" "system")
	 (require-library "debugger.scm" "system")]
	(else
	 (require-library "nodebugger.scm" "system"))))
(require-library "DynaceMacros.scm" "System")
(require-library "pointers.scm" "System")
(require-library "string.ss")
(require-library "file.ss")

(define NewInstance make-object)

(define crlf (string (integer->char 13) #\newline))

(define string-write
  (lambda (val)
    (let ((port (open-output-string)))
      (write val port)
      (get-output-string port))))

(define nil '())

(define (Msg s)
  (let* [(sp (open-output-string))]
    (if (string? s)
	(display s sp)
	(write s sp))
    (gMessage Application (get-output-string sp))
    s))

(define value->string
  (lambda (val doQuote)
    (let* [(str "")]
      
      (cond [(boolean? val)
	     (if val
		 (set! str "#t")
		 (set! str "#f"))]
	    [(number? val)
	     (set! str (number->string val))]
	    [(char? val)
	     (set! str (string val))]
	    [(string? val)
	     (set! str (string-append (string #\") val (string #\")))]
	    [(symbol? val)
	     (set! str (string-append "'" (symbol->string val)))]
	    [(list? val)
	     (let* [(first #t)]

	       (if doQuote
		   (set! str "'(")
		   (set! str "("))
	       (while (pair? val)
		      (if first
			  (set! first #f)
			  (set! str (string-append str " ")))
		      (set! str (string-append str (value->string (car val) #f)))
		      (set! val (cdr val)))
	       (set! str (string-append str ")")))]
	    [(dynace-object? val)
	     (set! str (string-append "(int->object " (number->string (object->int val)) ")"))]
	    [else
	     (set! str "'()")])
      str)))

(define null-object (int->object 0))
(define NULL (int->object 0))

(define (not-null? p)
  (not (pointers-equal? null-object p)))

(define (is-null? p)
  (pointers-equal? null-object p))

(define (string->function s)
  (object->pointer (gNewWithStr String s)))

(define (int->double n)
  (exact->inexact n))

(define (list-set! ml n val)
  (set-car! (list-tail ml n) val)
  ml)

(define sMakeListFun
  (lambda (lsin)
    (let mlist [(lobj (gNew LinkObject))
		(ls lsin)]
      (if (pair? ls)
	  (let* [(val (car ls))
		 (obj null)]
	    (cond [(dynace-object? val)
		   (set! obj val)]
		  [(string? val)
		   (set! obj (gNewWithStr String val))]
		  [(number? val)
		   (cond [(inexact? val)
			  (set! obj (gNewWithDouble DoubleFloat val))]
			 [(exact? val)
			  (set! obj (gNewWithLong LongInteger val))])]
		  [else
		   (set! obj  (gNew LongInteger))])
	    (gAddBefore lobj (gNewWithObj LinkValue obj))
		
	    (mlist lobj (cdr ls))))
      lobj)))

(require-library "ads.scm" "System")
(require-library "wds.scm" "System")
;(load (build-path (car (current-library-collection-paths))  "System" "ads.scm"))
