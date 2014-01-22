(define HWND_TOP       0)
(define HWND_BOTTOM    1)

(define WS_VISIBLE          #x10000000)
(define WS_CAPTION          #x00C00000)
(define WS_SYSMENU          #x00080000)
(define WS_THICKFRAME       #x00040000)
(define WS_MINIMIZEBOX      #x00020000)
(define WS_MAXIMIZEBOX      #x00010000)

(define SM_1_PER_CHAR       #b0010)  ;  1 per a particlar font

(define *display-window* null)

(define *display* display)
(define *write* write)
(define *current-output-port* (current-output-port))
(define *current-input-port*  (current-input-port))
(define *newline* newline)
(define *read* read)
(define *dynace-string-port* #f)

(define *close-display-window*
  (lambda (wind)
    (set! *display-window* null)
    (gDispose wind)
    1))

(define display
  (lambda (x . port)
    (*dynace-display* *display* x port)))

(define write
  (lambda (x . port)
    (*dynace-display* *write* x port)))

(define newline
  (lambda port
    (*dynace-display* *display* #\newline port)))

(define *dynace-display* 
  (lambda (dis x port)
    (cond ((and (dynace-object? x)
		(IsObj x))
	   (let ((sobj (gStringRep x)))
	     (set! x (gStringValue sobj))
	     (gDispose sobj)))
	  ((dynace-object? x)
	   (set! x "<Invalid Dynace Object>")))
    (cond ((and (null? port)
		(eq? *current-output-port* (current-output-port)))
	   (cond ((eq? *display-window* null)
		  (let* ((pm (gSetScalingMode Application SM_1_PER_CHAR)))
		    (set! *display-window* (gNewPopupWindow PopupWindow "Scheme display window" 20 80))
		    (gSetScalingMode Application pm))
;		  (gSetStyle *display-window*
;			     (bitwise-and (bitwise-ior
;					   WS_VISIBLE
;					   WS_CAPTION
;					   WS_THICKFRAME)
;					  (bitwise-not WS_MINIMIZEBOX)
;					  (bitwise-not WS_MAXIMIZEBOX)
;					  (bitwise-not WS_SYSMENU)))

		  (gCompletionFunction *display-window* (object->pointer
							 (gNewWithStr String "*close-display-window*")))
		  (gSetEcho *display-window* 1)
		  (gSetMaxLines *display-window* 1000)
		  (gShow *display-window*)))

	   (gSetZOrder *display-window* HWND_TOP)

	   (let ((sp (open-output-string)))
	     (dis x sp)
	     (gPuts *display-window* (get-output-string sp))
	     (close-output-port sp)
	     (gRedrawWindow *display-window*)))
	  ((null? port)
	   (dis x))
	  ((not (= (length port) 1))
	   (let ((sp (open-output-string)))
	     (*display* (string-append "display: expects 1 to 2 arguments, given "
				       (number->string (+ (length port) 1))
				       ": ") sp)
	     (*write* x sp)
	     (let loop ((ls port))
	       (cond ((pair? ls)
		      (*display* " " sp)
		      (*write* (car ls) sp)
		      (loop (cdr ls)))))
	     (error (get-output-string sp))
	     (close-output-port sp)))
	  (else
	   (dis x (car port))))
    x))

(define read 
  (lambda port
    (cond ((and (null? port)
		(eq? *current-output-port* (current-output-port)))
	   (cond ((eq? *display-window* null)
		  (set! *display-window* (gNewPopupWindow PopupWindow "Scheme display window" 20 80))
		  (gCompletionFunction *display-window* (object->pointer (gNewWithStr String "*close-display-window*")))
		  (gSetEcho *display-window* 1)
		  (gShow *display-window*)))

	   (let* ((sp   (if *dynace-string-port* *dynace-string-port* (open-output-string)))
		  (rval null)
		  (mrfs (lambda (str)
			  (with-handlers ([not-break-exn? (lambda (exn) (set! *dynace-string-port* #f)
								  'dynace-read-continue)])
					 (let* [(sp   (open-input-string str))
						(rval (*read* sp))
						(rest (read-line sp))]
					   (close-input-port sp)
					   (cond [(eof-object? rval)
						  (error "")]
						 [(and (not (eof-object? rest))
						       (not (zero? (string-length rest))))
						  (set! *dynace-string-port* (open-output-string))
						  (display rest *dynace-string-port*)]
						 [else
						  (set! *dynace-string-port* #f)])
					   rval)))))
	     (let loop ((done #f))
	       (cond ((not done)
		      (set! done #t)
		      (cond ((not *dynace-string-port*)
			     (display (sGets *display-window*) sp)))
		      (set! rval (mrfs (get-output-string sp)))
		      (if (and (symbol? rval) (eq? rval 'dynace-read-continue))
			  (set! done #f))
		      (loop done))))
	     (close-output-port sp)
	     rval))
	  ((null? port)
	   (*read*))
	  ((not (= (length port) 1))
	   (let ((sp (open-output-string)))
	     (*display* (string-append "read: expects 0 to 1 arguments, given "
				       (number->string (+ (length port) 1))
				       ":") sp)
	     (let loop ((ls port))
	       (cond ((pair? ls)
		      (*display* " " sp)
		      (*write* (car ls) sp)
		      (loop (cdr ls)))))
	     (error (get-output-string sp))
	     (close-output-port sp)))
	  (else
	   (*read* (car port))))))

    