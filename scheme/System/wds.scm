(require-library "Windows.scm" "System")

(define MASK_FORMAT_NONE	0)
(define MASK_FORMAT_PHONE	1)
(define	MASK_FORMAT_ZIPCODE	2)
(define	MASK_FORMAT_SSNUMBER	3)

(define DBMS_WATCOM  1)
(define DBMS_ACCESS  2)
(define DBMS_SYBASE  3)
(define DBMS_MSSQL   4)
(define DBMS_MYSQL   5)

(define (sInitFunction win fun)
  (gInitFunction win (string->function fun)))

(define (sAssociate win id fun)
  (gAssociate win id (string->function fun)))

(define (sSetFunction ctl fun)
  (gSetFunction ctl (string->function fun)))

(define (sSetChgFunction ctl fun)
  (gSetChgFunction ctl (string->function fun)))

(define (sCompletionFunction win fun)
  (gCompletionFunction win (string->function fun)))

(define (sCheckFunction ctl fun)
  (gCheckFunction ctl (string->function fun)))

(define (sAddTab ctl id name init1 comp1 act1 deact1)
  (let* [(init  (if (zero? (string-length init1))
		    (object->pointer null-object)
		    (string->function init1)))
	 (comp  (if (zero? (string-length comp1))
		    (object->pointer null-object)
		    (string->function comp1)))
	 (act   (if (zero? (string-length act1))
		    (object->pointer null-object)
		    (string->function act1)))
	 (deact (if (zero? (string-length deact1))
		    (object->pointer null-object)
		    (string->function deact1)))]
    (gAddTab ctl id name init comp act deact)))

(define (sAddTabFromFile ctl id name init1 comp1 act1 deact1 filename)
  (let* [(init  (if (zero? (string-length init1))
		    (object->pointer null-object)
		    (string->function init1)))
	 (comp  (if (zero? (string-length comp1))
		    (object->pointer null-object)
		    (string->function comp1)))
	 (act   (if (zero? (string-length act1))
		    (object->pointer null-object)
		    (string->function act1)))
	 (deact (if (zero? (string-length deact1))
		    (object->pointer null-object)
		    (string->function deact1)))]
    (gAddTabFromFile ctl id name init comp act deact filename)))

(define-macro vPrintf
  (lambda (wind mask . args)
    `(gPuts ,wind (format ,mask ,@args))))

(define (sLoadMenuFromFile win file id)
  (gUse win (gLoadResourceFromFile ExternalMenu file id)))

(define (sEventFunction ctl event fun)
  (gEventFunction ctl event (string->function fun)))


; The following functions should not be used!  We can't delete them right now, though.
(define WDS:CTL_STATIC     0)
(define WDS:CTL_TEXT       1)
(define WDS:CTL_NUMERIC    2)
(define WDS:CTL_DATE       3)
(define WDS:CTL_BUTTON     4)
(define WDS:CTL_CHECKBOX   5)
(define WDS:CTL_COMBOBOX   6)
(define WDS:CTL_RADIOBTN   7)
(define WDS:CTL_LISTBOX    8)
(define WDS:CTL_STATICTEXT 9)

(defmacro WDS:AddStaticControl (wnd row col var name text)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 0 ,name ,text "" "" WDS:CTL_STATIC)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddTextControl (wnd row col width var name)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 ,width ,name "" "" "" WDS:CTL_TEXT)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddNumericControl (wnd row col width var name)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 ,width ,name "" "" "" WDS:CTL_NUMERIC)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddDateControl (wnd row col width var name)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 ,width ,name "" "" "" WDS:CTL_DATE)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddButton (wnd row col width var func name text)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 ,width ,name ,text ,func "" WDS:CTL_BUTTON)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddRadioButton (wnd row col width var name text next)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 ,width ,name ,text "" ,next WDS:CTL_RADIOBTN)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddCheckBox (wnd row col width var name text)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 ,width ,name ,text "" "" WDS:CTL_CHECKBOX)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddComboBox (wnd row col height width var name)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col ,height ,width ,name "" "" "" WDS:CTL_CHECKBOX)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddListBox (wnd row col height width var name)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col ,height ,width ,name "" "" "" WDS:CTL_LISTBOX)])
     (set! ,var c)
     ctl))

(defmacro WDS:AddStaticTextControl (wnd row col var name text)
  `(let-values ([(ctl c) (AddControl ,wnd ,row ,col 0 0 ,name ,text "" "" WDS:CTL_STATICTEXT)])
     (set! ,var c)
     ctl))

(define SetSchemeTag
  (lambda (obj ptr)
    (gSetTag obj (gNewWithLong LongInteger ptr))
    (gAutoDisposeTag obj)
    obj))

(define GetSchemeTag
  (lambda (obj)
    (Deref-Pointer (gLongValue (gGetTag obj)))))


