
(load "resources.scm")


(define (start)
  (let ((win nil))

    (set! win (gNewWithStr MainWindow "My Test Application"))
    
    (gLoadIcon win ALGOCORP_ICON)

    (sLoadMenuFromFile win "main.res" IDR_MENU1)
    (sAssociate win ID_FILE_MESSAGE  "file-message")
    (sAssociate win ID_FILE_EXIT  "file-exit")
    (sAssociate win ID_FILE_DIALOG  "file-dialog")

    (gProcessMessages win)))


(define (file-dialog wind id)
  (let* ((dlg nil)
	 (r 0))
    (set! dlg (gNewDialogFromFile ModalDialog "main.res" DL1 wind))
    (init-controls dlg)
    (set! r (gPerform dlg))
    (if (neq? r 0)
	(display-values wind dlg))
    (gDispose dlg))
  0)

(define (display-values wind dlg)
  (vPrintf wind "First Name = ~a~n" (gCtlStringValue dlg FIELD_FIRST_NAME))
  (vPrintf wind "Last Name = ~a~n" (gCtlStringValue dlg FIELD_LAST_NAME)))

(define (init-controls dlg)
  (let ((ctl nil))

    (set! ctl (gAddControl dlg TextControl FIELD_FIRST_NAME))
    (gTextRange ctl 0 10)

    (set! ctl (gAddControl dlg TextControl FIELD_LAST_NAME))
    (gTextRange ctl 1 30)
    (gCapitalize ctl)))


(define (file-message wind id)
  (gMessage wind "File Message")
  0)

(define (file-exit wind id)
  (gQuitApplication Application 0)
  0)



(start)

