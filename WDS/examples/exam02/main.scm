
(load "resources.scm")

(define (start)
  (let ((win nil))
    (set! win (gNewWithStr MainWindow "My Test Application"))
    (gLoadIcon win ALGOCORP_ICON)

    (sLoadMenuFromFile win "main.res" IDR_MENU1)
    (sAssociate win ID_FILE_MESSAGE  "file-message")
    (sAssociate win ID_FILE_EXIT  "file-exit")

    (gProcessMessages win)))



(define (file-message wind id)
  (gMessage wind "File Message")
  0)

(define (file-exit wind id)
  (gQuitApplication Application 0)
  0)



(start)

