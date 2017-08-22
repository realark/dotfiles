(setf swank:*globally-redirect-io* T)


;; Incudine settings
;; Avoid problems in realtime because the default value is :SPAWN (each
;; request in a separate thread)
;;(setf swank:*communication-style* :sigio)
