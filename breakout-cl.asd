; vim: ft=lisp et
(in-package :asdf)
(defsystem "breakout-cl"
  :version
  "0.0.6"
  :depends-on
  (
   "fude-gl"            ; Opengl utilities.
   "sdl2"               ; Window manager.
   "harmony"            ; Audio processing server.
   "dexador"            ; Http client.
   "opticl"             ; Image file loader.
   "3d-matrices"        ; Matrix-operations.
   )
  :pathname
  "src/"
  :components
  ((:file "breakout-cl")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "breakout-cl").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "breakout-cl"))))
  (append (call-next-method) '((test-op "breakout-cl.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "breakout-cl")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "breakout-cl"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
