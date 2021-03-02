; vim: ft=lisp et
(in-package :asdf)
(defsystem "breakout-cl.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "breakout-cl")
  :components
  ((:file "breakout-cl"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :breakout-cl args)))