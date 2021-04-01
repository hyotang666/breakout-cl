(in-package :cl-user)

(defpackage :breakout-cl
  (:use :cl)
  (:export))

(in-package :breakout-cl)

;;;; GAME-OBJECT

(defstruct game-object x y w h)

(defstruct (movable (:include game-object)) velocity)

(defstruct (blocks (:include game-object)) color)

(defstruct (normal-block (:include blocks)) (brokenp nil :type boolean))

(defstruct (solid-block (:include blocks)))

(defstruct (player (:include movable) (:constructor %make-player))
  (life 3 :type (unsigned-byte 8)))

(defstruct (ball (:include movable) (:constructor %make-ball))
  radius
  (stuckp t :type boolean))

;;;; CONSTRUCTOR

(defun make-player (win)
  (multiple-value-bind (w h)
      (sdl2:get-window-size win)
    (let ((paddle-w 100) (paddle-h 20))
      (%make-player :x (- (/ w 2) (/ paddle-w 2))
                    :y (- h paddle-h)
                    :w paddle-w
                    :h paddle-h
                    :velocity 500))))

(defparameter *initial-velocity* (list 100 -350))

(defun make-ball (player &optional (ball (%make-ball)))
  (let ((radius 12.5))
    (with-slots (x y w h velocity (r radius) stuckp)
        ball
      (with-slots ((px x) (py y) (pw w))
          player
        (setf x (+ px (/ pw (- 2 radius)))
              y (+ py (* (- radius) 2))
              w (* radius 2)
              h (* radius 2)
              velocity (apply #'3d-vectors:vec2 *initial-velocity*)
              r radius
              stuckp t))))
  ball)

;;;; IMAGES

(defparameter *images* (make-hash-table :test #'equal))

;;;; ENSURE-LOAD

(defmacro efiletype-case (pathname &body clause*)
  ;; Trivial syntax check.
  (assert (every (lambda (clause) (typep clause '(cons cons *))) clause*))
  (let ((type (gensym "FILETYPE")))
    `(let ((,type (pathname-type ,pathname)))
       (cond
         ,@(mapcar
             (lambda (clause)
               `((find ,type ',(car clause) :test #'string-equal)
                 ,@(cdr clause)))
             clause*)
         (t
          (error "Not supported file type ~S. ~S" ,type
                 ',(loop :for c :in clause*
                         :append (car c))))))))

(defun ensure-load (url)
  (let* ((filename (subseq url (1+ (position #\/ url :from-end t))))
         (pathname
          (ensure-directories-exist
            (merge-pathnames (format nil "img/~A" filename)
                             (asdf:system-source-directory
                               (asdf:find-system :breakout-cl))))))
    (unless (probe-file pathname)
      (uiop:format! *trace-output* "~&Downloading ~S" filename)
      (dex:fetch url pathname))
    (efiletype-case pathname
      ((png) (opticl:read-png-file pathname))
      ((jpg jpeg) (opticl:read-jpeg-file pathname)))))

(let ((urls (make-hash-table)))
  (flet ((def (name url)
           (setf (gethash name urls) url)))
    (def :face "https://learnopengl.com/img/textures/awesomeface.png")
    (def :block "https://learnopengl.com/img/in-practice/breakout/textures/block.png")
    (def :block-solid "https://learnopengl.com/img/in-practice/breakout/textures/block_solid.png")
    (def :background "https://learnopengl.com/img/in-practice/breakout/textures/background.jpg")
    (def :paddle "https://learnopengl.com/img/in-practice/breakout/textures/paddle.png"))
  (defun ensure-image (name)
    (or (gethash name *images*)
        (setf (gethash name *images*)
                (ensure-load
                  (or (gethash name urls)
                      (error "Unknown image file name. ~S ~S" name
                             (alexandria:hash-table-keys urls))))))))

(let ((pathnames (make-hash-table)))
  (flet ((def (name url)
           (let ((pathname
                  (ensure-directories-exist
                    (merge-pathnames
                      (format nil "sounds/~A"
                              (subseq url (1+ (position #\/ url :from-end t))))
                      (asdf:system-source-directory
                        (asdf:find-system :breakout-cl))))))
             (unless (probe-file pathname)
               (uiop:format! *trace-output* "~&Downloading ~S" url)
               (dex:fetch url pathname))
             (setf (gethash name pathnames) pathname))))
    (def :breakout "https://learnopengl.com/audio/in-practice/breakout/breakout.mp3")
    (def :break "https://learnopengl.com/audio/in-practice/breakout/bleep.mp3")
    (def :solid "https://learnopengl.com/audio/in-practice/breakout/solid.wav")
    (def :bounce "https://learnopengl.com/audio/in-practice/breakout/bleep.wav")
    (defun ensure-sound (name)
      (or (gethash name pathnames)
          (error "Unknown file name ~S: ~S" name
                 (alexandria:hash-table-keys pathnames))))))

(defmacro with-harmony ((var) &body body)
  `(symbol-macrolet ((,var org.shirakumo.fraf.harmony:*server*))
     (org.shirakumo.fraf.harmony:maybe-start-simple-server)
     (unwind-protect (progn ,@body) (org.shirakumo.fraf.harmony:stop ,var))))

(defun play (name &key repeat (mixer :effect))
  (org.shirakumo.fraf.harmony:play (ensure-sound name)
                                   :name name
                                   :mixer mixer
                                   :if-exists :restart
                                   :repeat repeat))

;;;; SHADER

(fude-gl:defshader splite 330 (fude-gl:xy fude-gl:st)
  (:vertex ((|texCoord| :vec2) &uniform (model :mat4) (projection :mat4))
    "texCoord = st;"
    "gl_Position = projection * model * vec4(xy, 0.0, 1.0);")
  (:fragment ((color :vec4) &uniform (image :|sampler2D|)
              (|spliteColor| :vec3))
    "color = vec4(spliteColor, 1.0) * texture(image, texCoord);"))

;;;; HELPERS

(defun model-matrix (x y w h &optional (rotate 0))
  (3d-matrices:nmscale
    (3d-matrices:nmtranslate
      (3d-matrices:nmrotate
        (3d-matrices:nmtranslate
          (3d-matrices:mtranslation (3d-vectors:vec x y 0))
          (3d-vectors:vec (* 0.5 w) (* 0.5 h) 0))
        3d-vectors:+vz+ (fude-gl:radians rotate))
      (3d-vectors:vec (* -0.5 w) (* -0.5 h) 0))
    (3d-vectors:vec w h 1)))

(defun parse-level (string)
  (let (h)
    (multiple-value-bind (data width)
        (uiop:while-collecting (data width)
          (with-input-from-string (in string)
            (loop :for line = (read-line in nil)
                  :while line
                  :do (with-input-from-string (in line)
                        (loop :for exp = (read in nil)
                              :while exp
                              :collect exp :into exps
                              :count exp :into width
                              :finally (data exps)
                                       (width width)))
                  :count line :into height
                  :finally (setf h height))))
      (assert (apply #'= width))
      (values data (car width) h))))

(defun init-level (data w h screen-w screen-h)
  (let ((array (make-array (list w h) :initial-element nil))
        (unit-width (/ screen-w w))
        (unit-height (/ screen-h h)))
    (flet ((color (elt)
             (case elt
               (1 (3d-vectors:vec 0.8 0.8 0.7))
               (2 (3d-vectors:vec 0.2 0.6 1.0))
               (3 (3d-vectors:vec 0.0 0.7 0.0))
               (4 (3d-vectors:vec 0.8 0.8 0.4))
               (5 (3d-vectors:vec 1.0 0.5 0.0))
               (otherwise (3d-vectors:vec 1.0 1.0 1.0)))))
      (loop :for line :in data
            :for y :upfrom 0
            :do (loop :for elt :in line
                      :for x :upfrom 0
                      :unless (zerop elt)
                        :do (setf (aref array x y)
                                    (funcall
                                      (if (= 1 elt)
                                          #'make-solid-block
                                          #'make-normal-block)
                                      :x (* x unit-width)
                                      :y (* y unit-height)
                                      :w unit-width
                                      :h unit-height
                                      :color (color elt)))))
      array)))

(defun level (source win)
  (multiple-value-call #'init-level
    (parse-level source)
    (multiple-value-bind (w h) (sdl2:get-window-size win) (values w (/ h 2)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun <keypress-pred> (key)
    (etypecase key
      ((cons (eql or)) `(or ,@(mapcar #'<keypress-pred> (cdr key))))
      ((cons (eql and)) `(and ,@(mapcar #'<keypress-pred> (cdr key))))
      (atom
       `(sdl2:keyboard-state-p
          ,(sdl2:scancode-key-to-value
             (intern (format nil "SCANCODE-~A" key) :keyword)))))))

(defmacro keypress-case (&body clause+)
  `(cond
     ,@(mapcar
         (lambda (clause)
           (if (eq 'otherwise (car clause))
               `(t ,@(cdr clause))
               `(,(<keypress-pred> (car clause)) ,@(cdr clause))))
         clause+)))

(defgeneric move (subject dt width &key)
  (:method ((player player) (dt float) (width integer) &key ball)
    (with-slots (x w velocity)
        player
      (keypress-case
        (:left
         (let ((new (max 0 (- x (* velocity dt)))))
           (setf x new)
           (when (ball-stuckp ball)
             (setf (ball-x ball) new))))
        (:right
         (let ((new (min (- width w) (+ x (* velocity dt)))))
           (setf x new)
           (when (ball-stuckp ball)
             (setf (ball-x ball) new))))
        (:space (setf (ball-stuckp ball) nil)))))
  (:method ((ball ball) (dt float) (width integer) &key
            (player (alexandria:required-argument :player))
            (win (alexandria:required-argument :win)))
    (with-slots (x y w velocity stuckp)
        ball
      (unless stuckp
        (let ((new
               (3d-vectors:v+ (3d-vectors:vec2 x y)
                              (3d-vectors:v* dt velocity))))
          (cond
            ((<= (3d-vectors:vx new) 0)
             (setf (3d-vectors:vx velocity) (- (3d-vectors:vx velocity))
                   (3d-vectors:vx new) 0))
            ((<= width (+ (3d-vectors:vx new) w))
             (setf (3d-vectors:vx velocity) (- (3d-vectors:vx velocity))
                   (3d-vectors:vx new) (- width w)))
            ((<= (3d-vectors:vy new) 0)
             (setf (3d-vectors:vy velocity) (- (3d-vectors:vy velocity))
                   (3d-vectors:vy new) 0)))
          (setf x (3d-vectors:vx new)
                y (3d-vectors:vy new)))
        (when (< (nth-value 1 (sdl2:get-window-size win)) y)
          (decf (player-life player))
          (make-ball player ball)))))
  (:method ((ball ball) (dt float) (width integer) &key)
    (with-slots (x y)
        ball
      (gl:viewport -100 0 800 600))))

(defun ortho (win)
  (multiple-value-bind (w h)
      (sdl2:get-window-size win)
    (3d-matrices:mortho 0 w h 0 -1 1)))

(defgeneric collidep (subject object)
  (:method ((circle ball) (rect game-object))
    (with-slots (radius x y)
        circle
      (with-slots (w h (rect-x x) (rect-y y))
          rect
        (let* ((center (3d-vectors:vec (+ x radius) (+ y radius)))
               (aabb-half-extents (3d-vectors:vec (/ w 2) (/ h 2)))
               (aabb-center
                (3d-vectors:nv+ (3d-vectors:vec rect-x rect-y)
                                aabb-half-extents))
               (difference
                (3d-vectors:v-
                  (3d-vectors:v+ aabb-center
                                 (3d-vectors:vclamp
                                   (3d-vectors:v- center aabb-center)
                                   (3d-vectors:v- aabb-half-extents)
                                   aabb-half-extents))
                  center)))
          (if (< (3d-vectors:vlength difference) radius)
              (values t (vector-direction difference) difference)
              (values nil :up (3d-vectors:vec 0 0)))))))
  (:method ((ball ball) (block normal-block))
    (unless (normal-block-brokenp block)
      (call-next-method)))
  (:method ((subject ball) (object null)) ; Do nothing
    ))

(defgeneric response (subject object &key)
  (:method :before ((ball ball) (player player) &key) (play :bounce))
  (:method ((ball ball) (player player) &key)
    (with-slots (x w)
        player
      (let* ((center-board (+ x (/ w 2)))
             (distance (- (+ (ball-x ball) (ball-radius ball)) center-board))
             (percentage (/ distance (/ w 2)))
             (strength 2)
             (length (3d-vectors:vlength (ball-velocity ball))))
        (3d-vectors:vsetf (ball-velocity ball)
                          (* (car *initial-velocity*) percentage strength)
                          (* -1
                             (abs (- (3d-vectors:vy (ball-velocity ball))))))
        (setf (ball-velocity ball)
                (3d-vectors:v* (3d-vectors:vunit (ball-velocity ball))
                               length)))))
  (:method ((ball ball) (block blocks) &key direction difference)
    (if (find direction '(:left :right))
        (let ((penetration
               (- (ball-radius ball) (abs (3d-vectors:vx difference)))))
          (setf (3d-vectors:vx (ball-velocity ball))
                  (- (3d-vectors:vx (ball-velocity ball))))
          (if (eq :left direction)
              (incf (3d-vectors:vx (ball-velocity ball)) penetration)
              (decf (3d-vectors:vx (ball-velocity ball)) penetration)))
        (let ((penetration
               (- (ball-radius ball) (abs (3d-vectors:vy difference)))))
          (setf (3d-vectors:vy (ball-velocity ball))
                  (- (3d-vectors:vy (ball-velocity ball))))
          (if (eq :up direction)
              (incf (3d-vectors:vx (ball-velocity ball)) penetration)
              (decf (3d-vectors:vx (ball-velocity ball)) penetration)))))
  (:method :before ((ball ball) (block normal-block) &key) (play :break))
  (:method ((ball ball) (block normal-block) &key)
    (setf (normal-block-brokenp block) t)
    (call-next-method))
  (:method :before ((ball ball) (block solid-block) &key) (play :solid))
  (:method ((ball ball) (level array) &key)
    (dotimes (x (array-total-size level))
      (let ((elt (row-major-aref level x)))
        (multiple-value-bind (collidep direction difference)
            (collidep ball elt)
          (when collidep
            (response ball elt
                      :direction direction
                      :difference difference)))))))

(defun check-collision (ball player level)
  (unless (ball-stuckp ball)
    (if (collidep ball player)
        (response ball player)
        (response ball level))))

(let ((compass (make-hash-table)))
  (flet ((def (k v)
           (setf (gethash k compass) v)))
    (def :up (3d-vectors:vec 0 1))
    (def :right (3d-vectors:vec 1 0))
    (def :down (3d-vectors:vec 0 -1))
    (def :left (3d-vectors:vec -1 0)))
  (defun vector-direction (target)
    (loop :with normalized = (3d-vectors:vunit target)
          :with max = 0
          :with best
          :for direction :being :each :hash-key :of compass :using
               (:hash-value v)
          :for dot-product = (3d-vectors:v. normalized v)
          :if (< max dot-product)
            :do (setf max dot-product
                      best direction)
          :finally (return best))))

;;;; MAIN

(defparameter *level1*
  "5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
4 4 4 4 4 0 0 0 0 0 4 4 4 4 4
4 1 4 1 4 0 0 1 0 0 4 1 4 1 4
3 3 3 3 3 0 0 0 0 0 3 3 3 3 3
3 3 1 3 3 3 3 3 3 3 3 3 1 3 3
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2")

(defgeneric draw (object texture &key)
  (:method (model-mat texture &key)
    "Default method to draw."
    (fude-gl:in-texture texture)
    (fude-gl:send model-mat 'splite :uniform "model")
    (fude-gl:draw 'splite))
  (:method ((bg (eql :background)) texture &key
            (win (alexandria:required-argument :win)))
    (draw (multiple-value-call #'model-matrix 0 0 (sdl2:get-window-size win))
          texture))
  (:method :before ((o blocks) texture &key)
    (fude-gl:send (blocks-color o) 'splite :uniform "spliteColor"))
  (:method ((o game-object) texture &key)
    (with-slots (x y w h)
        o
      (call-next-method (model-matrix x y w h) texture)))
  (:method ((level array) (textures list) &key)
    (dotimes
        (i (array-total-size level)
           (gl:uniformf (fude-gl:uniform "spliteColor" 'splite) 1 1 1))
      (let ((o (row-major-aref level i)))
        (draw o (getf textures (type-of o))))))
  (:method ((block normal-block) textures &key)
    (unless (normal-block-brokenp block)
      (call-next-method)))
  (:method ((n null) texture &key &allow-other-keys)) ; Do nothing.
  )

(define-condition sequence-transition () ((next :reader next :initarg :next)))

(defmacro sequence-handler-bind ((var init-form) &body body)
  `(let ((,var ,init-form))
     (tagbody
      :top
       (handler-bind ((sequence-transition
                       (lambda (condition)
                         (setf ,var (next condition))
                         (go :top))))
         ,@body))))

(fude-gl:deftexture background :texture-2d
  (fude-gl:tex-image-2d (ensure-image :background)))

(fude-gl:deftexture block :texture-2d
  (fude-gl:tex-image-2d (ensure-image :block)))

(fude-gl:deftexture block-solid :texture-2d
  (fude-gl:tex-image-2d (ensure-image :block-solid)))

(fude-gl:deftexture paddle :texture-2d
  (fude-gl:tex-image-2d (ensure-image :paddle)))

(fude-gl:deftexture ball :texture-2d
  (fude-gl:tex-image-2d (ensure-image :face)))

(fude-gl:defvertices splite
    (concatenate '(array single-float (*))
                 (make-instance 'splite :x 0.0 :y 1.0 :s 0.0 :t 1.0)
                 (make-instance 'splite :x 1.0 :y 0.0 :s 1.0 :t 0.0)
                 (make-instance 'splite :x 0.0 :y 0.0 :s 0.0 :t 0.0)
                 (make-instance 'splite :x 0.0 :y 1.0 :s 0.0 :t 1.0)
                 (make-instance 'splite :x 1.0 :y 1.0 :s 1.0 :t 1.0)
                 (make-instance 'splite :x 1.0 :y 0.0 :s 1.0 :t 0.0)))

(defun main ()
  (uiop:nest
    (with-harmony (server)
      (play :breakout :repeat t))
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :x 100
                           :y 100
                           :w 800
                           :h 600
                           :title "Breakout-CL"))
    (sdl2:with-gl-context (context win)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))
    (fude-gl:with-shader ()
      (fude-gl:in-vertices 'splite)
      (fude-gl:send
        (multiple-value-bind (w h)
            (sdl2:get-window-size win)
          (3d-matrices:mortho 0 w 0 h -1 1))
        'splite
        :uniform "projection"))
    (fude-gl:with-text-renderer (render-text :size 64 :win win))
    (sequence-handler-bind (scene #'entry-point)
      (funcall scene win #'render-text))))

(defun entry-point (win text-renderer)
  (uiop:nest
    (fude-gl:with-shader () (fude-gl:in-vertices 'splite))
    (fude-gl:with-textures ()
      (fude-gl:send (ortho win) 'splite :uniform "projection")
      (gl:uniformf (fude-gl:uniform "spliteColor" 'splite) 1 1 1))
    (let* ((title "Breakout!")
           (bbox
            (vecto:string-bounding-box title (* 2 fude-gl:*font-size*)
                                       (fude-gl:font-loader "Ubuntu-M")))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:keydown (:keysym keysym)
        (case (sdl2:scancode keysym)
          (otherwise (signal 'sequence-transition :next #'game)))))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:in-vertices 'splite)
      (fude-gl:in-vertex-array (fude-gl:vertex-array 'splite))
      (fude-gl:connect 'splite "image" 'background)
      (draw :background 'background :win win)
      (funcall text-renderer title
               :x :center
               :y (- (* (floor (nth-value 1 (sdl2:get-window-size win)) 4) 3)
                     (floor (- (zpb-ttf:ymax bbox) (zpb-ttf:ymin bbox)) 2)))
      (funcall text-renderer "Push any key to start."
               :x :center
               :y :center
               :scale 0.25))))

(defun game (win text-renderer)
  (uiop:nest
    (fude-gl:with-shader ())
    (fude-gl:with-textures ())
    (let* ((level (level *level1* win))
           (player (make-player win))
           (ball (make-ball player)))
      (fude-gl:in-vertices 'splite)
      (fude-gl:send (ortho win) 'splite :uniform "projection"))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (move player 0.025 (sdl2:get-window-size win) :ball ball)
      (move ball 0.025 (sdl2:get-window-size win) :player player :win win)
      (check-collision ball player level)
      (fude-gl:in-vertices 'splite)
      (fude-gl:in-vertex-array (fude-gl:vertex-array 'splite))
      (draw :background 'background :win win)
      (draw level '(normal-block block solid-block block-solid))
      (draw ball 'ball)
      (draw player 'paddle)
      (funcall text-renderer (format nil "Lives: ~S" (player-life player))
               :y (- 600 (floor fude-gl:*font-size* 2))
               :scale 0.5)
      (when (zerop (player-life player))
        (signal 'sequence-transition :next #'game-over)))))

(defun game-over (win text-renderer)
  (uiop:nest
    (let* ((title "Game over!")
           (bbox
            (vecto:string-bounding-box title (* 2 fude-gl:*font-size*)
                                       (fude-gl:font-loader "Ubuntu-M")))))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t)
      (:keydown (:keysym keysym)
        (case (sdl2:scancode keysym)
          (otherwise (signal 'sequence-transition :next #'entry-point)))))
    (:idle nil
     (funcall text-renderer title
              :x :center
              :y (- (* (floor (nth-value 1 (sdl2:get-window-size win)) 4) 3)
                    (floor (- (zpb-ttf:ymax bbox) (zpb-ttf:ymin bbox)) 2)))
     (funcall text-renderer "Push any key." :x :center :y :center :scale 0.25)
     (sdl2:gl-swap-window win) (sleep 2))))