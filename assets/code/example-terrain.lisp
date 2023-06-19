(defparameter *vao* nil)
(defparameter *vbo* nil)
(defparameter *ebo* nil)
(defparameter *num-indices* nil)

(defun main ()
  (with-graphics (graphics "03 Terrain"
			   :hide-cursor t
			   :window-width 800
			   :window-height 600)
    (let* ((camera (make-instance 'fps-camera :sensitivity 0.1 :speed 10.0 :pos (vec 0 0 10)))
	   (points (make-array 0 :adjustable t :fill-pointer 0))
	   (indices (make-array 0 :adjustable t :fill-pointer 0))
	   (shader (load-shader-from-disk #P"./colored.vert" #P"./colored.frag"))
	   (width 150))

      (create-mesh points indices)
      (setup-gl-mesh points indices)

      (graphics-setup-fps-camera graphics camera)

      (flet ((tick (time dt)
	       (process-tick camera time dt (graphics-keys graphics)))
	     (render ()
	       (render-mesh shader camera)))
        (graphics-run graphics #'render #'tick)))))

(defun create-mesh-vertices (points width)
  (dotimes (x width)
    (dotimes (z width)
      (let* ((seeds '(1 2 3 4 5 6))
	     (wpos (vec (/ x 10) (/ y 10) (/ z 10)))
	     (value (+ (* (perlin (v* wpos 0.2) :seed (nth 0 seeds)) 5)
		       (* (perlin (v* wpos 0.4) :seed (nth 1 seeds)) 2)
		       (perlin     wpos      :seed (nth 2 seeds))
		       (/ (perlin (v* wpos 2)   :seed (nth 3 seeds)) 2)
		       (/ (perlin (v* wpos 4)   :seed (nth 4 seeds)) 4)
		       (/ (perlin (v* wpos 8)   :seed (nth 5 seeds)) 8)))
	     (value (if (< value 0)
			(/ value 4)
		      value))
	     (value (if (< value -1)
			-1.0
		      value))
	     (color (height-to-color value min max)))
	(when t
	  (vector-push-extend (vx wpos) points)
	  (vector-push-extend value points)
	  (vector-push-extend (vz wpos) points)
	  (vector-push-extend (vx color) points)
	  (vector-push-extend (vy color) points)
	  (vector-push-extend (vz color) points))))))

(defun height-to-color (y min max)
  (let* ((diff (- max min))
	 (dist (- y min))
	 (y (float (/ dist diff))))
    (vec y y y)))

(defun create-mesh-indices (indices width)
  (dotimes (z (1- width))
    (dotimes (x width)
      (vector-push-extend (+ (* z w) x) indices)
      (vector-push-extend (+ (* z w) x w) indices))

    (vector-push-extend #xFFFFFFFF indices))

  (setf *num-indices* (length indices)))

(defun setup-gl-mesh (points indices)
  (setf *vao* (gl:gen-vertex-array))
  (setf *vbo* (gl:gen-buffer))
  (setf *ebo* (gl:gen-buffer))
  
  (gl:bind-vertex-array *vao*)
  
  (let ((gl-arr (make-gl-array points :float)))
    (gl:bind-buffer :array-buffer *vbo*)
    (gl:buffer-data :array-buffer :static-draw gl-arr)
    (gl:free-gl-array gl-arr))
  
  (let ((gl-arr (make-gl-array indices :unsigned-int)))
    (gl:bind-buffer :element-array-buffer *ebo*)
    (gl:buffer-data :element-array-buffer :static-draw gl-arr)
    (gl:free-gl-array gl-arr))
  
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float :false (* 6 4) 0)
  
  (gl:enable-vertex-attrib-array 1)
  (gl:vertex-attrib-pointer 1 3 :float :false (* 6 4) (* 3 4))
  
  (gl:bind-vertex-array 0))
    
(defun render-mesh (shader camera debugp)
  (shader-use shader)

  (let ((view (view-matrix camera))
	(proj (mperspective 45 (/ v-width v-height) 0.1 1000)))
    (shader-set-uniform shader "view" (marr view))
    (shader-set-uniform shader "projection" (marr proj))
    (shader-set-uniform shader "model" (marr (meye 4))))

  (when debugp
    (gl:polygon-mode :front-and-back :line))
  
  (cl-opengl-bindings:primitive-restart-index #xFFFFFFFF)

  (gl:bind-vertex-array *vao*)
  (gl:draw-elements :triangle-strip
		    (gl:make-null-gl-array :unsigned-int)
		    :count 44849)
  (gl:polygon-mode :front-and-back :fill)
  (gl:bind-vertex-array 0))
  
