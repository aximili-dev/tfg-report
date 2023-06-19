(defun main ()
  (with-graphics (graphics "02 Meshes"
		  :hide-cursor t
		  :window-width 800
		  :window-height 600)
    (let* ((camera (make-instance 'fps-camera :sensitivity 0.1 :speed 10.0 :pos (vec 0 0 10)))
	   (model-shader (load-shader-from-disk #P"./shaders/model.vert" #P"./shaders/model.frag"))
	   (cube-model (load-model #P"./models/cube.obj" :diffuse-path #P"./container.png" :specular-path #P"./container.specular.png"))
	   (cube-entity (make-instance 'entity :model cube-model :shader model-shader :transform (transform)))
	   (d20-model (load-model #P"./models/Dice_d20.obj" :diffuse-path #P"./d20white.png" :specular-path #P"./d20white.png"))
	   (d20-entity (make-instance 'entity :model d20-model :shader model-shader :transform (transform (vec 0 0 0) (vec 3 3 3)))))
      (graphics-setup-fps-camera graphics camera)
      (flet ((tick (time dt)
	       (process-tick camera time dt (graphics-keys graphics)))
	     (render (current-frame fps &key debugp)
	       (gl:clear-color 0.21 0.36 0.43 1.0)
	       (gl:clear :color-buffer :depth-buffer)
	       
	       (setup-lights model-shader)

	       (if debugp
		   (gl:polygon-mode :front-and-back :line)
		   (gl:polygon-mode :front-and-back :fill))

	       (push (format nil "FPS: ~3,3f" fps) (graphics-debug-text graphics))))
	(graphics-run graphics #'render #'tick)))))

(defparameter *light-positions* (list (vec  0.7  0.2  2.0)
				      (vec  2.3 -3.3 -4.0)
				      (vec -4.0  2.0 -12.0)
				      (vec  0.0  0.0 -3.0)))










(defun setup-lights (shader)
  (shader-use shader)
  
  (let ((i 0))
    (macrolet ((set-light-prop (i prop &rest values)
			       `(let ((loc (format nil "pointLights[~d].~a" ,i ,prop)))
				  (shader-set-uniform shader loc ,@values))))
	      (dolist (light-pos *light-positions*)
		(with-vec (x y z) light-pos
			  (set-light-prop i "position" x y z)
			  (set-light-prop i "constant" 1.0)
			  (set-light-prop i "linear" 0.09)
			  (set-light-prop i "quadratic" 0.032)
			  (set-light-prop i "ambient" 0.2 0.2 0.2)
			  (set-light-prop i "diffuse" 0.5 0.5 0.5)
			  (set-light-prop i "specular" 1.0 1.0 1.0))
		(incf i)))))
  
(defun render-entities (d20-entity cube-entity)
  (render-entity d20-entity
		 (graphics-v-width graphics)
		 (graphics-v-height graphics)
		 camera
		 :debugp debugp)

  (dotimes (i 12)
    (let* ((angle (* i (/ 360 12)))
	   (angle (* angle (/ pi 180)))
	   (pos (vrot (vec 0 0 6) +vy+ angle)))
      (with-slots (transform) cube-entity
	(setf transform (transform pos
				   (vec 1 1 1)
				   (qfrom-angle +vy+ angle)))
	
	(render-entity cube-entity
		       (graphics-v-width graphics)
		       (graphics-v-height graphics)
		       camera
		       :debugp debugp))))
