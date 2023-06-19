(defun main ()
  (with-graphics (graphics "01 Triangle"
		  :hide-cursor nil
		  :window-width 800
		  :window-height 600)
    (let ((vertex-positions #(-0.5 -0.5  0.0  0.5 -0.5  0.0  0.0  0.5  0.0))
	  (vao (gl:gen-vertex-array))
	  (vbo (gl:gen-buffer))
	  (shader (load-shader-from-disk #P"./triangle.vert" #P"./triangle.frag")))

      (setup vao vbo)

      (loop while (not (glfw:window-should-close-p (graphics-window graphics)))
	    do (render vao shader graphics vertex-positions))

      (gl:delete-buffers (list vbo))
      (gl:delete-vertex-arrays (list vao))
      (shader-free shader))))

(defun setup (vao vbo)
  (gl:bind-vertex-array vao)
  
  (gl:bind-buffer :array-buffer vbo)
  
  (with-gl-array (gl-array vertex-positions :float)
		 (gl:buffer-data :array-buffer :static-draw gl-array))
  
  (gl:vertex-attrib-pointer 0 3 :float :false (* 3 4) 0)
  (gl:enable-vertex-attrib-array 0)
  
  (gl:bind-vertex-array 0))

(defun render (vao shader graphics vertex-positions)
  (gl:clear-color 0.21 0.36 0.43 1.0)
  (gl:clear :color-buffer :depth-buffer)
  
  (gl:bind-vertex-array vao)
  
  (shader-use shader)
  (gl:draw-arrays :triangles 0 (length vertex-positions))
  
  (gl:bind-vertex-array 0)
  
  (glfw:swap-buffers (graphics-window graphics))
  (glfw:poll-events))
