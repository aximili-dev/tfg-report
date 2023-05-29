;; A mesh represented as a list of points and a list of indices
(defparameter *vertices* #(-0.5 -0.5 0.0 ...))
(defparameter *indices*  #(0 1 2 2 3 4 5 ...))

;; Shader source code
(defparameter *vs-source* "#version 330 core\n ...")
(defparameter *fs-source* "#version 330 core\n ...")

;; Needed for interacting with OS windows
(glfw:with-init
 ;; Creates a window and the OpenGL context
 (let* ((window (glfw:create-window :width 800
				    :height 600
				    :title "Example"
				    :context-version-major 3
				    :context-version-minor 3
				    :opengl-profile
				    :opengl-core-profile))
	;; Create some OpenGL objects to store data
	(vbo (gl:gen-buffer))
	(ebo (gl:gen-buffer))
	(vao (gl:gen-vertex-array))

	;; Create shader objects
	(vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader))
	(shader-program (gl:create-program)))

   ;; Bind and compile shader source code
   (gl:shader-source vs *vs-source*)
   (gl:compile-shader vs)

   (gl:shader-source fs *fs-source*)
   (gl:compile-shader fs)

   ;; Link compiled shaders to shader program
   (gl:attach-shader program vs)
   (gl:attach-shader program fs)
   (gl:link-program program)

   ;; Bind our Vertex Array Object so we can specify our mesh layout
   (gl:bind-vertex-array vao)

   ;; Bind the buffers we created to the vao, specifying their type
   (gl:bind-buffer :array-buffer vbo)
   (gl:bind-buffer :element-array-buffer ebo)

   ;; Copy the vertex and index data into the buffers
   (gl:buffer-data :array-buffer :static-draw (array-to-gl-array *vertices*))
   (gl:buffer-data :element-array-buffer :static-draw (array-to-gl-array *indices*))

   ;; Specify Vertex Attribute Pointers
   (gl:vertex-attrib-pointer 0 3 :float :false (* 3 4) 0)
   (gl:enable-vertex-attrib-array 0)

   (loop until (glfw:window-should-close-p window)
	 do (progn
	      ;; Set the shader program we are using
	      (gl:use-program shader-program)

	      ;; Bind the Vertex Array Object for our mesh
	      (gl:bind-vertex-array vao)

	      ;; Draw triangles with the current state
	      (gl:draw-elements :triangles
				(gl:make-null-gl-array :unsigned-int)
				:count (length *indices*))

	      ;; Unbind our vertex array
	      (gl:bind-vertex-array 0)

	      ;; Tell the OS to draw the framebuffer to the screen
	      (glfw:swap-buffers window)
	      (glfw:poll-events)))

   (gl:delete-buffers (list vbo ebo))
   (gl:delete-vertex-arrays (list vao))
   (gl:delete-program shader-program)
   (gl:delete-shaders vs fs)))
