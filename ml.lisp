(defstruct picture
  label
  red
  green
  blue
)

(defun read-picture (f)
  "Read a picture into a stuct"
  (with-open-file (s f :direction :input :element-type '(unsigned-byte 8))
    (let ((label (read-byte s))
          ( red (make-array 1024 :element-type '(unsigned-byte)))
          ( blue (make-array 1024 :element-type '(unsigned-byte)))
          ( green (make-array 1024 :element-type '(unsigned-byte)))
          )
          (read-sequence red s)
           (read-sequence green s)
           (read-sequence blue s)
           (make-picture :label label :red red :green green :blue blue)
      )
    )
  )

(defun write-picture (picture filename)
  "Write picture to ppm"
  (with-open-file (s filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "P3" s)
    (write-line "32 32" s)
    (write-line "255" s)
    (loop
      :for r :across (picture-red picture)
      :for g :across (picture-green picture)
      :for b :across (picture-blue picture)
      do (write-string (format nil "~D ~D ~D ~%" r g b) s)
    )))
