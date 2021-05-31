(defconstant DATA-DIR "/home/j/Projects/Lisp/cifar-10-batches-bin/")
(defconstant DATA-FILES '("../cifar-10-batches-bin/data_batch_1.bin" "../cifar-10-batches-bin/data_batch_2.bin" "../cifar-10-batches-bin/data_batch_3.bin" "../cifar-10-batches-bin/data_batch_4.bin" "../cifar-10-batches-bin/data_batch_5.bin"))
(defconstant TEST-FILE "/home/j/Projects/Lisp/cifar-10-batches-bin/test_batch.bin")
(defconstant IMG-WIDTH 32)
(defconstant IMG-HEIGHT 32)
(defconstant IMG-SIZE (* IMG-WIDTH IMG-HEIGHT))

(defstruct picture
  label
  red
  green
  blue
  )

(defun read-picture (stream)
  "Read a picture into a stuct"
    (let ((label (read-byte stream nil nil))
          ( red (make-array IMG-SIZE :element-type '(unsigned-byte 8)))
          ( blue (make-array IMG-SIZE :element-type '(unsigned-byte 8)))
          ( green (make-array IMG-SIZE :element-type '(unsigned-byte 8)))
          )
      (when (and
           label
           (eq (+
                (read-sequence red stream)
                (read-sequence green stream)
                (read-sequence blue stream))
               (* IMG-SIZE 3)))
           (make-picture :label label :red red :green green :blue blue)
  )))

(defun load-data ()
  "Read all training data into a list"
  (apply #'append (map 'list #'read-data-batch DATA-FILES)))

(defun load-examples ()
  "Read the test data into a list"
  (read-data-batch TEST-FILE))

(defun read-data-batch (filename)
  "Read the pictures in a file to a list of pictures"
  (with-open-file (s filename :direction :input :element-type '(unsigned-byte 8))
    (do ((data (list (read-picture s)) (cons (read-picture s) data)))
        ((eq (car data) nil) (cdr data))
  )))

(defun diff-vector (a b)
  "returns sum of absolute diffs between two lists"
  (reduce #'+ (map 'vector (lambda (x y) (abs (- x y))) a b )))

(defun diff-picture (a b)
  "returns sum of absolute diffs between two pictures"
  (check-type a picture)
  (check-type b picture)
  (+
   (diff-vector (picture-red a) (picture-red b))
   (diff-vector (picture-green a) (picture-green b))
   (diff-vector (picture-blue a) (picture-blue b))
   ))

(defun classify (pic data)
  (check-type pic picture)
  (check-type data list)
  "take a picture and a list of training pictures, get the picture with least difference"
  (let* (( scored (mapcar (lambda (x) (list x (diff-picture pic x))) data))
        ( guess (caar (sort scored (lambda (x y) (< (cadr x) (cadr y)))))))
    (format t "Guess label: ~D%" (picture-label guess))
  (write-picture pic "test.ppm")
  (write-picture guess "gues.ppm")
  ))

(defun write-picture (picture filename)
  "Write picture to ppm"
  (with-open-file (s filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "P3" s)
    (write-line (format nil "~D ~D" IMG-WIDTH IMG-HEIGHT) s)
    (write-line "255" s)
    (loop
      :for r :across (picture-red picture)
      :for g :across (picture-green picture)
      :for b :across (picture-blue picture)
      do (write-string (format nil "~D ~D ~D ~%" r g b) s)
    )))

(defun main ()
  "classify the first test picture"
  (let ((data (load-data))
        (example-picture (car (load-examples))))
    (classify example-picture data)
    ))
