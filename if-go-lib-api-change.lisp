(ql:quickload :cl-ppcre)
(setf )

(defun scan-file (filepath)
  "read a file and return parse result"
  )

(defun scan-string (stream)
  (let* ((str (read-line stream))
         (this-line (cl-ppcre:split "\\s+" str :limit 2))))
  (cond ((string= "type" (car this-line))
         (concatenate 'string str (find-this-block stream))
         )
        ((equal "func" (car this-line))
         (give-func-declare str)
         )
        )
  )

(defstruct go-function
  name
  args
  return-value)

(defstruct go-method
  type
  name
  args
  return-value)

(defun give-func-declare (line)
  (let ((rest (cdr (cl-ppcre:split "\\s+" line :limit 2)))) ; clean "func"
    (if (char= #\( (elt (car rest) 0))
        ;; make method
        (let (which-type
              name
              args
              return-value
              temp)
          (setf temp (cl-ppcre:split "(?<=\\))\\s+" rest :limit 2))
          (setf which-type (value-and-types (car temp)))

          (setf temp (cl-ppcre:split "(?<=\\))\\s+" (cadr temp) :limit 2))
          (make-method))
        ;; make function
        
        )))

(defun value-and-types (wrap-code)
  "wrap-code is (a int, b int, c ...interface{})"
  
  )


(defun find-this-block (stream)
  "find code body block"
  (let ((stack '(#\{))
        (result '()))
    (do (c)
        ((not stack) (return (coerce result 'string)))
      ;; update c and result
      (setf c (read-char stream)
            result (append result (list c)))
      ;; judge this char
      (cond ((char= c #\}) 
             (setf stack (butlast stack)))
            ((char= c #\{)
             (setf stack (append stack (list c))))))))
