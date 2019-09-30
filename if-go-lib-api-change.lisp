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
  (let ((rest (cadr (cl-ppcre:split "\\s+" line :limit 2)))) ; clean "func"
    (print rest)
    (if (char= #\( (elt rest 0))
        ;; make method
        (let (which-type
              name
              args
              return-value
              temp)
          (print temp)
          (setf temp (cl-ppcre:split "(?<=\\))\\s+" rest :limit 2))
          (print temp)
          ;; temp is ("(a int)", "funcname() return {")
          (setf which-type (value-and-types (car temp)))
          
          (setf temp (cadr temp))
          (setf name (cl-ppcre:scan-to-strings "\\b\\w+" temp))
          (print temp)
          
          (setf temp (cadr (cl-ppcre:split "\\b\\w+" temp :limit 2)))
          ;;temp is "(a int, b int) return {", function args and return type now
          (setf temp (cl-ppcre:split "(?<=\\))\\s+" temp  :limit 2))
          ;;temp is list now
          (setf args (value-and-types (car temp)))
          (setf temp (cadr temp))
          ;;temp is only return value now
          (setf temp (car (cl-ppcre:split "\\s*{" temp)))
          (print temp)
          ;;(make-go-method)
          )
        ;; make function
        ;;(make-go-function)
        )))

(defun value-and-types (wrap-code)
  "wrap-code is (a int, b int, c ...interface{})"
  ""
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
