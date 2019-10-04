(eval-when (:execute :load-toplevel :compile-toplevel)
  (load "~/quicklisp/setup.lisp")
  (ql:quickload :cl-ppcre)
  (ql:quickload :hash-set)

  (defpackage #:if-go-lib-api-change
    (:use #:cl #:cl-ppcre #:hash-set))

  (in-package #:if-go-lib-api-change))

;;;;:= TODO: need pub/pravite judger
;;;;:= TODO: need to handle "a,b,c int" format in struct

(setf *print-case* :capitalize)


(defstruct go-package-file
  name)


(defstruct go-type
  name
  type
  fields
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


(defun scan-file (filepath)
  "read a file and return parse result"
  (with-open-file (s filepath)
    (scan-code s))
  )


(defun scan-code (stream)
  "return all dependencies"
  (do* ((result '())
       (str (read-line stream nil) (read-line stream nil))
       (this-line (cl-ppcre:split "\\s+|\\(" str :limit 2) (cl-ppcre:split "\\s+|\\(" str :limit 2) ))
       ((not str) (remove nil result))
    (if str
        (setf result
              (append result
                      (list
                       (cond ((string= "type" (car this-line))
                              ;; find when this code block end, merge it with first line to get whole
                              ;; code block 
                              (let ((code-block (concatenate 'string str (find-this-block stream))))
                                (with-input-from-string (ll code-block)
                                  (let* ((first-line (read-line ll))
                                         (type-is (cl-ppcre:scan-to-strings "\\w+(?=\\s*{\\s*$)" first-line))) ; first line
                                    (cond ((string= "struct" type-is)
                                           (let ((type-instance (make-go-type :name "" :type type-is :fields '())))
                                             ;; find name
                                             (setf (go-type-name type-instance)
                                                   (nth 1 (cl-ppcre:split "\\s+" first-line)))
                                             ;; read all line left
                                             (do* ((li (read-line ll nil) (read-line ll nil))
                                                   (cut-li (cl-ppcre:split "\\s+" li))
                                                   (result '()))
                                                  ((not li)
                                                   (setf (go-type-fields type-instance) result)
                                                   (return type-instance))
                                               (if (not (equal "}" li))
                                                   (setf result (append result
                                                                        (list (remove-if (lambda (x) (string= x ""))
                                                                                         cut-li))))))))
                                          ((string= "interface" type-is)
                                           (let ((type-instance (make-go-type :name "" :type type-is :fields '())))
                                             ;; find name
                                             (setf (go-type-name type-instance)
                                                   (nth 1 (cl-ppcre:split "\\s+" first-line)))
                                             ;; read all line left
                                             (do* ((li (read-line ll nil) (read-line ll nil))
                                                   (cut-li (cl-ppcre:split "\\s+" li :limit 2))
                                                   (result '()))
                                                  ((not li)
                                                   (setf (go-type-fields type-instance) result)
                                                   (return type-instance))
                                               (if (not (equal "}" li))
                                                   (setf result (append result
                                                                        (list (remove-if (lambda (x) (string= x ""))
                                                                                         cut-li)))))))
                                           )))
                                  )))
                             ((string= "func" (car this-line))
                              (give-func-declare str)
                              )
                             ((string= "package" (car this-line))
                              (make-go-package-file :name (cadr this-line)))
                             )))))))


(defun give-func-declare (line)
  "return method and function struct"
  (declare (string line))
  (let ((rest (cadr (cl-ppcre:split "\\w+\\s*(?=(\\w+|\\())" line :limit 2)))) ; clean "func"
    (if (char= #\( (elt rest 0))
        ;; make method
        (let (which-type
              name
              args
              return-value
              temp)
          (setf
           ;; update temp to (type, rest)
           temp (cl-ppcre:split "(?<=\\))\\s*" rest :limit 2)
           ;; give which-type value
           which-type (value-and-types (car temp))
           ;; update temp to rest string
           temp (cadr temp)
           ;; give method name
           name (cl-ppcre:scan-to-strings "\\b\\w+" temp)
           ;; update temp to string without method name
           temp (cadr (cl-ppcre:split "\\b\\w+\\s+" temp :limit 2))
           ;; give args value
           args (value-and-types (cl-ppcre:scan-to-strings "\\(.*?\\)" temp))
           ;; temp only have return part now
           temp (cadr (cl-ppcre:split "\\(.*?\\)\\s*" temp  :limit 2))
           return-value (value-and-types (cl-ppcre:scan-to-strings ".*(?=\s*{)" temp))
           )

          (make-go-method :type which-type :name name :args args :return-value return-value)
          )
        ;; make function
        (let (name args return-value (temp rest))
          (setf
           ;;give function name
           name (cl-ppcre:scan-to-strings "\\b\\w+" temp)
           ;; update temp to string without method name
           temp (cadr (cl-ppcre:split "\\b\\w+\\s+" temp :limit 2))
           ;; give args value
           args (value-and-types (cl-ppcre:scan-to-strings "\\(.*\\)" temp))
           ;; temp only have return part now
           temp (cadr (cl-ppcre:split "\\(.*?\\)\\s*" temp  :limit 2))
           return-value (value-and-types (cl-ppcre:scan-to-strings ".*(?=\s*{)" temp))
           )
          (make-go-function :name name :args args :return-value return-value)
          )
        )))


(defun value-and-types (wrap-code)
  "wrap-code is (a int, b int, c ...interface{})"
  (let* ((clean-parameter-code (cl-ppcre:scan-to-strings "(?<=\\().*(?=\\))" wrap-code))
         (group (cl-ppcre:split "\\s*,\\s*" clean-parameter-code)))
    (loop
       for pair in group
       for (v ty) = (cl-ppcre:split "\\s+" pair)
       collect (cons v ty))
    ))


(defun find-this-block (stream)
  "find code body block"
  (let ((stack '(#\{))
        (result '(#\linefeed)))
    (do (c)
        ((not stack) (coerce result 'string))
      ;; update c and result
      (setf c (read-char stream)
            result (append result (list c)))
      ;; judge this char
      (cond ((char= c #\}) 
             (setf stack (butlast stack)))
            ((char= c #\{)
             (setf stack (append stack (list c))))))))


;;:= need equal function
(defstruct go-package
  (name "")
  (import-packages (make-hash-set) :type hash-set) ;;:= need finish import packages
  (definations '() :type list))


(defun pickup-package (l &key update)
  (let ((gp (make-go-package)))
    (setf (go-package-definations gp)
          (loop
             for ele in l
             if (go-package-file-p ele)
             do (setf (go-package-name gp) (go-package-file-name ele))
             else collect ele))
    gp))
