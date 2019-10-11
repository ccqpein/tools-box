(eval-when (:execute :load-toplevel :compile-toplevel)
  (load "~/quicklisp/setup.lisp")
  (ql:quickload :cl-ppcre)
  (ql:quickload :cl-fad)
  (ql:quickload :hash-set)

  (defpackage #:if-go-lib-api-change
    (:use #:cl #:cl-ppcre #:hash-set))
  
  (in-package #:if-go-lib-api-change))

;;;;:= TODO: need pub/pravite judger
;;;;:= TODO: need to handle "a,b,c int" format in struct

(setf *print-case* :capitalize)


(defstruct go-package-head
  "this file package"
  name
  path)


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
    (scan-code s filepath))
  )


(defun scan-code (stream &optional (filepath ""))
  "return all declares"
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
                              (make-go-package-head :name (cadr this-line) :path filepath))
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


;;:= need equal function, path is matter, and name is matter too
(defstruct go-package
  (name "")
  (path "")
  (import-packages (make-hash-set) :type hash-set) ;;:= TODO: need finish import packages
  (definations '() :type list))


(defun pickup-package (l)
  "l is all definations return from scan function. 
Return this file's info, just one file"
  (let ((gp (make-go-package)))
    (setf (go-package-definations gp)
          (loop
             for ele in l
             if (go-package-head-p ele) ;; this line is `package main` line
             do (setf (go-package-name gp) (go-package-head-name ele)
                      (go-package-path gp) (go-package-head-path ele))
             else collect ele))
    (the go-package gp))) ;; just remind myself


(defun update-go-package (source with)
  source)


(defun merge-pickup-packages (pgs)
  ;;:= TODO: update function
  (print pgs)
  (let (cache)
    (dolist (pg pgs cache)
      (update-go-package
       (find-if (lambda (p) (string= (go-package-name p) (go-package-name pg))) cache)
       pg)))
  )


(defun list-all-files-and-folders (root &key (hide nil))
  "scan root folder and return all files & folders"
  (loop
     with dirs = '()
     with files = '()
     for p in (cl-fad:list-directory root)
       when (or hide
              (eq (char= (elt (or (pathname-name p)
                                  (car (last (pathname-directory p))))
                              0)
                         #\.)
                  hide))
     if (not (pathname-name p))
     do (push p dirs)
     else
     do (push p files)
     finally (return (values files dirs))))


(defun clean-ignore-dir (dirs ignore-dir)
  (remove-if (lambda (p) (string= ignore-dir (car (last (pathname-directory p))))) dirs))


(defun filter-file-type (type files)
  (remove-if-not (lambda (f) (string= type (pathname-type f))) files))


(defun loop-files-with-root (root &optional (filetype "go") &key ignore-dir)
  (do* ((root-dirs (list root))
        (root-dir (car root-dirs) (car root-dirs))
        (result)
        )
       ((not root-dirs) result)
    (multiple-value-bind (files dirs) (list-all-files-and-folders root-dir)
      (setf result (merge-pickup-packages ;;:= need update too
                    (map 'list (lambda (file)
                                 (pickup-package (scan-file file)))
                         (filter-file-type filetype files))))
      (setf root-dirs (append (cdr root-dirs) (clean-ignore-dir dirs ignore-dir))))
    ))
