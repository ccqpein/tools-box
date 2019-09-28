(ql:quickload :cl-ppcre)

(defvar *return-regex* "(\\w+|\\((\\w+\\ *,*\\ *)+\\))")

(defvar *func-regex* (concatenate 'string "^func\\ +\\w+\\((|(\\w+\\ +\\w+\\ *,*\\ *)+(\\w+\\ +\\.\\.\\.interface{})*)\\)\\ +" *return-regex* "*\\ *{*.*$"))

(defvar *struct-regex* "^type\\ +\\w+\\ +struct\\ *{(|\\ *\\n+(\\ +(\\w+|\\w+\\ *,*\\ *)+\\ +\\w+\\n+)*)}")
