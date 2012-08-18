(ql:quickload 'cl-ppcre)

(defpackage #:todo
  (:use :cl :cl-ppcre :qt)
  (:export :*todo-location*
	   :parse-org-text
	   :time-now))

(in-package :todo)

(defvar *todo-location*)

(defun today-string ()
  "Returns the date as \"ddmmyyyy\""
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (declare (ignorable sec min hour))
    (format nil "~2,'0d~2,'0d~d" date month year)))

(defun get-today ()
  "Opens today's todo file in *todo-location*/ddmmyyyy.org"
  (with-open-file (todo (make-pathname :location *todo-location*
				       :name (today-string)
				       :type ".org"))
    (slurp-stream todo)))

(defun time-now ()
  (multiple-value-bind
	(second minute hour) (get-decoded-time)
    (list hour minute)))

;;;see http://www.ymeme.com/slurping-a-file-common-lisp-83.html
(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun parse-org-text (text)
 (let ((acc))
   (do-scans (start
	      end reg-start reg-end
	      "\\*\\sTODO\\s(.+?)\\n\\s+?SCHEDULED:\\s+?<(.+)>"
	      text (nreverse acc))
     (push 
      (list (subseq text (aref reg-start 0) (aref reg-end 0))
	    (multiple-value-bind (_ time)
		(scan-to-strings "(\\d{1,2}):(\\d\\d)"
				 (subseq text (aref reg-start 1)
					 (aref reg-end 1)))
	      (map 'list #'parse-integer time)))
      acc))))
