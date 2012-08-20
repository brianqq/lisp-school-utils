(ql:quickload 'cl-ppcre)
(ql:quickload 'trivial-timers)
(defpackage #:todo
  (:use :cl :cl-ppcre :trivial-timers :qt)
  (:export :*todo-location*
	   :parse-org-text
	   :get-today
	   :make-timers))

(in-package :todo)

;;; http://cl-cookbook.sourceforge.net/functions.html
(declaim (ftype (function (function &rest t) function) curry)
	 (inline curry))
(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

;;; File processing
(defvar *todo-location* "/home/brian/Documents/todo/")

(defun today-string ()
  "Returns the date as \"ddmmyyyy\""
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (declare (ignorable sec min hour))
    (format nil "~2,'0d~2,'0d~d" date month year)))

;;;see http://www.ymeme.com/slurping-a-file-common-lisp-83.html
(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun get-today ()
  "Opens today's todo file in *todo-location*/ddmmyyyy.org"
  (with-open-file (todo (make-pathname :directory *todo-location*
				       :name (today-string)
				       :type "org")
			:direction :inpu)
    (slurp-stream todo)))

(defun interp-time (string)
  (multiple-value-bind (_ time) (scan-to-strings "(\\d{1,2}):(\\d\\d)" string)
    (declare (ignorable _))
    (map 'list #'parse-integer time)))

(defun parse-org-text (text)
 (let ((acc))
   (do-scans (start end reg-start reg-end
	      "\\*\\sTODO\\s(.+?)\\n\\s+?(?!CLOSED)\\s+?SCHEDULED:\\s+?<(.+)>"
	      text (nreverse acc))
     (push 
      (list (subseq text (aref reg-start 0) (aref reg-end 0))
	    (interp-time (subseq text (aref reg-start 1) (aref reg-end 1))))
      acc))))

;;;File processing ends here

(defun alert (x) (format t "~a~%" x))

(defun make-timers (action-list)
  (loop for (text (hour min)) in action-list do
       (schedule-timer (make-timer (curry #'alert text))
		       (multiple-value-bind (_s _m _h date month year)
			   (get-decoded-time)
			 (declare (ignorable _s _m _h))
			 (encode-universal-time 0 min hour date month year))
		       :absolute-p t)))

(make-timers (parse-org-text (get-today)))
