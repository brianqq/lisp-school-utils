(defun avg (thing &key (add-step #'identity)
			     (count-step (lambda (_)
					   (declare (ignorable _))
					   1)))
	   (do ((list thing (cdr list))
		(counter 0 (+ (funcall count-step (car list))
			      counter))
		(sum 0 (+ sum (funcall add-step (car list)))))
	       ((endp list) (/ sum counter))))

;;Ideally there should be some kind of dsl that goes from
;;pseudomath definitions into list operators that let you
;;swap in alternatives to the basic functions like average
;(defop variance (E (expt (- x Ex) 2)))


(defun compose (&rest fns)
  (lambda (x)
    (reduce #'funcall fns
	    :initial-value x
	    :from-end t)))

(defun curry (fn &rest args)
  (lambda (&rest more-args)
    (apply fn (append args more-args))))



;;TODO these are really shitty keyword args
(defun variance (list &key
			(avg #'avg)
			(take #'identity)
			(put (lambda (_ x)
			       (declare (ignorable _))
			       x)))
  (let ((mean (funcall avg list)))
    (values
     (funcall avg
	      (loop for x in list collect
		   (funcall put x
			    (expt (- (funcall take x) mean) 2))))
     mean)))

(defun e-fn-p (list)
  (eq (car list) 'e))

(defun pull-e-fn (list)
  (let ((acc))
    (labels ((pull-helper (list)
	       (cond ((atom list) nil)
		     ((e-fn-p list) (push list acc))
		     (t (mapc #'pull-helper list)))))
      (pull-helper list)
      acc)))

(defun e-fn-replace (code gensyms)
  (assert (= (length gensyms)
	     (length (pull-e-fn code))))
  (let ((gensyms gensyms))
    (labels ((replace-helper (list)
	       (cond ((atom list) list)
		     ((e-fn-p list)
		      (pop gensyms))
		     (t (mapcar #'replace-helper list)))))
      (replace-helper code))))


(defmacro with-gensyms (names &body body)
  `(let (,@(loop for x in names
	      collect `(,x (gensym ,(format nil "~a" x)))))
     ,@body))

;;this got really messy really fast
(defmacro E (&rest command)
  (with-gensyms (name list)
    (let* ((elist (pull-e-fn command))
	   (gensyms (loop for x in elist collect
			 (gensym (format nil "~a" x))))) ;;;I still think hashmaps would be better
      (let ((command (e-fn-replace command gensyms)))
	`(lambda (,list &key
		     (avg #'avg)
		     (take #'identity)
		     (put
		      (lambda (_ x)
			(declare (ignorable _))
			x)))
	   (let (,@(loop for x in elist
		      for sym in gensyms collect
			`(,sym (funcall ,x ,list
					:avg avg
					:take take
					:put put))))
	     (values
	      (funcall avg
		       (loop for ,name in ,list collect
			    (funcall put ,name
				     (let ((x? (funcall take ,name)))
				       (declare (ignorable x?))
				       ,@command))))
	      ; (list
	      ;	      ,@(loop for sym in gensyms
	;	   for name in elist collect
	;	     `(cons ',name ,sym)
	      ,@(loop for sym in gensyms collect sym))))))))

(defun variance (list &key
			(avg #'avg)
			(take #'identity)
			(put
			 (lambda (_ x)
			   (declare (ignorable _))
			   x)))
  (funcall (e (expt
	       (- x? (e x?))
	       2))
	   list
	   :avg avg
	   :take take
	   :put put))
