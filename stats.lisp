(defun avg (thing &key
		    (add-step #'identity)
		    (count-step (lambda (_)
				  (declare (ignorable _))
				  1)))
  "Computes the average of `thing`. By default `thing` is assumed to be a list of numbers, in which case we compute sum/length. The keyword arguments can be used to change this behavior."
	   (do ((list thing (cdr list))
		(counter 0 (+ (funcall count-step (car list))
			      counter))
		(sum 0 (+ sum (funcall add-step (car list)))))
	       ((endp list) (/ sum counter))))


(defun variance (list &key
			(avg #'avg)
			(take #'identity)
			(put (lambda (_ x)
			       (declare (ignorable _))
			       x)))
  "Computes variance of `list`. `avg` is the function we use to calculate an average. `take` and `put` are used to change how we process list---take takes list and returns a value (for example `(cdr list)`) and put puts this value back into a data structure that we finally call `avg` on."
  (let ((mean (funcall avg list)))
    (values
     (funcall avg
	      (loop for x in list collect
		   (funcall put x
			    (expt (- (funcall take x) mean) 2))))
     mean)))

(defun e-fn-p (list)
  "Is `(car list)` E?"
  (eq (car list) 'e))

(defun pull-e-fn (list)
  "Returns all the lists in some tree whos car is E"
  (let ((acc))
    (labels ((pull-helper (list)
	       (cond ((atom list) nil)
		     ((e-fn-p list) (push list acc))
		     (t (mapc #'pull-helper list)))))
      (pull-helper list)
      acc)))

(defun e-fn-replace (code gensyms)
  "Replaces an e-fn with a corresponding element of `gensyms`"
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
  "Binds each element in names to a gensym"
  `(let (,@(loop for x in names
	      collect `(,x (gensym ,(format nil "~a" x)))))
     ,@body))


(defmacro E (&rest command)
  "Implements a DSL for writing formulas using expected value. Currently you can take the expected value of a function of one variable, `x?`. 
\(E x?) => a function that takes the average of its parameter
\(E (expt (- x? (E x?)))) => a function that takes the variance of its parameter. "
  (with-gensyms (name list)
    (let* ((elist (pull-e-fn command))
	   (gensyms (loop for x in elist collect
			 (gensym (format nil "~a" x))))) 
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
	     ;;We do this so that nested uses of E work properly---
	     ;;(E (expt (- x? (E x?)) 2)) should spit out a function that
	     ;;computes variance, which means inner calls to E must
	     ;;be evaluated first and applied to `list`
	     (values
	      (funcall avg
		       (loop for ,name in ,list collect
			    (funcall put ,name
				     (let ((x? (funcall take ,name)))
				       (declare (ignorable x?))
				       ,@command))))
	      ,@(loop for sym in gensyms collect sym))))))))

(defun variance (list &key
			(avg #'avg)
			(take #'identity)
			(put
			 (lambda (_ x)
			   (declare (ignorable _))
			   x)))
  "Computes variance of `list`. `avg` is the function we use to calculate an average. `take` and `put` are used to change how we process list---take takes list and returns a value (for example `(cdr list)`) and put puts this value back into a data structure that we finally call `avg` on."
  (funcall (e (expt
	       (- x? (e x?))
	       2))
	   list
	   :avg avg
	   :take take
	   :put put))
