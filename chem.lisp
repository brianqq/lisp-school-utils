(ql:quickload 'defmemo)
(ql:quickload 'drakma)
(ql:quickload 'cl-ppcre)

(defconstant +avogadro+ 6.0221415e+23)

(let ((table-scan (cl-ppcre:create-scanner "(?<=<td>).*?(?=</td>)"
						    :single-line-mode t)))
  (defun wiki-scrape (element property)
    "This function scrapes the wikipedia page for `element` for the entry in wikipedia's html table that comes after `property`"
    (let ((page (drakma:http-request
		 (format nil
			 "http://en.wikipedia.org/wiki/~@(~S~)"
			 element))))
      (cl-ppcre:scan-to-strings table-scan page
				:start (cl-ppcre:scan property page)))))

(defmemo:defmemo get-mass (elem)
  "Scrapes wikipedia for the atomic mass of `elem`"
    (read-from-string (wiki-scrape elem "Standard atomic weight")))

(defmemo:defmemo get-oxstate (elem)
  "Scrapes wikipedia for the oxidation state of `elem`"
  (mapcar #'read-from-string
	  (cl-ppcre:all-matches-as-strings
	   "-?\\d+\\.?\\d*"
	   (cl-ppcre:regex-replace-all "(<.*?>|\\[.*?\\]|\\(.*?\\))"
				       (wiki-scrape elem "Oxidation states")
				       ""))))

(defparameter *symbol-to-name* (make-hash-table))

(defun get-name (symb)
	   (gethash symb *symbol-to-name*))

(defun put-nums (list)
  (do ((lst list (cdr lst))
       (acc))
      ((endp lst)  (nreverse acc))
    (push (car lst) acc)
    (if (and (not (numberp (cadr lst))) (not (numberp (car lst))))
	(push 1 acc))))

(defun molecule-mass (expr)
  "Takes a molecular formula written out like '(C 6 H 12 O 6) and calculates the molar mass"  
  (let ((atoms (group (put-nums expr) 2)))
    (loop for (elem amnt) in atoms sum
	 (* (get-mass (get-name elem)) amnt))))
