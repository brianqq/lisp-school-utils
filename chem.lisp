(ql:quickload 'defmemo)
(ql:quickload 'drakma)
(ql:quickload 'cl-ppcre)

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

(defmemo get-mass (elem)
    (read-from-string (wiki-scrape 'elem "Standard atomic weight")))

(defmemo get-ion (elem)
  (mapcar #'read-from-string
	  (cl-ppcre:all-matches-as-strings
	   "-?\\d+\\.?\\d*"
	   (cl-ppcre:regex-replace-all "(<.*?>|\\[.*?\\]|\\(.*?\\))"
				       (wiki-scrape elem "Oxidation states")
				       ""))))
