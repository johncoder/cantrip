;;; cantrip.el -- quick and easy transients -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'transient)

(defvar-local cantrip--symbol-keys '())

(defun cantrip--symbol (s)
  "Get symbol for S."
  (let ((v (assoc s cantrip--symbol-keys)))
    (if v (cdr v)
      (progn
	(push (cons s (make-symbol s)) cantrip--symbol-keys)
	(cdr (assoc s cantrip--symbol-keys))))))

(defun cantrip-create-transient (alias actions)
  "Create a transient ALIAS manualy using ACTIONS."
  (pcase-let ((`(,class ,slots ,suffixes ,docstr ,body)
	       (transient--expand-define-args actions)))
    (progn
      (defalias alias
	(lambda ()
	  (interactive)
	  (transient-setup alias)))
      (put alias 'interactive-only t)
      (put alias 'function-documentation docstr)
      (put alias 'transient--prefix
	   (transient-prefix :command alias))
      (put alias 'transient--layout
	   (cl-mapcan (lambda (s)
			(transient--parse-child alias s))
		      suffixes))
      alias)))

(defun cantrip--internal-symbol-p (value)
  "Test whether VALUE is an internal symbol."
  (or (eq (cantrip--symbol "$.") value)
      (eq (cantrip--symbol "$segment") value)))

(defun cantrip--transient-function-name (namespace segments ht)
  "Get transient function name from NAMESPACE, SEGMENTS and HT."
  (string-join (append (list namespace)
		       segments
		       (list (gethash (cantrip--symbol "$segment") ht)
			     "transient"))
	       "-"))

(defun say-hi (v)
  "Say hi V."
  (lambda ()
    (interactive)
    (message "hi %s" v)))

(defun cantrip--make-transient (namespace segments ht dispatcher cantrip-transient-cache)
  "Make a transient in NAMESPACE for SEGMENTS using HT and DISPATCHER with CANTRIP-TRANSIENT-CACHE."
  (let* ((menu-label (string-join segments ":"))
	 (transient-function-name (cantrip--transient-function-name namespace segments ht))
	 (choices (remove-if #'cantrip--internal-symbol-p (hash-table-keys ht)))
	 (actions (make-vector (+ 1 (length choices)) 0)))
    (setq counter 0)
    (aset actions 0 menu-label)
    (dolist (choice choices)
      (let* ((choice-value (gethash choice ht))
	     (label (cond ((stringp choice-value) choice-value)
			  ((hash-table-p choice-value)
			   (gethash (cantrip--symbol "$segment") choice-value))))
	     (handler (cond ((stringp choice-value)
			     (funcall dispatcher (string-join
						  (remove-if (lambda (s)
							       (string= "" s))
							     (list menu-label label))
						  ":")))
			    ((hash-table-p choice-value)
			     (or (cdr (assoc transient-function-name cantrip-transient-cache))
				 (cantrip--make-transient namespace
							  (append segments (list label))
							  choice-value
							  dispatcher
							  cantrip-transient-cache))))))
	(incf counter 1)
	(aset actions counter (list (format "%s" choice) label handler))))
    (push (cons transient-function-name
		(cantrip-create-transient (intern transient-function-name)
					  (list "generated doc string" actions)))
	  cantrip-transient-cache)
    (cdr (assoc transient-function-name cantrip-transient-cache))))

;; test cantrip--make-transient
(progn
  (let ((ht (make-hash-table))
	(ht2 (make-hash-table))
	(ctc '()))
    (puthash (cantrip--symbol "$segment") "taz" ht2)
    (puthash (cantrip--symbol "n") "new" ht2)
    (puthash (cantrip--symbol "h") "hue" ht2)

    (puthash (cantrip--symbol "f") "foo" ht)
    (puthash (cantrip--symbol "b") "bar" ht)
    (puthash (cantrip--symbol "h") ht2 ht)
    (puthash (cantrip--symbol "$segment") "lol" ht)
    (message (json-encode ht))
    (cantrip--make-transient "cantrip-test" nil ht #'say-hi ctc)))

(defun cantrip--get-key-choices (input)
  "Get a string of possible letter choices from INPUT."
  (string-join
   (seq-uniq
    (mapcar (lambda (i)
	      (string-join (list i (upcase i)) ""))
	    (split-string
	     (replace-regexp-in-string
	      "[-]+" ""
	      (concat (downcase input) "acdefghijklmnopqrstuvwxyz") ""))))
   ""))

(defun cantrip--select-candidate (segment ht)
  "Get a candidate from SEGMENT for use in HT."
  (let ((candidate-strings (split-string (get-key-choices segment) "")))
    (dolist (candidate-string candidate-strings)
      (unless (string= "" candidate-string)
	(let* ((candidate (cantrip--symbol candidate-string))
	       (candidate-value (gethash candidate ht)))
	  (if (or (eq nil candidate-value)
		  (and (hash-table-p candidate-value)
		       (stringp (gethash (cantrip--symbol "$segment") candidate-value))
		       (string= segment (gethash (cantrip--symbol "$segment") candidate-value)))
		  (and (hash-table-p candidate-value)
		       (stringp (gethash (cantrip--symbol "$.") candidate-value))
		       (string= segment (gethash (cantrip--symbol "$.") candidate-value))))
	      (return candidate-string)))))))

;; test cantrip--select-candidate
(let ((ht (make-hash-table))
      (segment "foo")
      (ht2 (make-hash-table)))
  (puthash (cantrip--symbol "f") 42 ht)
  (puthash (cantrip--symbol "F") 42 ht)
  (puthash (cantrip--symbol "o") 42 ht)
  ;; (puthash (cantrip--symbol "$segment") "foo" ht2)
  ;; (puthash (cantrip--symbol "O") ht2 ht)
  (message "%s" (cantrip--select-candidate segment ht))
  (string= "O" (cantrip--select-candidate segment ht)))

(defun cantrip--walk-segments (segments ht)
  "Recur on SEGMENTS nesting each segment under hash-table HT."
  (let* ((segment (car segments))
	 (segment-key-string (cantrip--select-candidate segment ht))
	 (segment-key (cantrip--symbol segment-key-string))
	 (segment-value (gethash segment-key ht)))
    (cond
     ;; we're done walking segments
     ((eq nil segments) ht)

     ;; there are more segments; this segment is a hash
     ((cdr segments)
      (cond ((string= "" segment-key-string) nil)
	    ((hash-table-p segment-value)
	     (cantrip--walk-segments (cdr segments) segment-value))
	    (t (let ((next-ht (make-hash-table)))
		 (if (not (eq nil segment-value))
		     (puthash (cantrip--symbol "$.") segment-value next-ht))
		 (puthash (cantrip--symbol "$segment") segment next-ht)
		 (puthash segment-key next-ht ht)
		 (cantrip--walk-segments (cdr segments) next-ht)))))

     ;; (car segments) is a leaf; find a candidate & store it
     (t (cond ((hash-table-p segment-value)
	       (puthash (cantrip--symbol "$.") segment segment-value))
	      ((not segment-value)
	       (puthash segment-key segment ht)))))
  ht))

;; test cantrip--walk-segments
(progn
  (let ((ht (make-hash-table)))
    (dolist (item '("foo:bar:baz"
		    "foo:bar"
		    "foo:qaz"
		    "moo"))
      (let ((segments (split-string item ":")))
	;; (message "segments %s" segments)
	(cantrip--walk-segments segments ht)))
    (message "%s" (json-encode ht)))
  nil)

(defun cantrip--get-scripts-from-json-file (filepath)
  "Get a hash of scripts from FILEPATH package.json."
  (let* ((json-object-type 'hash-table)
	 (json-key-type 'string)
	 (json-array-type 'list)
	 (json (json-read-file filepath)))
    (gethash "scripts" json)))

(defun cantrip--process-scripts-hash-table (scripts-hash-table)
  "Process SCRIPTS-HASH-TABLE into a hash-table of aliased commands."
  (let ((ht (make-hash-table))
	(scripts (hash-table-keys scripts-hash-table)))
    (dolist (item scripts)
      (let ((segments (split-string item ":")))
	(cantrip--walk-segments segments ht)))
    ht))
    
;; test cantrip--walk-segments with cantrip--get-scripts-from-json-file
(progn
  (let* ((ht (make-hash-table))
	 (sample-content (cantrip--get-scripts-from-json-file "./sample.json"))
	 (scripts (hash-table-keys sample-content)))
    (dolist (item scripts)
      (let ((segments (split-string item ":")))
    	(cantrip--walk-segments segments ht)))
    (message "%s" (json-encode ht))
    ht))

;; test cantrip--process-scripts-hash-table
(progn
  (let ((sample-content (cantrip--get-scripts-from-json-file "./sample.json")))
    (cantrip--process-scripts-hash-table sample-content)))

(provide 'cantrip)
;;; cantrip.el ends here
