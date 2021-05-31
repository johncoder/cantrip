;;; cantrip.el -- quick and easy transients -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'transient)

(defvar-local cantrip--testing nil)
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
  (string-join (remove-if #'emptyp
			  (append (list namespace)
				  segments
				  (list (gethash (cantrip--symbol "$segment") ht "root")
					"transient")))
	       "-"))

(defun emptyp (s)
  "Is S an empty string."
  (string= "" s))

(defun say-hi (v)
  "Say hi V."
  (lambda ()
    (interactive)
    (message "hi %s" v)))

(defun cantrip--split-vector (v quantity)
  "Split vector V into groups of size QUANTITY."
  (let* ((current (make-vector (min quantity (- (length v) 1)) 0))
	 (result (list current))
	 (total 0)
	 (counter 0))
    (dolist (item (nthcdr 1 (append v nil)))
      (aset current counter item)
      (incf counter 1)
      (incf total 1)
      (if (eq (% counter quantity) 0)
	  (progn
	    (setq counter 0)
	    (setq current (make-vector (min quantity (- (length v) total 1)) 0))
	    (push current result))))
    (vconcat (reverse result))))

;; test cantrip--split-vector
(when cantrip--testing
  (progn
    (cantrip--split-vector ["zero" "one"] 5)))

;; test cantrip--split-vector
(when cantrip--testing
  (progn
    (cantrip--split-vector ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"] 3)))

;; test cantrip--split-vector with lists
(when cantrip--testing
  (progn
    (cantrip--split-vector ["THIS SHOULD GO MISSING"
			    ("one" 1 2 3)
			    ("two" 1 2 3)
			    ("three" 1 2 3)
			    ("four" 1 2 3)
			    ("five" 1 2 3)
			    ("six" 1 2 3)
			    ("seven" 1 2 3)
			    ("eight" 1 2 3)
			    ("nine" 1 2 3)
			    ("ten" 1 2 3)]
			   3)))

;; TODO(john): refactor this into something cleaner
(defun cantrip--make-transient (namespace segments ht dispatcher cantrip-transient-cache)
  "Make a transient in NAMESPACE for SEGMENTS using HT and DISPATCHER with CANTRIP-TRANSIENT-CACHE."
  (let* ((counter 0)
	 (menu-label (string-join segments ":"))
	 (transient-function-name (cantrip--transient-function-name namespace segments ht))
	 (choices (sort
		   (remove-if #'cantrip--internal-symbol-p (hash-table-keys ht))
		   #'string-collate-lessp))
	 (actions (make-vector (+ 1 (length choices)) 0)))
    (aset actions 0 (if (string= "" menu-label) namespace menu-label))
    (dolist (choice choices)
      (incf counter 1)
      (let* ((choice-value (gethash choice ht))
	     (label (cond ((stringp choice-value)
			   (if (string= "$." choice-value)
			       menu-label
			     choice-value))
			  ((hash-table-p choice-value)
			   (format "%s (more)" (gethash (cantrip--symbol "$segment") choice-value)))
			  (t (progn (message "Unexpected type for label") nil))))
	     (handler (cond ((stringp choice-value)
			      (if (string= "$." choice-value)
				  (funcall dispatcher (string-join
						       (list menu-label)
						       ":"))
				(funcall dispatcher (string-join
						      (remove-if #'emptyp
								 (list menu-label label))
						     ":"))))
			    ((hash-table-p choice-value)
			     (let* ((segment-name (gethash (cantrip--symbol "$segment") choice-value))
				    (next-transient-function-name
				     (cantrip--transient-function-name namespace
								       (append segments (list segment-name))
								       choice-value)))
			       (if (not (assoc next-transient-function-name cantrip-transient-cache))
				   (progn
				     (setq cantrip-transient-cache
					   (append cantrip-transient-cache
						   (cantrip--make-transient namespace
									    (append segments (list segment-name))
									    choice-value
									    dispatcher
									    cantrip-transient-cache)))))
			       (cdr (assoc next-transient-function-name cantrip-transient-cache))))
			    (t (progn (message "Unexpected type for handler") nil)))))
        (aset actions counter (list (format "%s" choice) label handler))))
    (if (> (length choices) 0)
	(progn
	  (push (cons transient-function-name
		      (cantrip-create-transient (intern transient-function-name)
						(let* ((stuff (vconcat (vector menu-label)
								       (cantrip--split-vector actions 10)))
						       (thingies (list "generated doc string" stuff)))
						  ;; NOTE(john): these are here for debugging :sweat-smile:
						  ;; (pp (cantrip--split-vector actions 5))
						  ;; (pp actions)
						  ;; (pp thingies)
						  thingies)))
		cantrip-transient-cache)))
    cantrip-transient-cache))

;; test cantrip--make-transient with sample.json
(when cantrip--testing
  (progn
    (let* ((sample-content (cantrip--get-scripts-from-json-file "./sample.json"))
	   (ht (cantrip--process-scripts-hash-table sample-content))
	   (ctc '()))
      ;; (message (json-encode ht))
      (cantrip--make-transient "test-cantrip" nil ht #'say-hi ctc))))

;; test cantrip--make-transient
(when cantrip--testing
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
      (cantrip--make-transient "cantrip-test" nil ht #'say-hi ctc))))

;; test cantrip-create-transient
(when cantrip--testing
  (let ((actions (make-vector 3 0)))
    (progn
      (aset actions 0 "Menu")
      (aset actions 1
	    (list "f" "Foo helloski" (lambda () (interactive) (message "Hi world"))))
      (aset actions 2
	    (list "d" "doit" (lambda () (interactive) (message "hi from dorp"))))
      (cantrip-create-transient (intern "test-cantrip-transient")
				(list "this is the doc string" actions)))))

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
  (let ((candidate-strings (split-string (cantrip--get-key-choices segment) "")))
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
(when cantrip--testing
  (let ((ht (make-hash-table))
	(segment "foo")
	(ht2 (make-hash-table)))
    (puthash (cantrip--symbol "f") 42 ht)
    (puthash (cantrip--symbol "F") 42 ht)
    (puthash (cantrip--symbol "o") 42 ht)
    ;; (puthash (cantrip--symbol "$segment") "foo" ht2)
    ;; (puthash (cantrip--symbol "O") ht2 ht)
    (message "%s" (cantrip--select-candidate segment ht))
    (string= "O" (cantrip--select-candidate segment ht))))

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
(when cantrip--testing
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
    nil))

(defun cantrip--get-scripts-from-json-file (filepath)
  "Get a hash of scripts from json in FILEPATH."
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
(when cantrip--testing
  (progn
    (let* ((ht (make-hash-table))
	   (sample-content (cantrip--get-scripts-from-json-file "./sample.json"))
	   (scripts (hash-table-keys sample-content)))
      (dolist (item scripts)
	(let ((segments (split-string item ":")))
	  (cantrip--walk-segments segments ht)))
      (message "%s" (json-encode ht))
      ht)))

;; test cantrip--process-scripts-hash-table
(when cantrip--testing
  (progn
    (let ((sample-content (cantrip--get-scripts-from-json-file "./sample.json")))
      (cantrip--process-scripts-hash-table sample-content))))

(provide 'cantrip)
;;; cantrip.el ends here
