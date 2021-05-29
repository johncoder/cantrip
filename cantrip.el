;;; cantrip.el -- quick and easy transients -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'transient)

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

(defun cantrip--get-key-choices (input)
  "Get a string of possible letter choices from INPUT."
  (string-join
   (seq-uniq
    (mapcar (lambda (i)
	      (string-join (list i (upcase i)) ""))
	    (split-string (concat (downcase input) "acdefghijklmnopqrstuvwxyz") "")))
   ""))

(defun cantrip--select-candidate (segment ht)
  "Get a candidate from SEGMENT for use in HT."
  (let ((candidate-strings (split-string (get-key-choices segment) "")))
    (dolist (candidate-string candidate-strings)
      (unless (string= "" candidate-string)
	(let* ((candidate (intern candidate-string))
	       (candidate-value (gethash candidate ht)))
	  (if (or (eq nil candidate-value)
		  (and (hash-table-p candidate-value)
		       (stringp (gethash (intern "segment") candidate-value))
		       (string= segment (gethash (intern "segment") candidate-value))))
	      (return candidate-string)))))))

;; test cantrip--select-candidate
(let ((ht (make-hash-table))
      (segment "foo")
      (ht2 (make-hash-table)))
  (puthash (intern "f") 42 ht)
  (puthash (intern "F") 42 ht)
  (puthash (intern "o") 42 ht)
  ;; (puthash (intern "segment") "foo" ht2)
  ;; (puthash (intern "O") ht2 ht)
  (message "%s" (cantrip--select-candidate segment ht))
  (string= "O" (cantrip--select-candidate segment ht)))

(defun cantrip--walk-segments (segments ht)
  "Recur on SEGMENTS nesting each segment under hash-table HT."
  (cond
    ;; we're done walking segments
    ((eq nil segments) ht)

    ;; there are more segments; this segment is a hash
    ((cdr segments)
     (let* ((segment (car segments))
	    (candidate-strings (split-string (get-key-choices segment) "")))
       (message "handling segment %s (more...)" segment)
       ;; iterate candidates until one is used
       (dolist (candidate-string candidate-strings)
	 (let* ((candidate (intern candidate-string))
		(candidate-value (gethash candidate ht)))
	   (cond
	     ;; skip empty candidates
	     ((string= "" candidate-string) ht)
	     ;; the candidate does not yet have a value
	     ((eq nil candidate-value)
	      (let ((next-ht (make-hash-table)))
		;; this next one needs to know its own segment name
		(puthash (intern "segment") segment next-ht)
		;; its value is now a new hash-table
		(puthash candidate next-ht ht)
		(cantrip--walk-segments (cdr segments) next-ht)
		(return ht)))
	     ;; the candidate is already a hash-table
	     ((hash-table-p candidate-value)
	      (progn
		(cantrip--walk-segments (cdr segments) candidate-value)
		(return ht)))
	     ;; the candidate exists, but it's not a hash-table
	     (t
	      (let ((next-ht (make-hash-table)))
		;; this next one needs to know its own segment name
		(puthash (intern "segment") segment next-ht)
		;; current value goes in the next hash table
		(puthash (intern ".") candidate-value next-ht)
		;; next hash table goes in the current hash table
		(puthash candidate next-ht ht)
		;; recur
		(cantrip--walk-segments (cdr segments) next-ht)
		(return nil))))))))

    ;; (car segments) is a leaf; find a candidate & store it
    (t
     (let* ((segment (car segments))
	    (candidate-strings (split-string (get-key-choices segment) "")))
       (message "handling leaf segment %s" segment)
       (dolist (candidate-string candidate-strings)
	 (let* ((candidate (intern candidate-string)))
	   (cond ((string= ":" candidate-string) ht) ; invalid option
		 ((string= "" candidate-string) ht)  ; invalid option
		 ((gethash candidate ht) ht) ; already exists
		 ((not (gethash candidate ht)) ; this candidate is available
		  (progn
		    (puthash candidate segment ht)
		    (message "puthash %s %s gethash: %s" candidate segment (gethash candidate ht))
		    (return ht)))
		 (t ht))))
       ht)))
  ht)

(provide 'cantrip)
;;; cantrip.el ends here
