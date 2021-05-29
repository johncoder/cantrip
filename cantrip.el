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
		       (string= segment (gethash (intern "segment") candidate-value)))
		  (and (hash-table-p candidate-value)
		       (stringp (gethash (intern ".") candidate-value))
		       (string= segment (gethash (intern ".") candidate-value))))
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
  (let* ((segment (car segments))
	 (segment-key-string (cantrip--select-candidate segment ht))
	 (segment-key (intern segment-key-string))
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
		     (puthash (intern ".") segment-value next-ht))
		 (puthash (intern "segment") segment next-ht)
		 (puthash segment-key next-ht ht)
		 (cantrip--walk-segments (cdr segments) next-ht)))))

     ;; (car segments) is a leaf; find a candidate & store it
     (t (cond ((hash-table-p segment-value)
	       (puthash (intern ".") segment segment-value))
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

(provide 'cantrip)
;;; cantrip.el ends here
