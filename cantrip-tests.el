;;; cantrip-tests.el --- tests for cantrip

;;; Commentary:
;;; Code:
(require 'cantrip)

(defvar-local cantrip--testing nil)

(defun say-hi (v)
  "Say hi V."
  (lambda ()
    (interactive)
    (message "hi %s" v)))

;; test cantrip--make-transient with scripts.json
(when cantrip--testing
  (progn
    (let* ((sample-content (cantrip--get-scripts-from-json-file "./scripts.json"))
	   (ht (cantrip--process-scripts-hash-table sample-content))
	   (ctc '()))
      ;; (message (json-encode ht))
      (cantrip--make-transient "test-cantrip" nil ht #'say-hi ctc))))

;; test cantrip--make-transient with scripts.json, but with projectile compilation
(when cantrip--testing
  (progn
    (let* ((sample-content (cantrip--get-scripts-from-json-file "./scripts.json"))
	   (ht (cantrip--process-scripts-hash-table sample-content))
	   (ctc '()))
      ;; (message (json-encode ht))
      (cantrip--make-transient "test-cantrip" nil ht
			       (lambda (script-key)
				 (interactive)
				 (let ((script (gethash script-key sample-content)))
				   (if script
				       (cantrip--projectile-compile script)
				     (message "Whoops, script %s not found?" script-key))))
			       ctc))))

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
;; test cantrip--walk-segments with cantrip--get-scripts-from-json-file
(when cantrip--testing
  (progn
    (let* ((ht (make-hash-table))
	   (sample-content (cantrip--get-scripts-from-json-file "./scripts.json"))
	   (scripts (hash-table-keys sample-content)))
      (dolist (item scripts)
	(let ((segments (split-string item ":")))
	  (cantrip--walk-segments segments ht)))
      (message "%s" (json-encode ht))
      ht)))

;; test cantrip--process-scripts-hash-table
(when cantrip--testing
  (progn
    (let ((sample-content (cantrip--get-scripts-from-json-file "./scripts.json")))
      (cantrip--process-scripts-hash-table sample-content))))

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
