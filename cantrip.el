;;; cantrip.el --- Quick and easy transients from json -*- lexical-binding: t; -*-

;; Copyright (c) John Nelson

;; Author: John Nelson <jnelson@johncoder.com>
;; Homepage: https://github.com/johncoder/cantrip
;; Keywords: lisp
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (transient "0.3.0") (projectile "2.0.0"))
;; File: cantrip.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; cantrip.el is a package that can use JSON to create transient prefixes.

;; Cantrip will bind C-x a r to cantrip-run.  It will look for a
;; parent directory that is a git repository, and parse a JSON file
;; containing scripts.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'projectile)
(require 'transient)

;;;###autoload
(defcustom cantrip-default-files '("package.json" "scripts.json")
  "Files that Cantrip will automatically use.  Must be at the top level of a git repository.")

;;;###autoload
(defcustom cantrip-define-global-key-bindings t
  "Whether to bind some Cantrip commands in the global key map.")

(defvar-local cantrip--symbol-keys '())

(defun cantrip--empty-p (s)
  "Is S an empty string."
  (string= "" s))

(defun cantrip--autolocate-scripts-file ()
  "Locate the parent directory containing one of the default files."
  (dolist (cantrip-default-file cantrip-default-files)
    (let ((scripts-file (concat (locate-dominating-file default-directory ".git")
                                cantrip-default-file)))
      (when (file-exists-p scripts-file)
        (message "cantrip | found scripts file %s" scripts-file)
        (return scripts-file)))))

(defun cantrip--create-script-dispatcher (scripts)
  "Create a dispatcher for SCRIPTS."
  (lambda (transient-args-key)
    (lambda (script-key)
      (interactive)
      (let ((script (gethash script-key scripts)))
        (if script
            (cantrip--projectile-compile script)
          (message "cantrip | script %s not found" script-key))))))

(defun cantrip--create-script-dispatcher-args (scripts)
  "Create a dispatcher for SCRIPTS that extracts optional args from TRANSIENT-ARGS-KEY."
  (lambda (transient-args-key)
    (message "cantrip | dispatcher for transient-args-key: %s" transient-args-key)
    (lambda (script-key)
      (interactive) ; TODO(john): see if this is still necessary
      (let ((script (gethash script-key scripts)))
        (if script
            (cantrip--projectile-compile-args script transient-args-key)
          (message "cantrip | script %s not found" script-key))))))

;;;###autoload
(defun cantrip-run ()
  "Run cantrip in the current directory."
  (interactive)
  (let ((scripts-file (cantrip--autolocate-scripts-file)))
    (if (not scripts-file)
        (message "cantrip | No scripts file found.")
      (let ((script-file-content (cantrip--get-scripts-from-json-file scripts-file)))
        (cantrip--make-transient "cantrip-auto"
                                 nil
                                 (cantrip--process-scripts-hash-table script-file-content)
                                 (cantrip--create-script-dispatcher-args script-file-content)
                                 nil)
        (funcall #'cantrip-auto-root-transient)))))

;;;###autoload
(defun cantrip-define-prefix (source-file namespace)
  "Use cantrip to create a custom transient.
It loads JSON from SOURCE-FILE, and creates a transient under
NAMESPACE.  It returns the transient function."
  (if (not (file-exists-p source-file))
      (lambda ()
        (interactive)
        (message "cantrip | Unable to create %s; file missing: %s." namespace source-file))
    (let ((script-file-content (cantrip--get-scripts-from-json-file source-file)))
      (cantrip--make-transient namespace
                               nil
                               (cantrip--process-scripts-hash-table script-file-content)
                               (cantrip--create-script-dispatcher script-file-content)
                               nil)
      (lambda ()
        (interactive)
        (funcall (intern (format "%s-root-transient" namespace)))))))

;;;###autoload
(progn
  (defun cantrip-maybe-define-global-key-bindings ()
    (when cantrip-define-global-key-bindings
      (let ((map (current-global-map)))
        (dolist (elt '(("C-x a r" . cantrip-run)))
          (let ((key (kbd (car elt)))
                (def (cdr elt)))
            (unless (or (lookup-key map key)
                        (where-is-internal def (make-sparse-keymap) t))
              (define-key map key def)))))))
  (if after-init-time
      (cantrip-maybe-define-global-key-bindings)
    (add-hook 'after-init-hook 'cantrip-maybe-define-global-key-bindings t)))

;;;###autoload
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

(defun cantrip--symbol (s)
  "Get cantrip symbol for S."
  (let ((v (assoc s cantrip--symbol-keys)))
    (if v
        (cdr v)
      (push (cons s (make-symbol s)) cantrip--symbol-keys)
      (cdr (assoc s cantrip--symbol-keys)))))

(defun cantrip--internal-symbol-p (value)
  "Test whether VALUE is an internal symbol."
  (or (eq (cantrip--symbol "$.") value)
      (eq (cantrip--symbol "$segment") value)))

(defun cantrip--transient-function-name (namespace segments ht)
  "Get transient function name from NAMESPACE, SEGMENTS and HT."
  (string-join (remove-if #'cantrip--empty-p
                          (append (list namespace)
                                  segments
                                  (list (gethash (cantrip--symbol "$segment") ht "root")
                                        "transient")))
               "-"))

(defun cantrip--projectile-compile-args (v transient-name-key)
  "Create a function to compile V using projectile with args from TRANSIENT-NAME-KEY."
  (lambda (&optional args)
    (interactive (list (transient-args (intern transient-name-key))))
    (let* ((args--long (seq-find (lambda (i) (string= "--long" i)) args))
           (args--append (seq-find (lambda (i) (string-prefix-p "--append=" i t)) args))
           (compilation-command-value
            (if (string-prefix-p "--append=" args--append t)
                (concat v " " (replace-regexp-in-string "--append=" "" args--append))
              v)))
      ;; TODO(john): when args--long, do the compilation in a dedicated buffer
      (projectile-run-compilation compilation-command-value))))

(defun cantrip--projectile-compile (v)
  "Compile V using projectile."
  (lambda ()
    (interactive)
    (projectile-run-compilation v)))

(defun cantrip--projectile-compile-echo (v)
  "Compile V using projectile."
  (lambda ()
    (interactive)
    (projectile-run-compilation (format "echo %s" v))))

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
      (when (eq (% counter quantity) 0)
        (setq counter 0)
        (setq current (make-vector (min quantity (- (length v) total 1)) 0))
        (push current result)))
    (vconcat (reverse result))))

;; TODO(john): refactor this into something cleaner
(defun cantrip--make-transient (namespace segments ht make-dispatcher cantrip-transient-cache)
  "Make a transient for the current segments.
The transient is created in NAMESPACE using the : delimited
SEGMENTS.  The hash-table HT contains the options.
MAKE-DISPATCHER takes the transient name, which is used to gather
the list of arguments to the prefix.  CANTRIP-TRANSIENT-CACHE is
an alist of previously created transients."
  (let* ((counter 0)
         (menu-label (string-join segments ":"))
         (transient-function-name (cantrip--transient-function-name namespace segments ht))
         (choices (sort
                   (remove-if #'cantrip--internal-symbol-p (hash-table-keys ht))
                   #'string-collate-lessp))
         (actions (make-vector (+ 1 (length choices)) 0))
         (dispatcher (funcall make-dispatcher transient-function-name)))
    ;; (message "cantrip | creating transient: %s" transient-function-name)
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
                          (t (progn (message "cantrip | unexpected type for label") nil))))
             (handler (cond ((stringp choice-value)
                             (if (string= "$." choice-value)
                                 (funcall dispatcher (string-join
                                                      (list menu-label)
                                                      ":"))
                               (funcall dispatcher (string-join
                                                    (remove-if #'cantrip--empty-p
                                                               (list menu-label label))
                                                    ":"))))
                            ((hash-table-p choice-value)
                             (let* ((segment-name (gethash (cantrip--symbol "$segment") choice-value))
                                    (next-transient-function-name
                                     (cantrip--transient-function-name namespace
                                                                       (append segments (list segment-name))
                                                                       choice-value)))
                               (lambda ()
                                 (interactive)
                                 (when (not (assoc next-transient-function-name cantrip-transient-cache))
                                   (setq cantrip-transient-cache
                                         (append cantrip-transient-cache
                                                 (cantrip--make-transient namespace
                                                                          (append segments (list segment-name))
                                                                          choice-value
                                                                          make-dispatcher
                                                                          cantrip-transient-cache))))
                                 (funcall (cdr (assoc next-transient-function-name cantrip-transient-cache))))))
                            (t (progn (message "cantrip | unexpected type for handler") nil)))))
        (aset actions counter (list (format "%s" choice) label handler))))
    (when (> (length choices) 0)
      ;; This calls defalias on the symbol identified by transient-function-name, which is used below
      (cantrip-create-transient (intern transient-function-name)
                                (list "generated doc string"
                                      (vconcat (vector (format "Cantrip\n%s\n\nArguments:"
                                                               (cantrip--autolocate-scripts-file)))
                                               [("-a" "Append" "--append=")
                                                ;; ("-l" "Long running process" "--long") ; TODO(john): this!
                                                ])
                                      (vconcat (vector (format "Menu: %s" (if (string= "" menu-label)
                                                                              "root"
                                                                            menu-label)))
                                               (cantrip--split-vector actions 10))))
      (push (cons transient-function-name
                  (intern transient-function-name))
            cantrip-transient-cache))
    cantrip-transient-cache))

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
    
(provide 'cantrip)
;;; cantrip.el ends here
