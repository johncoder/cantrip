;;; cantrip.el --- Quick and easy transients from json -*- lexical-binding: t; -*-

;; Copyright (c) John Nelson

;; Author: John Nelson <jnelson@johncoder.com>
;; Homepage: https://github.com/johncoder/cantrip
;; Keywords: lisp
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (transient "0.3.0"))
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
(require 'transient)

;;;###autoload
(defcustom cantrip-default-files '("package.json" "scripts.json")
  "Files that Cantrip will automatically use.  Must be at the top level of a git repository.")

;;;###autoload
(defcustom cantrip-define-global-key-bindings t
  "Whether to bind some Cantrip commands in the global key map.")

;;;###autoload
(defvar cantrip-transform-command #'cantrip--transform-command
  "A function for intercepting the command and altering it prior to dispatch.")

;;;###autoload
(defvar cantrip-dispatch-command #'cantrip--compile
  "A function that dispatches the command.")

(defvar-local cantrip--symbol-keys '())

(defun cantrip--empty-p (s)
  "Is S an empty string."
  (string= "" s))

(defun cantrip--transform-command (key command args)
  "Use KEY, COMMAND, and ARGS to return a command for dispatch."
  (concat command " " args))

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
            (cantrip--compile script)
          (message "cantrip | script %s not found" script-key))))))

(defun cantrip--create-script-dispatcher-args (scripts)
  "Create a dispatcher for SCRIPTS that extracts optional args from TRANSIENT-ARGS-KEY."
  (lambda (transient-args-key)
    (message "cantrip | dispatcher for transient-args-key: %s" transient-args-key)
    (lambda (script-key)
      (interactive) ; TODO(john): see if this is still necessary
      (let ((script (gethash script-key scripts)))
        (if script
            (cantrip--compile-args script-key script transient-args-key)
          (lambda ()
            (interactive)
            (message "cantrip | script %s not found" script-key)))))))

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
                               (cantrip--create-script-dispatcher-args script-file-content)
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
  (eq (cantrip--symbol "$segment-label") value))

(defun cantrip--transient-function-name (namespace segments ht)
  "Get transient function name from NAMESPACE, SEGMENTS and HT."
  (string-join (remove-if #'cantrip--empty-p
                          (append (list namespace)
                                  segments
                                  (list (gethash (cantrip--symbol "$segment-label") ht "root")
                                        "transient")))
               "-"))

(defun cantrip--compile-args (script-key v transient-name-key)
  "Create a function to compile SCRIPT-KEY V with args from TRANSIENT-NAME-KEY."
  (lambda (&optional args)
    (interactive (list (transient-args (intern transient-name-key))))
    (let* ((args--long (seq-find (lambda (i) (string= "--long" i)) args))
           (args--append (seq-find (lambda (i) (string-prefix-p "--append=" i t)) args))
           (command-args
            (if (string-prefix-p "--append=" args--append t)
                (replace-regexp-in-string "--append=" "" args--append)
              ""))
           (localized-cmd (funcall cantrip-transform-command script-key v command-args)))
      ;; TODO(john): when args--long, do the compilation in a dedicated buffer
      (funcall (funcall cantrip-dispatch-command localized-cmd)))))

;;;###autoload
(defun cantrip--compile (command)
  "Compile COMMAND using COMPILE."
  (lambda ()
    (interactive)
    (message "cantrip | compile %s" command)
    (let ((default-directory (locate-dominating-file default-directory ".git")))
      (compile command))))

(defun cantrip--compile-echo (command)
  "Compile COMMAND, except it just echos."
  (lambda ()
    (interactive)
    (cantrip--compile (format "echo %s" command))))

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
         (choices (remove-if #'cantrip--internal-symbol-p (hash-table-keys ht)))
         (actions (make-vector (+ 1 (length choices)) 0))
         (dispatcher (funcall make-dispatcher transient-function-name)))
    ;; (message "cantrip | creating transient: %s" transient-function-name)
    (aset actions 0 (if (string= "" menu-label) namespace menu-label))
    (dolist (choice choices)
      (incf counter 1)
      (let* ((choice-value (gethash choice ht))
             (label (cond ((stringp choice-value)
                           (if (string= "$segment-identity" choice-value)
                               (format "%s (%s)" (car (last segments)) menu-label)
                             choice-value))
                          ((hash-table-p choice-value)
                           (format "%s (more)" (gethash (cantrip--symbol "$segment-label") choice-value)))
                          (t (progn (message "cantrip | unexpected type for label") nil))))
             (handler (cond ((stringp choice-value)
                             (if (string= "$segment-identity" choice-value)
                                 (funcall dispatcher menu-label)
                               (funcall dispatcher (string-join
                                                    (remove-if #'cantrip--empty-p
                                                               (list menu-label label))
                                                    ":"))))
                            ((hash-table-p choice-value)
                             (let* ((segment-name (gethash (cantrip--symbol "$segment-label") choice-value))
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
                                               ;; TODO(john): ("-l" "Long running process" "--long")
                                               (vector (list "-a" "Append Command" "--append=")
                                                       (list "-d" "Append Directory"
                                                             (concat "--append="
                                                                     (file-relative-name
                                                                      (car (cantrip--get-buffer-dir-and-filename))
                                                                      (locate-dominating-file default-directory ".git"))))
                                                       (list "-f" "Append File"
                                                             (concat "--append="
                                                                     (cdr (cantrip--get-buffer-dir-and-filename))))
                                                       (list "-F" "Append Path to File"
                                                             (concat "--append="
                                                                     (file-relative-name
                                                                      (concat (car (cantrip--get-buffer-dir-and-filename))
                                                                              (cdr (cantrip--get-buffer-dir-and-filename)))
                                                                      (locate-dominating-file default-directory ".git"))))))
                                      (vconcat (vector (format "Menu: %s" (if (string= "" menu-label)
                                                                              "root"
                                                                            menu-label)))
                                               (cantrip--split-vector actions 10))))
      (push (cons transient-function-name
                  (intern transient-function-name))
            cantrip-transient-cache))
    cantrip-transient-cache))

(defun cantrip--get-buffer-dir-and-filename ()
  "Get the dir and filename of the current buffer."
  (let ((buffer-filename (buffer-file-name (window-buffer (minibuffer-selected-window)))))
    (if buffer-filename
        (let* ((filedir (file-name-directory buffer-filename))
               (filenamedotext (car (cdr (split-string buffer-filename filedir)))))
          (cons filedir filenamedotext))
      (cons "" ""))))

(defun cantrip--mix-case (input)
  "Return INPUT as an interleaved list of downcase and upcase characters."
  (string-join
   (mapcar (lambda (s)
             (concat (downcase s) (upcase s)))
           (split-string input ""))
   ""))

(defun cantrip--get-key-choices (input)
  "Get a string of possible letter choices from INPUT."
  (string-join
   (seq-uniq (split-string
              (cantrip--mix-case
               (string-join
                (list (replace-regexp-in-string "[-]+" "" input)
                      "abcdefghijklmnopqrstuvwxyz")
                ""))
              ""))
   ""))

(defun cantrip--select-candidate (segment ht)
  "Get a candidate from SEGMENT for use in HT."
  (let ((candidate-strings (split-string (cantrip--get-key-choices segment) "")))
    (dolist (candidate-string candidate-strings)
      (unless (string= "" candidate-string)
        (let* ((candidate (cantrip--symbol candidate-string))
               (ht-candidate-value (gethash candidate ht)))
          (when (or (eq nil ht-candidate-value)            ; unused
                    (and (stringp ht-candidate-value)      ; segment is exact match
                         (string= segment ht-candidate-value))
                    (and (hash-table-p ht-candidate-value) ; hash table is exact match
                         (stringp (gethash (cantrip--symbol "$segment-label")
                                           ht-candidate-value))
                         (string= segment
                                  (gethash (cantrip--symbol "$segment-label")
                                           ht-candidate-value))))
            (return candidate-string)))))))

(defun cantrip--walk-segments (segments ht)
  "Recur on SEGMENTS nesting each segment under hash-table HT."
  (let* ((segment (car segments))
         (segment-key-string (cantrip--select-candidate segment ht))
         (segment-key (cantrip--symbol segment-key-string))
         (ht-segment-value (gethash segment-key ht)))
    (cond
     ;; we're done walking segments
     ((eq nil segments) ht)
     ;; there are more segments; this segment is a hash
     ((cdr segments)
      (cond ((string= "" segment-key-string) nil)
            ((and (stringp ht-segment-value) (string= "$segment-identity" ht-segment-value)) nil)
            ((hash-table-p ht-segment-value)
             (cantrip--walk-segments (cdr segments) ht-segment-value))
            ((or (eq nil ht-segment-value)
                 (stringp ht-segment-value))
             (let ((next-ht (make-hash-table)))
               ;; the next segment knows its label
               (puthash (cantrip--symbol "$segment-label") segment next-ht)
               ;; this segment points to the next
               (puthash segment-key next-ht ht)
               ;; the value for this segment key is a string, so tuck it under the next
               (if (stringp ht-segment-value)
                   (puthash (cantrip--symbol segment-key-string) "$segment-identity" next-ht))
               (cantrip--walk-segments (cdr segments) next-ht)))
            (t (message "cantrip | unexpected scenario walking with more segments"))))

     ;; segment is a leaf; find a candidate & store it
     ((not (cdr segments))
      (cond ((hash-table-p ht-segment-value)
             (puthash (cantrip--symbol segment-key-string) "$segment-identity" ht-segment-value))
            ((not ht-segment-value)
             (puthash segment-key segment ht))
            (t (message "cantrip | unexpected leaf segment value"))))
     (t (message "cantrip | unexpected scenario while walking segments")))
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
        (scripts (sort (hash-table-keys scripts-hash-table) #'string-lessp)))
    (dolist (item scripts)
      (let ((segments (split-string item ":")))
        (cantrip--walk-segments segments ht)))
    ht))
    
(provide 'cantrip)
;;; cantrip.el ends here
