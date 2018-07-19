;;; avy-custom-action.el --- Create a custom avy action. -*- lexical-binding: t -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: Daniel Hubman <hubisan@gmail.com>
;; URL: https://github.com/hubisan/avy-custom-action
;; Version: 0.1.0-alpha
;; Package-Requires: ((emacs "24.1") (avy "0.4.0"))
;; Keywords: point, location

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.)

;;; Commentary:
;;
;; Easily create custom avy actions.
;;
;; Example:

;;; Code:
(require 'avy)

;;; CUSTOMIZATION

(defgroup avy-custom-action nil
  "A use-package declaration for simplifying your `.emacs'."
  :group 'convenience)

(defcustom avy-custom-action-abort-actions-key nil
  "Key to abort during actions. 
Aborts the actions and executes :after"
  ;; TODO
  )

;;; POSITIONS

(defvar avy-custom-action-initial-position nil
  "Store position (window and point) before the action is executed.
Initial position is not reset until next custom action is run.")

(defvar avy-custom-action-positions nil
  "Hold position (window and point) history.
Stored positions are not reset until next custom action is run.")

(defun avy-custom-action--store-initial-position ()
  "Store the current position (point and window).
Gets stored in `avy-custom-action-initial-position'."
  (setq avy-custom-action-initial-position
        (list (cons (point) (selected-window)))))

(defun avy-custom-action--store-position ()
  "Store the current position (point and window).
Gets pushed to `avy-custom-action-positions'."
  (push (cons (point) (selected-window)) avy-custom-action-positions))

(defun avy-custom-action--goto-position-in-list (positions index)
  "Go to position stored at INDEX in POSITIONS."
  (let ((pos))
    (setq pos (nth index positions))
    ;; Try to go to to the position
    (condition-case nil
        (progn
          (let* ((window (cdr pos))
                 (frame (window-frame window)))
            (when (and (frame-live-p frame)
                       (not (eq frame (selected-frame))))
              (select-frame-set-input-focus frame))
            (select-window window)
            (goto-char (car pos))))
      (error "Not able to go to position %s." pos))))

(defun avy-custom-action-goto-position (index &optional reverse)
  "Go to position stored at INDEX in `avy-custom-action-positions'.
Last stored position has INDEX 0 unless REVERSE is set to t.
If REVERSE is set to t the first INDEX 0 is the first stored position."
  (let ((positions avy-custom-action-positions))
    (when (= (length positions) 0)
      (error "No positions stored in `avy-custom-action-positions'."))
    (when (or (< index 0) (> index (1- (length positions))))
      (error "Invalid index %s for position." index))
    ;; If reserve reserve the index
    (when reverse
      (setq index (- (1- (length positions)) index)))
    ;; Go to the position
    (avy-custom-action--goto-position-in-list positions index)))

(defun avy-custom-action-restore-initial-position ()
  "Go to the initial position."
  (let ((positions avy-custom-action-initial-position))
    (unless (= (length positions) 1)
      (error "No position stored in `avy-custom-action-initial-position'."))
    (avy-custom-action--goto-position-in-list positions 0)))

;;; HANDLERS

(defun avy-custom-action--handle-abort ()
  "Handle aborting actions.
The key defined with `avy-custom-action-abort-actions-key' aborts the actions.
If actions aborted process the post keywords."
  )

;;; KEYWORDS

(defconst avy-custom-action-keywords
  '(:pre (:before)
    :actions (:all-windows :style :before :repeat :action :after :store)
    :post (:after :goto))
  "Keywords to process in order defined in this list.
:pre, :actions and :post are not to be modified.")

(defvar avy-custom-action-keywords-plist nil
  "Store the plist for handling abort.")

(defun avy-custom-action--normalize-list (keyword args)
  "Normalize ARGS for KEYWORD that want a sexp or list of sexps as ARGS."
  (if (listp args)
      (progn
	(when (= 1 (length args))
	  (setq args (list args)))
	args)
    (error (concat (symbol-name keyword) " wants a sexp or list of sexps."))))

(defun avy-custom-action--process-keywords-recursively())

(defun avy-custom-action--process (plist)
  "Process each valid keyword found in plist ARGS."
  (declare (indent 1))
  (unless (listp plist)
    (error "Argument used is not a list."))
  (when (null (plist-get plist :actions))
    (error "The :actions keyword is mandatory."))
  (setq avy-custom-action-plist plist)
  (setq avy-custom-action-plist plist)
  (avy-custom-action--process-pre (plist))
  (avy-custom-action--process-actions (plist))
  (avy-custom-action--process-post (plist))

  
    (let ((keyword (car plist))
           (arg (cadr plist))
           (rest (cddr plist)))
      (unless (keywordp keyword)
        (error (format "%s is not a keyword" keyword)))
      (let* ((handler (concat "use-package-handler/" (symbol-name keyword)))
             (handler-sym (intern handler)))
        (if (functionp handler-sym)
            (funcall handler-sym name keyword arg rest state)
          (use-package-error
           (format "Keyword handler not defined: %s" handler))))))
  
  ;; Set up lexical bound variables
  
  ;; for each keyword in avy-custom-action-keywords
  ;;   process keyword
  ;;   if not :actions keyword > signal error

;;;; Pre

(defun avy-custom-action--process-pre (args)
  "Process each valid keyword found in plist ARGS."
  
  ;; for each keyword in avy-custom-action-keywords
  ;;   process keyword
  ;;   if :actions proess
  )

;;;;; :before

(defun avy-custom-action--process:before (arg)
  "Process :before keyword ARGuments.
And return "
  )

;;;; Actions

(defun avy-custom-action--process-actions (args)
  "Process each valid keyword found in plist ARGS."
  (save-mark-and-excursion

    )
  )

;;;;; :actions :all-windows

(defun avy-custom-action--process:actions:all-windows (arg)
  )

;;;;; :actions :style

(defun avy-custom-action--process:actions:style (arg)
  )

;;;;; :action :before

(defun avy-custom-action--process:actions:before (arg)
  )

;;;;; :actions :repeat

(defun avy-custom-action--process:actions:repeat (arg)
  )

;;;;; :actions :action

(defun avy-custom-action--process:actions:action (arg)
  )

;;;;; :actions :after

(defun avy-custom-action--process:actions:after (arg)
  )

;;;;; :actions :store

(defun avy-custom-action--process:actions:store (arg)
  )

;;;; Post

(defun avy-custom-action--process-post (args)
  "Process each valid keyword found in plist ARGS."
  )

;;;;; :after

(defun avy-custom-action--process:after (arg)
  )

;;;;; :goto

(defun avy-custom-action--process:goto (arg)
  )

;;; MAIN

;;;###autoload
(defmacro avy-custom-action (name &rest plist)
  "Define a custom avy action as interactive function with NAME.

For full documentation, please see the README.md file that came with
this file.

Parameters:
  NAME Name to use for the action.
  ARGS Property list, see Usage and Keywords.

Usage:
  (avy-custom-action action-name
    [:goto-last t | nil]
    [:before (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])]
    :actions
    (([:all-windows t | nil | 'all-frames]
      [:before (...) | ((...) (...) ...)]
      :action avy-goto-function-name | regexp
      [:repeat number]
      [:after (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])])
     [(action2) [... [(actionN)]]])
    [:after (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])])

Keywords:
  :before           Sexp or list of sexps to run before the actions.
  :actions       A list of avy actions with following keywords for each:
                 :all-windows   Change `avy-all-windows' temporarily.
                 :before        Sexp or list of sexp run before action.
                 :action        Name of a avy-goto function or a regex to use
                                with `avy--generic-jump'.
                 :repeat        Number of times the action should be repeated.
                 :after         Sexp or list of sepx run after action.
                 For each action executed the point is stored in
                 variable pts (lexical bound) after :after. The
                 variable can be accessed inside the actions or
                 in :after.
  :after         Sexp or list of sexps to run after the actions.
                 All points from the actions are stored in variable pts.
                 You can use this variable in your sexp e.g. (car pts)
  :goto          Go to a position after the action:
                 'stay (default) Restore initial position.
                 'first Go to first position stored after first action.
                 'last Go to last stored point.
                 Number Go to nth position.

Example:
  (avy-custom-action my-avy-mark-lines"

  (declare (debug t)
           (indent 1))

  (let ((before (plist-get args :before))
	(actions (plist-get args :actions))
	(after (plist-get args :after))
        (goto-last (plist-get args :goto-last)))

    (unless actions
      (error ":actions keyword is mandatory")

    ;; Normalize keywords that want a sexp or list of sexps
    (when before
      (setq before (avy-custom-action--normalize-list :before before)))
    (when after
      (setq after (avy-custom-action--normalize-list :after after)))

    ;; Create the
    `(defun ,name ()
       (interactive)
       (let ((start-point (point))
	     (pts nil)
	     (avy-all-windows avy-all-windows)
	     (goto-last ,goto-last))
	 ,@(when before (list before))
	 ,@(when actions
	     (mapcar
	      (lambda (action)
		(let ((action-before (plist-get action :before))
		      (action-all-windows (plist-get action :all-windows))
		      (action-action (plist-get action :action))
		      (action-after (plist-get action :after))
		      (action-repeat (plist-get action :repeat)))
		  ;; Normalize keywords that want a sexp or list of sexps
		  (when action-before
		    (setq action-before (avy-custom-action--normalize-list :before action-before)))
		  (when action-after
		    (setq action-after (avy-custom-action--normalize-list :after action-after)))
		  (unless action-repeat
		    (setq action-repeat 1))
		  (if (functionp action-action)
		      (setq action-action `(,action-action))
		    (setq action-action `(avy--generic-jump ,action-action nil avy-style)))
		  `(dotimes (i ,action-repeat)
		     (setq avy-all-windows ,action-all-windows)
		     ,@(when action-before (list action-before))
	      	     ,action-action
		     ,@(when action-after (list action-after))
		     (setq pts (append pts (list (point)))))))
	      actions))
	 ,@(when after (list after))
	 (progn
	   (when goto-last (goto-char start-point)))))))

(provide 'avy-custom-action)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;; copy word
(avy-custom-action my-avy-mark-lines
  :actions
  ((:all-windows t
		 :before (message "%s" "this is run before")
		 :action avy-goto-line
		 :repeat 2
		 :after (message "%s" "this is run after")))
  :after
  (progn
    (goto-char (nth 1 pts))
    (set-mark (nth 0 pts)))
  :stay nil)

;; mark lines
(avy-custom-action my-avy-mark-lines
  :stay nil
  :before (progn (message "%s" "this is run before"))
  :actions
  (:all-windows t
		 :before (message "%s" "this is run before")
		 :action avy-goto-line
		 :repeat 2
		 :after (message "%s" "this is run after")))
  :after
  (progn
    (goto-char (nth 1 pts))
    (set-mark (nth 0 pts))))

;; Copy region starting with ( and ending with ) to position and stay
(avy-custom-action my-copy-region-to-and-stay
  :actions
  ((:all-windows nil :action avy-goto-line :after (forward-char) :repeat 2))
  :after
  ((goto-char (nth 0 pts))
   (set-mark (nth pts))))

;;; avy-custom-action.el ends here
