;;; avy-custom-action.el --- Create a custom avy action. -*- lexical-binding: t -*-

;; Copyright (C) 2018  Free Software Foundation, Inc.

;; Author: Daniel Hubman <hubisan@gmail.com>
;; URL: https://github.com/hubisan/avy-custom-action
;; Version: 0.1.0-alpha
;; Package-Requires: ((emacs "25.2") (avy "0.4.0"))
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

(defcustom avy-custom-action-stay nil
  "Default value to use for :stay keyword."
  :type 'boolean
  :group 'avy-custom-action)

(defcustom avy-custom-action-actions-all-windows t
  "Default value to use for `avy-all-windows' :all-windows keyword in actions."
  :type
  '(choice
    (const :tag "All Frames" all-frames)
    (const :tag "This Frame" t)
    (const :tag "This Window" nil)))

;;; VARIABLES

(defconst avy-custom-action-keywords
  '(:pre :actions :post :stay)
  "Main keywords to process in order defined in this list.")

(defconst avy-custom-action-actions-keywords-defaults
  '((:all-windows nil)
    (:pre nil)
    (:action nil)
    (:repeat 1)
    (:post nil))
  "Actions keywords to process in order defined in this list.")

;;; HELPER FUNCTIONS

(defun avy-custom-action--normalize-list (keyword args)
  "Normalize ARGS for KEYWORD that want a sexp or list of sexps as ARGS."
  (if (listp args)
      (progn
	(when (= 1 (length args))
	  (setq args (list args)))
	args)
    (error (concat (symbol-name keyword) " wants a sexp or list of sexps."))))

;;; PROCESS KEYWORDS

(defun avy-custom-action--process-main-keywords (args)
  "Process each valid keyword found in plist ARGS."
  
  ;; for each keyword in avy-custom-action-keywords
  ;;   process keyword
  ;;   if :actions proess
  )

(defun avy-custom-action--process-main-keywords (args)
  "Process each valid keyword found in plist ARGS."
  
  ;; for each keyword in avy-custom-action-keywords
  ;;   process keyword
  ;;   if :actions proess
  

  )

;;; KEYWORD HANDLERS

;;;; :pre

(defun avy-custom-action--handler:pre (arg)
  "Handler :pre keyword ARGuments.
And return "
  )

;;;; :actions

(defun avy-custom-action--handler:actions (arg)
  )

;;;; :actions :all-windows

(defun avy-custom-action--handler:actions:all-windows (arg)
  )

;;;; :action :pre

(defun avy-custom-action--handler:actions:pre (arg)
  )

;;;; :actions :action

(defun avy-custom-action--handler:actions:action (arg)
  )

;;;; :actions :repeat

(defun avy-custom-action--handler:actions:action (arg)
  )

;;;; :actions :post

(defun avy-custom-action--handler:actions:action (arg)
  )

;;;; :post

(defun avy-custom-action--handler:pre (arg)
  )

;;;; :stay

(defun avy-custom-action--handler:stay (arg)
  )

;;; MAIN

;;;###autoload
(defmacro avy-custom-action (name &rest args)
  "Define a custom avy action as interactive function with NAME.

For full documentation, please see the README.md file that came with
this file.

Parameters:
  NAME Name to use for the action.
  ARGS Property list, see Usage and Keywords.

Usage:
  (avy-custom-action action-name
    [:stay t | nil]
    [:pre (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])]
    :actions
    (([:all-windows t | nil | 'all-frames]
      [:pre (...) | ((...) (...) ...)]
      :action avy-goto-function-name | regexp
      [:repeat number]
      [:post (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])])
     [(action2) [... [(actionN)]]])
    [:post (sexp) | ((sexp1) (sexp2) [(sexp3) [... [(sexpN)]]])])

Keywords:
  :pre           Sexp or list of sexps to run before the actions.
  :actions       A list of avy actions with following keywords for each:
                 :all-windows   Change `avy-all-windows' temporarily.
                 :pre           Sexp or list of sexp run before action.
                 :action        Name of a avy-goto function or a regex to use
                                with `avy--generic-jump'.
                 :repeat        Number of times the action should be repeated.
                 :post          Sexp or list of sepx run after action.
                 For each action executed the point is stored in
                 variable pts (lexical bound) after :post. The
                 variable can be accessed inside the actions or
                 in :post.
  :post          Sexp or list of sexps to run after the actions.
                 All points from the actions are stored in variable pts.
                 You can use this variable in your sexp e.g. (car pts)
  :stay          Set to t to restore point after the action.

Example:
  (avy-custom-action my-avy-mark-lines"

  (declare (debug t)
           (indent 1))

  (let ((pre (plist-get args :pre))
	(actions (plist-get args :actions))
	(post (plist-get args :post))
        (stay (plist-get args :stay)))

    (unless actions
      (error ":actions keyword is mandatory")

    ;; Normalize keywords that want a sexp or list of sexps
    (when pre
      (setq pre (avy-custom-action--normalize-list :pre pre)))
    (when post
      (setq post (avy-custom-action--normalize-list :post post)))

    ;; Create the
    `(defun ,name ()
       (interactive)
       (let ((start-point (point))
	     (pts nil)
	     (avy-all-windows avy-all-windows)
	     (stay ,stay))
	 ,@(when pre (list pre))
	 ,@(when actions
	     (mapcar
	      (lambda (action)
		(let ((action-pre (plist-get action :pre))
		      (action-all-windows (plist-get action :all-windows))
		      (action-action (plist-get action :action))
		      (action-post (plist-get action :post))
		      (action-repeat (plist-get action :repeat)))
		  ;; Normalize keywords that want a sexp or list of sexps
		  (when action-pre
		    (setq action-pre (avy-custom-action--normalize-list :pre action-pre)))
		  (when action-post
		    (setq action-post (avy-custom-action--normalize-list :post action-post)))
		  (unless action-repeat
		    (setq action-repeat 1))
		  (if (functionp action-action)
		      (setq action-action `(,action-action))
		    (setq action-action `(avy--generic-jump ,action-action nil avy-style)))
		  `(dotimes (i ,action-repeat)
		     (setq avy-all-windows ,action-all-windows)
		     ,@(when action-pre (list action-pre))
	      	     ,action-action
		     ,@(when action-post (list action-post))
		     (setq pts (append pts (list (point)))))))
	      actions))
	 ,@(when post (list post))
	 (progn
	   (when stay (goto-char start-point)))))))

(provide 'avy-custom-action)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;; copy word
(avy-custom-action my-avy-mark-lines
  :actions
  ((:all-windows t
		 :pre (message "%s" "this is run before")
		 :action avy-goto-line
		 :repeat 2
		 :post (message "%s" "this is run after")))
  :post
  (progn
    (goto-char (nth 1 pts))
    (set-mark (nth 0 pts)))
  :stay nil)

;; mark lines
(avy-custom-action my-avy-mark-lines
  :stay nil
  :pre (progn (message "%s" "this is run before"))
  :actions
  (:all-windows t
		 :pre (message "%s" "this is run before")
		 :action avy-goto-line
		 :repeat 2
		 :post (message "%s" "this is run after")))
  :post
  (progn
    (goto-char (nth 1 pts))
    (set-mark (nth 0 pts))))

;; Copy region starting with ( and ending with ) to position and stay
(avy-custom-action my-copy-region-to-and-stay
  :actions
  ((:all-windows nil :action avy-goto-line :post (forward-char) :repeat 2))
  :post
  ((goto-char (nth 0 pts))
   (set-mark (nth pts))))

;;; avy-custom-action.el ends here
