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
;; This package lets you create custom avy actions.
;;
;; Example:



;;; Code:
(require 'avy)

(defun avy-custom-action--normalize-list (keyword args)
  "Normalize ARGS for KEYWORD that want a sexp or list of sexps as ARGS."
  (if (listp args)
      (progn
	(when (= 1 (length args))
	  (setq args (list args)))
	args)
    (error (concat (symbol-name keyword) " wants a sexp or list of sexps."))))

;;;###autoload
(defmacro avy-custom-action (name &rest args)
  "Define a custom avy action as interactive function with NAME.

For full documentation, please see the README file that came with
this file.

Args:
  NAME Name to use for the action.
  ARGS Property list, see Usage and Keywords.

Usage:
  (avy-custom-action action-name
     [:keyword option]...)

Keywords:
  :stay          Set to t to restore point after the action.
  :pre           Sexp or list of sexps to run before the actions.
  :actions       A list of avy actions with following keywords for each:
                 :all-windows   Change `avy-all-windows' temporarily for the action.
                 :pre           Sexp or list of sexp run before action.
                 :action        Name of a avy-goto function or a regex to use with
                                `avy--generic-jump'.
                 :repeat        Number of times the action should be repeated.
                 :post          Sexp or list of sepx run after action.
                 For each action executed the point is stored in
                 variable pts (lexical bound) after :post. The
                 variable can be accessed inside the actions or
                 in :post.
  :post          Sexp or list of sexps to run after the actions.
                 All points from the actions are stored in variable pts.
                 You can use this variable in your sexp e.g. (car pts)

Example:
  (avy-custom-action my-avy-mark-lines
"
  (declare (debug t)
           (indent defun))

  (let ((stay (plist-get args :stay))
	(pre (plist-get args :pre))
	(actions (plist-get args :actions))
	(post (plist-get args :post)))

    ;; Normalize keywords that want a sexp or list of sexps
    (when pre
      (setq pre (avy-custom-action--normalize-list :pre pre)))
    (when post
      (setq post (avy-custom-action--normalize-list :post post)))

    ;; Create the
    `(defun ,name ()
       (interactive)
       (let ((start-point (point))
	     (pts)
	     (avy-all-windows)
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
  :stay nil
  :pre buffer-file-name
  :actions
  ((:all-windows t
		 :pre (message "%s" "this is run before")
		 :action avy-goto-line
		 :repeat 2
		 :post (message "%s" "this is run after")))
  :post
  (progn
    (goto-char (nth 1 pts))
    (set-mark (nth 0 pts))))

;; mark lines
(avy-custom-action my-avy-mark-lines
  :stay nil
  :pre (progn (message "%s" "this is run before"))
  :actions
  ((:all-windows t
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
