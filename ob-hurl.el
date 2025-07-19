;;; ob-hurl.el --- Org babel integration for hurl -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: December 05, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/JasZhe/hurl-mode
;;
;; This file is not part of GNU Emacs.
;;
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; Commentary:
;; Very simple org babel integration for hurl mode
;; a lot taken from the ob-template

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("hurl" . "tmp"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:hurl '())

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:hurl' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:hurl (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-hurl nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-hurl-var-to-hurl (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;;###autoload
(defun org-babel-execute:hurl (body params)
  "Execute a block of Hurl code with org-babel.
Any variables assigned to the src block get passed into the cli command via the --variable option.
This function is called by `org-babel-execute-src-block'"
  (message "executing Hurl source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         ;; same as org-babel--get-vars but checking for :secret instead
         (secrets (mapcar #'cdr
	                  (cl-remove-if-not (lambda (x) (eq (car x) :secret)) params)))
         (in-file (org-babel-temp-file "hurl" ".hurl"))
         (hurl-vars (cl-reduce
                     (lambda (acc elem)
                       (concat acc (format "--variable %s=%s" (car elem) (cdr elem)) " "))
                     vars :initial-value ""))
         ;; not sure how useful secrets are for an org-babel block but it was an easy lift to add this
         (hurl-secrets (cl-reduce
                        (lambda (acc elem)
                          (concat acc (format "--secret %s=%s" (car elem) (cdr elem)) " "))
                        secrets :initial-value "")))
    (with-temp-file in-file
      (insert body))
    (org-babel-eval
     (format "hurl %s %s %s" hurl-vars hurl-secrets (org-babel-process-file-name in-file)) "")))

(defun org-babel-hurl-var-to-hurl (var)
  "Convert an elisp var into a string of hurl source code
specifying a var of the same value."
  (format "%S" var))

(provide 'ob-hurl)
;;; ob-hurl.el ends here
