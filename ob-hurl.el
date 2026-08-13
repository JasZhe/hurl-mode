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
(require 'seq)
(require 'hurl-mode)

;; Declared here so the `let' in the optional ob-async integration is dynamic.
(defvar async-prompt-for-password)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("hurl" . "tmp"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:hurl '())

(defvar org-babel-hurl--async-process-buffer-name "*ob-hurl-process*"
  "Name of the process buffer used for async hurl Babel execution.")

(defcustom org-babel-hurl-keep-debug-artifacts t
  "When non-nil, retain the temporary Hurl input file after async execution.

The process log, verbose output, raw output, and formatted response buffers are
always retained until the next Hurl request.  This option additionally keeps
the generated input file so an async invocation can be reproduced."
  :type 'boolean
  :group 'org-babel)

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


(defun org-babel-hurl--secret-args-from-params ()
  "Parse repeated :secret entries from raw source block PARAMS.
Return a string of hurl --secret arguments."
  ;; HACK: babel normally dedupes non :var params before passing it to our execute function
  ;; This retrieves the full raw org src block params
  (when-let ((params (or (org-element-property :parameters (org-element-at-point))
                         (when (bound-and-true-p org-babel-current-src-block-location)
                           (save-excursion
                             (goto-char org-babel-current-src-block-location)
                             (org-element-property :parameters (org-element-at-point)))))))
    (mapconcat
     #'identity
     (delq nil
           (mapcar
            (lambda (param)
              (when (string-match "\\`secret[ \t]+\\(.+\\)\\'" param)
                (format "--secret %s" (match-string 1 param))))
            (org-babel-balanced-split params '((?\s ?\t) . ?:))))
     " ")))

(defun org-babel-hurl--truthy-param-p (value)
  "Return non-nil when VALUE enables a boolean Babel header argument."
  (member value '("yes" "true" "t" yes true t)))

(defun org-babel-hurl--asynchurl-param-p ()
  "Return non-nil when the raw source block params include enabled :asynchurl."
  (let ((params (org-element-property :parameters (org-element-at-point))))
    (when params
      (seq-some
       (lambda (param)
         (or (string= param "asynchurl")
             (and (string-match "\\`asynchurl\\(?:[ \t]+\\(.+\\)\\)?\\'" param)
                  (let ((value (match-string 1 param)))
                    (or (null value)
                        (org-babel-hurl--truthy-param-p value))))))
       (org-babel-balanced-split params '((?\s ?\t) . ?:))))))

(defun org-babel-hurl--params-asynchurl-p (params)
  "Return non-nil when PARAMS include enabled :asynchurl."
  (org-babel-hurl--truthy-param-p (cdr (assq :asynchurl params))))

(defun org-babel-hurl--command (args in-file)
  "Build the hurl command string from ARGS and IN-FILE."
  ;; Keep the verbose stream in the process output: it contains `* Captures:'
  ;; which `hurl-response--parse-and-filter-output' persists to .hurl-variables.
  (format "hurl --very-verbose --output - %s %s 2>&1"
          args
          (shell-quote-argument (org-babel-process-file-name in-file))))

(defun org-babel-hurl--error-output ()
  "Return only Hurl's error report from the verbose output buffer."
  (with-current-buffer hurl-response--output-buffer-name
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^error:" nil t)
          (buffer-substring-no-properties (line-beginning-position) (point-max))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun org-babel-hurl--formatted-response-output ()
  "Return only the formatted response body from hurl verbose output."
  (condition-case nil
      (let* ((resp-head (hurl-response--get-response-head))
             (resp-raw (hurl-response--raw-response))
             (resp (hurl-response--preprocess-response))
             (formatted-resp (hurl-response--formatted-response resp-head resp)))
        ;; `async-start' serializes strings with text properties as lists.
        ;; Babel then treats the result as a table rather than response text.
        (substring-no-properties
         (concat
          (if (string-match-p
               "^Bytes <[0-9a-fA-F]+\\(\\.\\.\\.\\)?>$" formatted-resp)
              resp-raw
            formatted-resp)
          "\n")))
    (error
     (with-current-buffer hurl-response--output-buffer-name
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun org-babel-hurl--result-output (exit-code)
  "Return the best available hurl output for Org Babel results.
EXIT-CODE is the hurl process exit code."
  (if (zerop exit-code)
      (org-babel-hurl--formatted-response-output)
    (org-babel-hurl--error-output)))

(defun org-babel-hurl--ob-async-around (original &rest args)
  "Avoid false password prompts for Hurl blocks run through `ob-async'."
  (let ((info (or (nth 2 args) (org-babel-get-src-block-info))))
    (if (and (equal (nth 0 info) "hurl")
             (assq :async (nth 2 info)))
        ;; `async-read-from-client' tests raw result chunks against TRAMP's
        ;; password regexp.  JSON output can match it despite no prompt.
        (let ((async-prompt-for-password nil))
          (apply original args))
      (apply original args))))

;;;###autoload
(defun org-babel-hurl-enable-ob-async-compatibility ()
  "Enable Hurl-specific compatibility with the optional `ob-async' package.

This preserves capture writing in the child Emacs and prevents `ob-async' from
mistaking Hurl response data for an interactive password prompt."
  (interactive)
  (unless (require 'ob-async nil t)
    (user-error "ob-async is not available"))
  (unless (advice-member-p #'org-babel-hurl--ob-async-around
                           #'ob-async-org-babel-execute-src-block)
    (advice-add #'ob-async-org-babel-execute-src-block :around
                #'org-babel-hurl--ob-async-around)))

(defun org-babel-hurl--log-async (message &rest args)
  "Log MESSAGE with ARGS to the async hurl process buffer."
  (with-current-buffer (get-buffer-create org-babel-hurl--async-process-buffer-name)
    (goto-char (point-max))
    (insert (apply #'format (concat message "\n") args))))

(defun org-babel-hurl--insert-async-result (source-buffer source-marker result
                                                         result-params info)
  "Insert async hurl RESULT back into SOURCE-BUFFER at SOURCE-MARKER."
  (unwind-protect
      (condition-case err
          (if (not (buffer-live-p source-buffer))
              (message "ob-hurl async finished, but source buffer no longer exists")
            (with-current-buffer source-buffer
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char source-marker)
                  (org-babel-insert-result result result-params info nil "hurl")))))
        (error
         (org-babel-hurl--log-async "result insertion failed: %S" err)
         (message "ob-hurl async result insertion failed: %S" err)))
    (set-marker source-marker nil)))

(defun org-babel-hurl--execute-sync (cmd)
  "Run CMD synchronously and return cleaned hurl output for Babel."
  (ignore-errors (kill-buffer hurl-response--output-buffer-name))
  (ignore-errors (kill-buffer hurl-response--raw-output-buffer-name))
  (get-buffer-create hurl-response--output-buffer-name)
  (let ((exit-code (call-process "/bin/sh" nil hurl-response--output-buffer-name nil "-c" cmd)))
    (with-current-buffer hurl-response--output-buffer-name
      (ansi-color-apply-on-region (point-min) (point-max)))
    (condition-case err
        (with-current-buffer hurl-response--output-buffer-name
          (hurl-response--parse-and-filter-output))
      (error
       (message "ob-hurl response parsing failed: %S" err)))
    (org-babel-hurl--result-output exit-code)))

(defun org-babel-hurl--execute-async (cmd result-params info source-buffer source-marker in-file)
  "Run CMD asynchronously and insert its output into the source block result.
RESULT-PARAMS and INFO are passed to `org-babel-insert-result'.
SOURCE-BUFFER and SOURCE-MARKER identify the source block position.  IN-FILE is
retained when `org-babel-hurl-keep-debug-artifacts' is non-nil."
  (ignore-errors (kill-buffer hurl-response--output-buffer-name))
  (ignore-errors (kill-buffer hurl-response--raw-output-buffer-name))
  (get-buffer-create hurl-response--output-buffer-name)
  (let* ((process-buffer (get-buffer-create org-babel-hurl--async-process-buffer-name))
         (proc (start-file-process "ob-hurl" process-buffer "/bin/sh" "-c"
                                   cmd)))
    (org-babel-hurl--log-async
     "started pid=%s input=%s\n  process=%s\n  verbose=%s\n  raw=%s\n  response=%s\n  cmd=%s"
     (process-id proc) in-file (buffer-name process-buffer)
     hurl-response--output-buffer-name hurl-response--raw-output-buffer-name
     hurl-response--buffer-name cmd)
    (set-process-query-on-exit-flag proc nil)
    (set-process-filter
     proc
     #'hurl-response--verbose-filter)
    (set-process-sentinel
     proc
     (lambda (process _event)
       (unless (process-live-p process)
         (unwind-protect
             (progn
               (condition-case err
                   (progn
                     (with-current-buffer (get-buffer-create hurl-response--output-buffer-name)
                       (ansi-color-apply-on-region (point-min) (point-max)))
                     (with-current-buffer hurl-response--output-buffer-name
                       (hurl-response--parse-and-filter-output))
                     (display-buffer hurl-response--buffer-name))
                 (error
                  (org-babel-hurl--log-async "response parsing failed: %S" err)
                  (message "ob-hurl async response parsing failed: %S" err)))
               (let ((result (org-babel-hurl--result-output (process-exit-status process))))
                 (org-babel-hurl--log-async
                  "finished pid=%s status=%s exit=%s bytes=%s"
                  (process-id process)
                  (process-status process)
                  (process-exit-status process)
                  (length result))
                 (run-at-time
                  0 nil #'org-babel-hurl--insert-async-result
                  source-buffer source-marker result result-params info)))
           (when (and (not org-babel-hurl-keep-debug-artifacts)
                      (file-exists-p in-file))
             (delete-file in-file))))))
    (format "hurl async process started: %s" (process-id proc))))

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
  (let* ((async (or (org-babel-hurl--params-asynchurl-p params)
                    (org-babel-hurl--asynchurl-param-p)))
         (params (assq-delete-all :asynchurl (copy-sequence params)))
         (info (org-babel-get-src-block-info))
         (processed-params (org-babel-process-params params))
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (secret-args (org-babel-hurl--secret-args-from-params))
         (in-file (org-babel-temp-file "hurl" ".hurl"))
         (hurl-vars (cl-reduce
                     (lambda (acc elem)
                       (concat acc (format "--variable %s=%s" (car elem) (cdr elem)) " "))
                     vars :initial-value ""))
         ;; not sure how useful secrets are for an org-babel block but it was an easy lift to add this
         (hurl-secret-files-secrets
          (mapconcat
           (lambda (secret) (concat " --secret " (shell-quote-argument secret)))
           (hurl-mode--read-secrets-files)))
         (hurl-secrets (concat hurl-secret-files-secrets
                               " "
                               secret-args
                               " "))
         (args (concat hurl-vars hurl-secrets
                       (when (file-exists-p hurl-global-variables-file)
                         (concat " --variables-file " hurl-global-variables-file))
                       (when (file-exists-p hurl-variables-file)
                         (concat " --variables-file " hurl-variables-file))))
         (cmd (org-babel-hurl--command args in-file))
         (result-params (cdr (assq :result-params processed-params)))
         (source-buffer (current-buffer))
         (source-marker (copy-marker (point-marker))))
    (with-temp-file in-file
      (insert body))
    (if async
        (org-babel-hurl--execute-async cmd result-params info source-buffer source-marker in-file)
      (org-babel-hurl--execute-sync cmd))))

(defun org-babel-hurl-var-to-hurl (var)
  "Convert an elisp var into a string of hurl source code
specifying a var of the same value."
  (format "%S" var))

(provide 'ob-hurl)
;;; ob-hurl.el ends here
