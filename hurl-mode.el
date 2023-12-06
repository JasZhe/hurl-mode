;;; hurl-mode.el --- Major mode for hurl
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen
;; Maintainer: Jason Zhen
;; Created: November 17, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.2"))
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
;; simple major mode for editing hurl files

;;; Code:
(require 'org-src)

(eval-when-compile
  (require 'rx))

(defgroup hurl nil
  "A major mode for editing Hurl files."
  :group 'tools)

(defgroup hurl-faces nil
  "Faces used in Hurl Mode"
  :group 'hurl
  :group 'faces)

(defface hurl-mode-method-face 
  '((t (:inherit font-lock-keyword-face)))
  "Face for HTTP method."
  :group 'hurl-faces)

(defface hurl-mode-url-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for urls"
  :group 'hurl-faces)

(defface hurl-mode-section-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for hurl sections"
  :group 'hurl-faces)

(defface hurl-mode-header-names-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for header names"
  :group 'hurl-faces)

(defface hurl-mode-header-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for header values"
  :group 'hurl-faces)

(defface hurl-mode-body-face
  '((t (:inherit font-lock-constant-face)))
  "Face for body values"
  :group 'hurl-faces)


(defconst hurl-mode--http-method-keywords
  '("GET" "HEAD" "POST" "PUT" "DELETE" "CONNECT" "OPTIONS" "TRACE" "PATCH"))
(defconst hurl-mode--http-method-regexp
  (rx-to-string `(: bol
                  (group (or ,@hurl-mode--http-method-keywords))
                  (group (+ not-newline)))))


(defconst hurl-mode--section-header-keywords
  '("[QueryStringParams]" "[FormParams]" "[MultipartFormData]" "[BasicAuth]" "[Cookies]" "[Asserts]" "[Captures]" "[Options]"))
(defconst hurl-mode--section-header-regexp
  (rx-to-string `(: bol
                  (or ,@hurl-mode--section-header-keywords))))


(defconst hurl-mode--expected-response-regexp
  (rx-to-string `(: bol
                  (group "HTTP")
                  (+ blank)
                  (group (= 3 digit)))))

(defconst hurl-mode--header-regexp
  (rx-to-string `(: bol
                  (group (* (or alpha "_" "-")) ":")
                  (+ blank)
                  (group (* any)))))


;; match ``` then anything except for ` then another ``` and end of line
;; simple but seems to work really well
(defconst hurl-mode-body-regexp "```[^`]*\n```$")

;; hacky bit to just abuse the org src block fontification for our purposes
;; TODO: could maybe simplify these down into a single function instead
(defun hurl-highlight-filter-xml (limit)
  "Highlight any xml region found in the text up to LIMIT."
  (when (re-search-forward "```xml[^`]*```$" limit t)
    (let ((code-start (+ (match-beginning 0) 7)) 
          (code-end (- (match-end 0) 4)))
      (save-match-data
        (ignore-errors (org-src-font-lock-fontify-block "xml" code-start code-end)))
      (goto-char (match-end 0)))))

(defun hurl-highlight-filter-json (limit)
  "Highlight any json region found in the text up to LIMIT."
  (when (re-search-forward "```json[^`]*```$" limit t)
    (let ((code-start (+ (match-beginning 0) 8))
          (code-end (- (match-end 0) 4)))
      (save-match-data
        (ignore-errors (org-src-font-lock-fontify-block "json" code-start code-end)))
      (goto-char (match-end 0)))))

(defun hurl-highlight-filter-graphql (limit)
  "Highlight any graphql region found in the text up to LIMIT."
  (when (and (fboundp 'graphql-mode) (re-search-forward "```graphql[^`]*```$" limit t))
    (let ((code-start (+ (match-beginning 0) 11))
          (code-end (- (match-end 0) 4)))
      (save-match-data
        (ignore-errors (org-src-font-lock-fontify-block "graphql" code-start code-end)))
      (goto-char (match-end 0)))))

;; stole this extend region stuff from haml mode with
;; a few modifications to make it work for our use case
(defun hurl-find-containing-block (re)
  "If point is inside a block matching RE, return (start . end) for the block."
  (save-excursion

    (let ((pos (point))
          start end)
      (beginning-of-line)
      (when (and
             (or (looking-at re)
                 (when (re-search-backward re nil t)
                   (looking-at re)))
             (let ((m-end (+ 1 (match-end 0))))
               (<= pos m-end)))
        (setq start (match-beginning 0)
              end (match-end 0)))
      (when (or start end)
        (cons start end)))))

(defun hurl-maybe-extend-region (extender)
  "Maybe extend the font lock region using EXTENDER.
With point at the beginning of the font lock region, EXTENDER is called.
If it returns a (START . END) pair, those positions are used to possibly
extend the font lock region."
  (let ((old-beg font-lock-beg)
        (old-end font-lock-end))
    (save-excursion
      (goto-char font-lock-beg)
      (let ((new-bounds (funcall extender)))
        (when new-bounds
          (setq font-lock-beg (min font-lock-beg (car new-bounds))
                font-lock-end (+ 0 (max font-lock-end (cdr new-bounds)))))))
    (or (/= old-beg font-lock-beg)
        (/= old-end font-lock-end))))

(defun hurl-extend-region-to-containing-block (re)
  "Extend the font-lock region to the smallest containing block matching RE."
  (hurl-maybe-extend-region
   (lambda ()
     (hurl-find-containing-block re))))

(defun hurl-extend-region-body ()
  "Extend the font-lock region to an enclosing filter."
  (hurl-extend-region-to-containing-block hurl-mode-body-regexp))

(defun hurl-extend-region-contextual ()
  (or
   (hurl-extend-region-body)
   (font-lock-extend-region-wholelines)
   (font-lock-extend-region-multiline)))


(defconst hurl-mode-keywords
  `((,hurl-mode--http-method-regexp (1 'hurl-mode-method-face) (2 'hurl-mode-url-face))
    (,hurl-mode--expected-response-regexp (1 'hurl-mode-method-face) (2 'hurl-mode-url-face))
    (,hurl-mode--header-regexp (1 'hurl-mode-header-names-face) (2 'hurl-mode-header-value-face))
    (hurl-highlight-filter-xml)
    (hurl-highlight-filter-json)
    (hurl-highlight-filter-graphql)
    (,hurl-mode-body-regexp (0 'hurl-mode-body-face))
    (,hurl-mode--section-header-regexp (0 'hurl-mode-section-face))))

(defconst hurl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

;;;###autoload
(define-derived-mode hurl-mode text-mode "Hurl"
  "Enable hurl mode"
  (setq-local font-lock-defaults '((hurl-mode-keywords)))
  (setq-local font-lock-multiline t)
  (setq-local font-lock-extend-region-functions '(hurl-extend-region-contextual))
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  )

;; so we don't get auto ` pairing if smartparens mode exists
(eval-after-load 'smartparens-mode
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'hurl-mode "`" nil :actions nil)))

(defun hurl-mode-send-request (arg)
  "Simple thin wrapper which sends the contents of the current file to hurl.
With one prefix arg, execute with the --test option.
With two (or more) prefix args, prompt for arbitrary additional options to hurl command"
  (interactive "P")
  (let ((args (cond ((equal arg '(4)) "--test")
                    ((equal arg '(16)) (read-string "additional cli options: "))
                    (t ""))))
    (async-shell-command (format "hurl %s %s" args (buffer-file-name)))))

(setq hurl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'hurl-mode-send-request)
    map))


(provide 'hurl-mode)
