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

;; this is just for org-src-get-lang-mode
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
  '((t (:inherit font-lock-preprocessor-face)))
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

(defface hurl-mode-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for hurl variables"
  :group 'hurl-faces)

(defface hurl-mode-variable-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl variable values"
  :group 'hurl-faces)

(defface hurl-mode-query-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for hurl query keywords"
  :group 'hurl-faces)

(defface hurl-mode-query-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl query arguments"
  :group 'hurl-faces)

(defface hurl-mode-filter-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for hurl filters"
  :group 'hurl-faces)

(defface hurl-mode-filter-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl filter arguments"
  :group 'hurl-faces)

(defface hurl-mode-pred-face
  '((t (:inherit font-lock-operator-face)))
  "Face for hurl filter arguments"
  :group 'hurl-faces)

(defface hurl-mode-pred-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl filter arguments"
  :group 'hurl-faces)

(defface hurl-mode-body-face
  '((t (:inherit font-lock-string-face)))
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

(defconst hurl-mode--arg-queries
  '("header" "certificate" "cookie" "xpath" "jsonpath" "regex" "variable"))

(defconst hurl-mode--no-arg-queries
  '("status" "url" "body" "duration" "sha256" "md5" "bytes"))

(defconst hurl-mode--certificate-attrs
  `("Subject" "Issuer" "Start-Date" "Expire-Date" "Serial-Number"))


(defconst hurl-mode--predicates
  '("==" "!=" ">" ">=" "<" "<=" "startsWith" "endsWith" "contains" "includes" "matches" "exists" "isBoolean" "isCollection" "isDate" "isEmpty" "isFloat" "isInteger" "isString"))

(defconst hurl-mode--no-arg-filters
  '("count" "daysAfterNow" "daysBeforeNow" "decode" "format" "htmlEscape" "htmlUnescape" "toDate" "toInt" "urlDecode" "urlEncode"))

(defconst hurl-mode--single-int-arg-filters '("nth"))
(defconst hurl-mode--single-string-arg-filters '("regex" "split" "xpath"))
(defconst hurl-mode--double-string-arg-filters `("replace"))

(defconst hurl-mode--capture-regexp
  (rx-to-string `(: bol
                    ;; variable name
                    (group (* (or alpha "_" "-")) ":")
                    (* blank)
                    ;; query
                    (group (or (or ,@hurl-mode--no-arg-queries)
                               (: (or ,@hurl-mode--arg-queries))
                               ))
                    (* blank)
                    ;; optional query param
                    (? (group (or (: "\"" (* (or (not "\"") "\\\"") ) "\"")
                                  (or ,@hurl-mode--certificate-attrs))))
                    (* blank)
                    ;; optional filter
                    (* (: (group (or (or ,@hurl-mode--no-arg-filters)
                                     (or ,@hurl-mode--single-int-arg-filters)
                                     (or ,@hurl-mode--single-string-arg-filters)
                                     (or ,@hurl-mode--double-string-arg-filters)))
                          (* blank)
                          ;; optional filter params, single or double string args, single int
                          (? (group (* digit)))
                          (? (group (: "\"" (* alnum) "\"")))
                          (* blank)
                          (? (group (: "\"" (* alnum) "\""))))))))

(defconst hurl-mode--assert-regexp
  (rx-to-string `(: bol
                    ;; query
                    (group (or (or ,@hurl-mode--no-arg-queries)
                               (or ,@hurl-mode--arg-queries)))
                    (* blank)
                    ;; optional query arg
                    (? (group (or (: "\"" (* (or (not "\"") "\\\"") ) "\"")
                                  ,@hurl-mode--certificate-attrs)))
                    (* blank)
                    ;; optional filter
                    (* (: (group (or ,@hurl-mode--no-arg-filters
                                     ,@hurl-mode--single-int-arg-filters
                                     ,@hurl-mode--single-string-arg-filters
                                     ,@hurl-mode--double-string-arg-filters))
                          (* blank)
                          ;; optional filter args, single int, single or double string
                          (? (group (* digit)))
                          (? (group (: "\"" (* (category ascii)) "\"")))
                          (* blank)
                          (? (group (: "\"" (* (category ascii)) "\"")))))
                    ;; predicate
                    (group (or ,@hurl-mode--predicates))
                    ;; optional predicate arg
                    (? (group (* (category ascii))))
                    )))


(defun hurl-fontify-single-string-filters ()
  "Loop through a line for each occurrence of filter + string arg"
  (while (setq next (and (not (eq (point) (save-excursion (end-of-line) (point))))
                         (re-search-forward
                          (rx-to-string `(group (or ,@hurl-mode--single-string-arg-filters)))
                          (save-excursion (end-of-line) (point)) t)))
    (add-text-properties (match-beginning 0) next '(font-lock-fontified t face hurl-mode-filter-face))
    (let ((filter-arg-rx (rx-to-string `(minimal-match (: "\"" (+ (not "\"") ) "\"")))))
      (when-let (arg-pos (re-search-forward filter-arg-rx (save-excursion (end-of-line) (point)) t))
        (add-text-properties (match-beginning 0) arg-pos
                             '(font-lock-fontified t face font-lock-string-face))))))

(defun hurl-fontify-double-string-filters ()
  "Loop through a line for each occurrence of filter + string arg + string arg"
  (while (setq next (and (not (eq (point) (save-excursion (end-of-line) (point))))
                         (re-search-forward
                          (rx-to-string `(group (or ,@hurl-mode--double-string-arg-filters)))
                          (save-excursion (end-of-line) (point)) t)))
    (add-text-properties (match-beginning 0) next '(font-lock-fontified t face hurl-mode-filter-face))
    (let ((filter-arg-rx (rx-to-string `(minimal-match (: "\"" (+ (not "\"") ) "\"")))))
      (when-let (arg-pos-1
                 (re-search-forward filter-arg-rx (save-excursion (end-of-line) (point)) t))
        (add-text-properties (match-beginning 0) arg-pos-1
                             '(font-lock-fontified t face font-lock-regexp-face))
        (when-let (arg-pos-2
                   (re-search-forward filter-arg-rx (save-excursion (end-of-line) (point)) t))
          (add-text-properties (match-beginning 0) arg-pos-2
                               '(font-lock-fontified t face font-lock-string-face)))))))


(defun hurl-fontify-single-int-filters ()
  "Loop through a line for each occurrence of filter + int arg"
  (while (setq next (and (not (eq (point) (save-excursion (end-of-line) (point))))
                         (re-search-forward
                          (rx-to-string `(group (or ,@hurl-mode--single-int-arg-filters)))
                          (save-excursion (end-of-line) (point)) t)))
    (add-text-properties (match-beginning 0) next '(font-lock-fontified t face hurl-mode-filter-face))
    (let ((filter-arg-rx (rx-to-string `(+ digit))))
      (when-let (arg-pos (re-search-forward filter-arg-rx (save-excursion (end-of-line) (point)) t))
        (add-text-properties (match-beginning 0) arg-pos
                             '(font-lock-fontified t face font-lock-constant-face))))))


(defun hurl-fontify-no-arg-filters ()
  "Loop through a line for each occurrence of no arg filter"
  (while (setq next (and (not (eq (point) (save-excursion (end-of-line) (point))))
                         (re-search-forward
                          (rx-to-string `(group (or ,@hurl-mode--no-arg-filters)))
                          (save-excursion (end-of-line) (point)) t)))
    (message "2: pos %s next %s substr: %s" (match-beginning 0) next
             (buffer-substring-no-properties (match-beginning 0) next))
    (add-text-properties (match-beginning 0) next '(font-lock-fontified t face hurl-mode-filter-face))))


(defun hurl-fontify-filters ()
  ;; need the save excursions to restart fontification from the beginning of the line for each type of filter
  (save-excursion
    (hurl-fontify-single-string-filters))
  (save-excursion
    (hurl-fontify-double-string-filters))
  (save-excursion
    (hurl-fontify-single-int-filters))
  (save-excursion
    (hurl-fontify-no-arg-filters))
  )

(defun hurl-fontify-asserts (limit)
  (cond ((setq next (re-search-forward
                     (rx-to-string `(: bol (group (or ,@hurl-mode--no-arg-queries))))
                     limit t))
         (setq pos (match-beginning 0))
         (message "1: limit %s pos %s next %s" limit (match-beginning 0) next)
         (add-text-properties pos next '(font-lock-fontified t font-lock-multiline t face hurl-mode-query-face))
         (hurl-fontify-filters)
         t))
  )

(defun hurl-fontify-src-blocks (limit)
  "Fontifies body blocks for detected languages.
Essentially creates a temporary buffer with the specified major mode and inserts the request body text into it. Then we can copy the font properties from that temporary buffer to our real hurl buffer.

This is basically a simplified version of the bomination of org-fontify-meta-lines-and-blocks-1 and org-src-font-lock-fontify-block
since we don't need to care about the other block types in org."
  (when (re-search-forward
         (rx bol (group "```"
                        (group (one-or-more (any "a-zA-Z")))
                        (group (group (zero-or-more (not (any " \t\n"))))
                               (zero-or-more (any " \t"))
                               (group (zero-or-more any)))))
         limit t)
    (let ((beg (match-beginning 0))
          (end-of-beginline (match-end 0))
          (block-start (match-end 0))
          (block-end nil)
          (lang (match-string 2))
          end-of-endline)
      (when (re-search-forward
             (rx-to-string `(group bol (or (seq (one-or-more "*") space)
                                           (seq bol "```" (zero-or-more any))))))
        (setq block-end (match-beginning 0))
        (setq end-of-endline (match-end 0))
        (add-text-properties
         beg end-of-endline '(font-lock-fontified t font-lock-multiline t))

        ;; this save match data is really important or else we only fontify a single block
        (save-match-data
          (remove-text-properties block-start block-end '(face nil))
          (let ((string (buffer-substring-no-properties block-start block-end))
                (lang-mode (org-src-get-lang-mode lang))
                (my-buffer (current-buffer)))
            (when (fboundp lang-mode)
              ;; for debugging purposes having a real (hidden) buffer to see what's being fontified is nice (org does this)
              ;; but we can easily switch to just with-temp-buffer if we no longer want this
              (with-current-buffer (get-buffer-create (format " *hurl fontify buffer:%s*" lang-mode))
                (erase-buffer)
                (insert string " ")
                (funcall lang-mode)
                (font-lock-ensure)
                (let ((pos (point-min)) next)
                  (while (setq next (next-property-change pos))
                    (dolist (prop (append '(font-lock-face face) font-lock-extra-managed-props))
                      (let ((new-prop (get-text-property pos prop)))
                        (when (not (eq prop 'invisible))
                          (put-text-property (+ block-start (1- pos)) (1- (+ block-start next)) prop new-prop my-buffer)))
                      )
                    (setq pos next))
                  )))))
        ;; this t is really important
        ;; see: https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
        t))))


(defconst hurl-mode-keywords
  `((,hurl-mode--http-method-regexp (1 'hurl-mode-method-face)
     (2 'hurl-mode-url-face))
    (,hurl-mode--expected-response-regexp (1 'hurl-mode-method-face)
                                          (2 'hurl-mode-url-face))
    (,hurl-mode--header-regexp (1 'hurl-mode-header-names-face)
                               (2 'hurl-mode-header-value-face))
    (hurl-fontify-asserts)
    (hurl-fontify-src-blocks)
    (,hurl-mode--section-header-regexp (0 'hurl-mode-section-face))))

(defconst hurl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

;;;###autoload
(define-derived-mode hurl-mode text-mode "Hurl"
  "Enable hurl mode"
  (setq-local font-lock-defaults '(hurl-mode-keywords t nil nil backward-paragraph))
  (setq-local font-lock-multiline t)
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
