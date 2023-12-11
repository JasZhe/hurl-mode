;;; hurl-mode.el --- Major mode for hurl
;;
;; Copyright (C) 2023 Jason Zhen
;;
;; Author: Jason Zhen <jaszhe@gmail.com>
;; Maintainer: Jason Zhen <jaszhe@gmail.com>
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

(defface hurl-mode-pred-negation-face
  '((t (:inherit font-lock-negation-char-face)))
  "Face for hurl predicate not prefix"
  :group 'hurl-faces)

(defface hurl-mode-pred-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for hurl predicates"
  :group 'hurl-faces)

(defface hurl-mode-pred-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl predicate arguments"
  :group 'hurl-faces)

(defface hurl-mode-body-face
  '((t (:inherit font-lock-string-face)))
  "Face for body values"
  :group 'hurl-faces)

(defface hurl-mode-template-face
  '((t (:inherit font-lock-type-face)))
  "Face for templates"
  :group 'hurl-faces)


(defconst hurl-mode--http-method-keywords
  '("GET" "HEAD" "POST" "PUT" "DELETE" "CONNECT" "OPTIONS" "TRACE" "PATCH"))
(defconst hurl-mode--http-method-regexp
  (rx-to-string `(: bol
                  (group (or ,@hurl-mode--http-method-keywords))
                  (group (+ not-newline)))))

(defconst hurl-mode--template-regexp
  (rx-to-string `(: "{{" (* (or alnum "-" "_")) "}}")))

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

(defconst hurl-mode--arg-queries
  '("header" "cookie" "xpath" "jsonpath" "regex" "variable"))

(defconst hurl-mode--no-arg-queries
  '("status" "url" "body" "duration" "sha256" "md5" "bytes"))

(defconst hurl-mode--certificate-attrs
  '("Subject" "Issuer" "Start-Date" "Expire-Date" "Serial-Number"))

(defconst hurl-mode--predicates
  '("==" "!=" ">" ">=" "<" "<=" "startsWith" "endsWith" "contains" "includes" "matches" "exists" "isBoolean" "isCollection" "isDate" "isEmpty" "isFloat" "isInteger" "isString"))

(defconst hurl-mode--no-arg-filters
  '("count" "daysAfterNow" "daysBeforeNow" "decode" "format" "htmlEscape" "htmlUnescape" "toDate" "toInt" "urlDecode" "urlEncode"))

(defconst hurl-mode--single-int-arg-filters '("nth"))
(defconst hurl-mode--single-string-arg-filters '("regex" "split" "xpath"))
(defconst hurl-mode--double-string-arg-filters `("replace"))


(defun hurl-fontify-src-blocks (limit)
  "Fontifies body blocks for detected languages.
Essentially creates a temporary buffer with the specified major mode and inserts the request body text into it. Then we can copy the font properties from that temporary buffer to our real hurl buffer.

This is basically a simplified version of the bomination of org-fontify-meta-lines-and-blocks-1 and org-src-font-lock-fontify-block
since we don't need to care about the other block types in org."
  (when (re-search-forward
         (rx bol (group "```"
                        (group (zero-or-more (any "a-zA-Z")))
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
      (when (re-search-forward (rx-to-string `(group bol "```")))
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
            (if (fboundp lang-mode)
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
                  ))
              ;; otherwise if lang not found just fontify with body face
              (add-text-properties block-start block-end `(font-lock-fontified t font-lock-multiline t face hurl-mode-body-face))
              ()
              )))
        ;; this t is really important
        ;; see: https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
        t))))


(defun hurl-fontify-no-arg-queries (limit)
  (save-match-data
    (when (setq next (re-search-forward (rx-to-string `(: bol (group (or ,@hurl-mode--no-arg-queries)))) limit t))
      (add-text-properties (match-beginning 0) next '(font-lock-fontified t face hurl-mode-query-face))
      t))
  )

(defun hurl-fontify-arg-queries (limit)
  (save-match-data
    (when (setq next (re-search-forward (rx-to-string `(: bol (group (or ,@hurl-mode--arg-queries)))) limit t))
      (add-text-properties (match-beginning 0) next '(font-lock-fontified t face hurl-mode-query-face))
      (let ((filter-arg-rx (rx-to-string `(minimal-match (: "\"" (+ (not "\"") ) "\"")))))
        (when-let (arg-pos (re-search-forward filter-arg-rx (save-excursion (end-of-line) (point)) t))
          (add-text-properties (match-beginning 0) arg-pos
                               '(font-lock-fontified t face font-lock-string-face))))
      t))
  )

(defconst hurl--query-regexp
  `(or (group (or ,@hurl-mode--no-arg-queries))
       (: (group (or ,@hurl-mode--arg-queries)) blank
          (group (minimal-match (: "\"" (+ (not "\"") ) "\""))))
       (: (group "certificate") blank
          (group (or ,@hurl-mode--certificate-attrs))
          ))
  )

;; need to go to beginning of line so we can match the other filters with other arg requirements
(defun hurl--string-arg-filter-matcher ()
  (list
   (rx-to-string `(: (group (or ,@hurl-mode--single-string-arg-filters))
                   blank
                   (group (minimal-match (: "\"" (+ (not "\"") ) "\"")))))
   nil
   '(beginning-of-line)
   '(1 'hurl-mode-filter-face t)
   '(2 'hurl-mode-query-arg-face t)
   )
  )

(defun hurl--double-string-arg-filter-matcher ()
  (list
    (rx-to-string `(: (group (or ,@hurl-mode--double-string-arg-filters)) blank
                    (group (minimal-match (: "\"" (+ (not "\"") ) "\""))) blank
                    (group (minimal-match (: "\"" (+ (not "\"") ) "\"")))))
    nil
    '(beginning-of-line)
    '(1 'hurl-mode-filter-face t)
    '(2 'hurl-mode-query-arg-face t)
    '(3 'hurl-mode-query-arg-face t)
    )
  )

(defun hurl--int-arg-filter-matcher ()
  (list
    (rx-to-string `(: (group (or ,@hurl-mode--single-int-arg-filters))
                    blank
                    (group (+ digit))))
    nil
    '(beginning-of-line)
    '(1 'hurl-mode-filter-face t)
    '(2 'font-lock-constant-face t)
    )
  )

(defun hurl--no-arg-filter-matcher ()
  (list
   (rx-to-string `(: (group (or ,@hurl-mode--no-arg-filters))))
   nil
   '(beginning-of-line)
   '(0 'hurl-mode-filter-face t)
   )
  )

;; anchored highlighter for captures
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
(defun variable-and-capture-matcher ()
  (list
   ;; variable name as anchor
   (rx-to-string `(: bol (group (* (or alnum "_" "-")) ":") blank
                   ;; 0 or more queries with their arguments
                   (? ,hurl--query-regexp)))
   '(1 'hurl-mode-variable-face t t)
   '(2 'hurl-mode-query-face t t) ;; noarg
   '(3 'hurl-mode-query-face t t) ;; arg
   '(4 'hurl-mode-query-arg-face t t)
   '(5 'hurl-mode-query-face t t) ;; certificate
   '(6 'hurl-mode-query-arg-face t t)
   (hurl--string-arg-filter-matcher)
   (hurl--double-string-arg-filter-matcher)
   (hurl--int-arg-filter-matcher)
   (hurl--no-arg-filter-matcher)
   ;; fontify the rest for non capture type variables
   (list ".*" nil nil '(0 'font-lock-string-face nil))
   )
  )

(defun assert-matcher ()
  (list
   (rx-to-string `(: bol ,hurl--query-regexp))
   '(1 'hurl-mode-query-face t t) ;; noarg
   '(2 'hurl-mode-query-face t t) ;; arg
   '(3 'hurl-mode-query-arg-face t t)
   '(4 'hurl-mode-query-face t t) ;; certificate
   '(5 'hurl-mode-query-arg-face t t)
   (hurl--string-arg-filter-matcher)
   (hurl--double-string-arg-filter-matcher)
   (hurl--int-arg-filter-matcher)
   (hurl--no-arg-filter-matcher)
   (list
    (rx-to-string `(: (? (group "not")) (* blank) (group (or ,@hurl-mode--predicates)) (* blank) (? (group (* (category ascii))))))
    nil nil
    '(1 'font-lock-negation-char-face t t)
    '(2 'font-lock-keyword-face t t)
    '(3 'font-lock-constant-face t t)
    )
   )
  )

(defconst hurl-mode-keywords
  (list
   `(,hurl-mode--http-method-regexp (1 'hurl-mode-method-face) (2 'hurl-mode-url-face))
   `(,hurl-mode--template-regexp (0 'hurl-mode-template-face t t))
   (list (rx-to-string `(: (or (not "\\") line-start) "#" (* any))) '(0 'font-lock-comment-face t t))
   `(,hurl-mode--expected-response-regexp (1 'hurl-mode-method-face) (2 'hurl-mode-url-face))
   `(,hurl-mode--section-header-regexp (0 'hurl-mode-section-face))
   '(hurl-fontify-src-blocks)
   (variable-and-capture-matcher)
   (assert-matcher)
   )
  )

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
