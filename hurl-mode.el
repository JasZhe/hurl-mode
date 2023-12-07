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


;; match ``` then anything except for ` then another ``` and end of line
;; simple but seems to work really well
(defconst hurl-mode-body-regexp "```[^`]*\n```$")



;; removed the last few lines that cause a org-element-at-point warning
;; ignore-errors didn't stop the warnings from popping up so this worked instead
(defun hurl--org-src-font-lock-fontify-block (lang start end)
  "Fontify code block between START and END using LANG's syntax.
This function is called by Emacs' automatic fontification, as long
as `org-src-fontify-natively' is non-nil."
  (let ((modified (buffer-modified-p)) native-tab-width)
    (remove-text-properties start end '(face nil))
    (let ((lang-mode (org-src-get-lang-mode lang)))
      (when (fboundp lang-mode)
        (let ((string (buffer-substring-no-properties start end))
	      (org-buffer (current-buffer)))
	  (with-current-buffer
	      (get-buffer-create
	       (format " *org-src-fontification:%s*" lang-mode))
	    (let ((inhibit-modification-hooks nil))
	      (erase-buffer)
	      ;; Add string and a final space to ensure property change.
	      (insert string " "))
	    (unless (eq major-mode lang-mode) (funcall lang-mode))
            (setq native-tab-width tab-width)
            (font-lock-ensure)
	    (let ((pos (point-min)) next)
	      (while (setq next (next-property-change pos))
	        ;; Handle additional properties from font-lock, so as to
	        ;; preserve, e.g., composition.
                ;; FIXME: We copy 'font-lock-face property explicitly because
                ;; `font-lock-mode' is not enabled in the buffers starting from
                ;; space and the remapping between 'font-lock-face and 'face
                ;; text properties may thus not be set.  See commit
                ;; 453d634bc.
	        (dolist (prop (append '(font-lock-face face) font-lock-extra-managed-props))
		  (let ((new-prop (get-text-property pos prop)))
                    (when new-prop
                      (if (not (eq prop 'invisible))
		          (put-text-property
		           (+ start (1- pos)) (1- (+ start next)) prop new-prop
		           org-buffer)
                        ;; Special case.  `invisible' text property may
                        ;; clash with Org folding.  Do not assign
                        ;; `invisible' text property directly.  Use
                        ;; property alias instead.
                        (let ((invisibility-spec
                               (or
                                ;; ATOM spec.
                                (and (memq new-prop buffer-invisibility-spec)
                                     new-prop)
                                ;; (ATOM . ELLIPSIS) spec.
                                (assq new-prop buffer-invisibility-spec))))
                          (with-current-buffer org-buffer
                            ;; Add new property alias.
                            (unless (memq 'org-src-invisible
                                          (cdr (assq 'invisible char-property-alias-alist)))
                              (setq-local
                               char-property-alias-alist
                               (cons (cons 'invisible
			                   (nconc (cdr (assq 'invisible char-property-alias-alist))
                                                  '(org-src-invisible)))
		                     (remove (assq 'invisible char-property-alias-alist)
			                     char-property-alias-alist))))
                            ;; Carry over the invisibility spec, unless
                            ;; already present.  Note that there might
                            ;; be conflicting invisibility specs from
                            ;; different major modes.  We cannot do much
                            ;; about this then.
                            (when invisibility-spec
                              (add-to-invisibility-spec invisibility-spec))
                            (put-text-property
		             (+ start (1- pos)) (1- (+ start next))
                             'org-src-invisible new-prop
		             org-buffer)))))))
	        (setq pos next)))
            (set-buffer-modified-p nil)))))
    ;; Add Org faces.
    (let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
      (when (or (facep src-face) (listp src-face))
        (font-lock-append-text-property start end 'face src-face))
      (font-lock-append-text-property start end 'face 'org-block))
    (add-text-properties
     start end
     '(font-lock-fontified t fontified t font-lock-multiline t))
    ))



;; hacky bit to just abuse the org src block fontification for our purposes
;; TODO: could maybe simplify these down into a single function instead
(defun hurl-highlight-filter-xml (limit)
  "Highlight any xml region found in the text up to LIMIT."
  (when (re-search-forward "```xml[^`]*```$" limit t)
    (let ((code-start (+ (match-beginning 0) 7)) 
          (code-end (- (match-end 0) 4)))
      (save-match-data
        (hurl--org-src-font-lock-fontify-block "xml" code-start code-end))
      (goto-char (match-end 0)))))

(defun hurl-highlight-filter-json (limit)
  "Highlight any json region found in the text up to LIMIT."
  (when (re-search-forward "```json[^`]*```$" limit t)
    (let ((code-start (+ (match-beginning 0) 8))
          (code-end (- (match-end 0) 4)))
      (save-match-data
        (hurl--org-src-font-lock-fontify-block "json" code-start code-end))
      (goto-char (match-end 0)))))

(defun hurl-highlight-filter-graphql (limit)
  "Highlight any graphql region found in the text up to LIMIT."
  (when (and (fboundp 'graphql-mode) (re-search-forward "```graphql[^`]*```$" limit t))
    (let ((code-start (+ (match-beginning 0) 11))
          (code-end (- (match-end 0) 4)))
      (save-match-data
        (hurl--org-src-font-lock-fontify-block "graphql" code-start code-end))
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
  `((,hurl-mode--http-method-regexp (1 'hurl-mode-method-face)
				    (2 'hurl-mode-url-face))
    (,hurl-mode--expected-response-regexp (1 'hurl-mode-method-face)
					  (2 'hurl-mode-url-face))
    (,hurl-mode--header-regexp (1 'hurl-mode-header-names-face)
			       (2 'hurl-mode-header-value-face))
    (,hurl-mode--assert-regexp (1 'hurl-mode-query-face)
			       (2 'hurl-mode-query-arg-face)
			       (3 'hurl-mode-filter-face)
			       (4 'hurl-mode-filter-arg-face)
			       (5 'hurl-mode-filter-arg-face)
			       (6 'hurl-mode-filter-arg-face)
			       (7 'hurl-mode-pred-face)
			       (8 'hurl-mode-pred-arg-face)
			       )
    (,hurl-mode--capture-regexp (1  'hurl-mode-variable-face)
				(2 'hurl-mode-query-face)
				(3 'hurl-mode-query-arg-face)
				(4 'hurl-mode-filter-face)
				(5 'hurl-mode-filter-arg-face) ;; int arg
				(6 'hurl-mode-filter-arg-face) ;; string1
				(7 'hurl-mode-filter-arg-face) ;; string2
				)
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
