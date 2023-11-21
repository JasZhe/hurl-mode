;;; hurl-mode.el --- major mode for hurl
;;; Commentary:
;;; simple major mode for editing hurl files

;;; Code:

(eval-when-compile
  (require 'rx))

(defgroup hurl-mode nil
  "A major mode for editing Hurl files."
  :group 'tools)

(defgroup hurl-mode-faces nil
  "Faces used in Hurl Mode"
  :group 'hurl-mode
  :group 'faces)

(defface hurl-mode-method-face 
  '((t (:inherit font-lock-keyword-face)))
  "Face for HTTP method."
:group 'hurl-mode-faces)

(defface hurl-mode-url-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for urls"
  :group 'hurl-mode-faces)

(defface hurl-mode-section-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for hurl sections"
  :group 'hurl-mode-faces)

(defface hurl-mode-header-names-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for header names"
  :group 'hurl-mode-faces)

(defface hurl-mode-header-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for header values"
  :group 'hurl-mode-faces)

(defface hurl-mode-body-face
  '((t (:inherit font-lock-constant-face)))
  "Face for body values"
  :group 'hurl-mode-faces)


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
(defconst hurl-mode-body-regexp
  "```[^`]*```$")


(defun hurl-mode-fontify-region
    (beg end keywords syntax-table syntax-propertize-fn &optional font-lock-syntactic-face-fn)
  "Fontify a region between BEG and END using another mode's fontification.

KEYWORDS, SYNTAX-TABLE, SYNTACTIC-KEYWORDS and
SYNTAX-PROPERTIZE-FN are the values of that mode's
`font-lock-keywords', `font-lock-syntax-table',
`font-lock-syntactic-keywords', and `syntax-propertize-function'
respectively."
  (message "fontify region")
  (save-excursion
    (save-match-data
      (let ((font-lock-keywords keywords)
            (font-lock-syntax-table syntax-table)
            (syntax-propertize-function syntax-propertize-fn)
            (font-lock-multiline 'undecided)
            (font-lock-dont-widen t)
            (font-lock-syntactic-face-function font-lock-syntactic-face-fn)
            font-lock-keywords-only
            font-lock-extend-region-functions
            font-lock-keywords-case-fold-search)
        (save-restriction
	  (message "beg %d end %d" beg end)
          (narrow-to-region (1- beg) end)
	  (message "next")

	  ;; this is JUST for nxml mode
	  ;; otherwise nxml-fontify-matcher complains
	  ;; this tells nxml mode whether to fontify or something
	  ;; setting to beginning ensures that it always fontifies
	  ;; tried setting it to end, and there was odd behavior where adding new lines would mess it up
	  ;; but removing newlines was fine
	  (setq nxml-prolog-end beg)
	  
          ;; font-lock-fontify-region apparently isn't inclusive,
          ;; so we have to move the beginning back one char
          (font-lock-fontify-region (1- beg) end t)
	  )))))

;; REQUIRES THAT NXML HAS BEEN LOADED
(defun hurl-fontify-region-as-xml (beg end)
  "Fontify XML code from BEG to END.

This requires that `nxml-mode' is available."
  (message "fontify as xml")
  (when (boundp 'nxml-font-lock-keywords)
    (message "xml mode found")
    (hurl-mode-fontify-region beg end
                         nxml-font-lock-keywords
                         nxml-mode-syntax-table
                         (lambda (start end) (nxml-syntax-propertize start end)))))

;; inlining the json-mode syntax stuff since it's relatively simple
(defconst json-mode-number-re (rx (group (one-or-more digit)
                                         (optional ?\. (one-or-more digit)))))
(defconst json-mode-keyword-re  (rx (group (or "true" "false" "null"))))

(defconst json-font-lock-keywords-1
  (list
   (list json-mode-keyword-re 1 font-lock-constant-face)
   (list json-mode-number-re 1 font-lock-constant-face))
  "Level one font lock.")

(defvar json-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Objects
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; Arrays
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ;; Strings
    (modify-syntax-entry ?\" "\"" st)
    st))

(defconst json-mode-quoted-key-re
  (rx (group (char ?\")
             (zero-or-more (or (seq ?\\ ?\\)
                               (seq ?\\ ?\")
                               (seq ?\\ (not (any ?\" ?\\)))
                               (not (any ?\" ?\\))))
             (char ?\"))
      (zero-or-more blank)
      ?\:))

(defun json-mode--syntactic-face (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
json font lock syntactic face function."
  (cond
   ((nth 3 state)
    ;; This might be a string or a name
    (let ((startpos (nth 8 state)))
      (save-excursion
        (goto-char startpos)
        (if (looking-at-p json-mode-quoted-key-re)
            font-lock-keyword-face
          font-lock-string-face))))
   ((nth 4 state) font-lock-comment-face)))

(defun hurl-fontify-region-as-json (beg end)
  "Fontify JSON code from BEG to END."
  (when (boundp 'json-font-lock-keywords-1)
    (hurl-mode-fontify-region beg end
                              json-font-lock-keywords-1
                              json-mode-syntax-table
                              (lambda (start end) nil)
                              #'json-mode--syntactic-face)))


(defun hurl-highlight-filter-xml (limit)
  "Highlight any body region found in the text up to LIMIT."
  (when (re-search-forward "```xml[^`]*```$" limit t)
    (let ((code-start (+ (match-beginning 0) 7)) 
	  (code-end (- (match-end 0) 4)))
      (message "code start %s" code-start)
      (message "code end %s" code-end)
      (save-match-data
        (ignore-errors (org-src-font-lock-fontify-block "xml" code-start code-end)))
      (goto-char (match-end 0)))))

(defun hurl-highlight-filter-json (limit)
  "Highlight any body region found in the text up to LIMIT."
  (when (re-search-forward "```json[^`]*```$" limit t)
    (let ((code-start (+ (match-beginning 0) 8))
	  (code-end (- (match-end 0) 4)))
      (message "code start %s" code-start)
      (message "code end %s" code-end)
      (save-match-data
        (ignore-errors (org-src-font-lock-fontify-block "json" code-start code-end)))
      (goto-char (match-end 0)))))

(defun hurl-highlight-filter-graphql (limit)
  "Highlight any body region found in the text up to LIMIT."
  (when (and (fboundp 'graphql-mode) (re-search-forward "```graphql[^`]*```$" limit t))
    (let ((code-start (+ (match-beginning 0) 11))
	  (code-end (- (match-end 0) 4)))
      (message "code start %s" code-start)
      (message "code end %s" code-end)
      (save-match-data
        (ignore-errors (org-src-font-lock-fontify-block "graphql" code-start code-end)))
      (goto-char (match-end 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stole a lot of this from haml-mode

(defun hurl-find-containing-block (re)
  "If point is inside a block matching RE, return (start . end) for the block."
  (save-excursion

    (message "find containing block")
    (let ((pos (point))
          start end)
      (beginning-of-line)
      (when (and
             (or (looking-at re)
                 (when (re-search-backward re nil t)
                   (looking-at re)))
             (< pos (match-end 0)))
        (setq start (match-beginning 0)
              end (match-end 0)))
      (when start
	(message "start %s end %s" start end)
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
                font-lock-end (max font-lock-end (cdr new-bounds))))))
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
   (font-lock-extend-region-multiline)
   (font-lock-extend-region-multiline)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun hurl-mode--font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "^```" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "```$" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

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

(define-derived-mode hurl-mode text-mode "Hurl"
  "Enable hurl mode"
  (setq-local font-lock-defaults '((hurl-mode-keywords)))
  (setq-local font-lock-multiline t)
  ;;(setq-local font-lock-extend-region-functions '(hurl-extend-region-contextual)) ;; some weird behavior with this where font locking won't show until I press space, but this makes it so our blocks are dynamic
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  )


(defun hurl-mode-send-request ()
  "Simple thin wrapper over the cli command"
  (interactive)
  (async-shell-command (concat "hurl " (buffer-file-name))))

(provide 'hurl-mode)
