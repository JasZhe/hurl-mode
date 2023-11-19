;;; hurl-mode.el ---- major mode for hurl -*- lexical-binding; t; -*-

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
		    (group (* any)))))


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


(defconst hurl-mode-body-block-regexp
  (rx-to-string `(: bol "```"
		    (* alpha)
		    (group (* (or any "\n")))
		    "```")))

(defun hurl-mode-fontify-region
    (beg end keywords syntax-table syntax-propertize-fn)
  "Fontify a region between BEG and END using another mode's fontification.

KEYWORDS, SYNTAX-TABLE, SYNTACTIC-KEYWORDS and
SYNTAX-PROPERTIZE-FN are the values of that mode's
`font-lock-keywords', `font-lock-syntax-table',
`font-lock-syntactic-keywords', and `syntax-propertize-function'
respectively."
  (save-excursion
    (save-match-data
      (let ((font-lock-keywords keywords)
            (font-lock-syntax-table syntax-table)
            (syntax-propertize-function syntax-propertize-fn)
            (font-lock-multiline 'undecided)
            (font-lock-dont-widen t)
            font-lock-keywords-only
            font-lock-extend-region-functions
            font-lock-keywords-case-fold-search)
        (save-restriction
	  (message "beg %d end %d" beg end)
          (narrow-to-region beg end)
	  (message "next")

	  ;; otherwise nxml-fontify-matcher complains
	  ;; its set to point anyways in nxml-scan-prolog
	  (setq nxml-prolog-end (point))
	  
          ;; font-lock-fontify-region apparently isn't inclusive,
          ;; so we have to move the beginning back one char
          (font-lock-fontify-region beg end t)
	  )))))




(defun hurl-fontify-region-as-xml (beg end)
  "Fontify CSS code from BEG to END.

This requires that `nxml-mode' is available."
  (when (boundp 'nxml-font-lock-keywords)
    (hurl-mode-fontify-region beg end
                         nxml-font-lock-keywords
                         nxml-mode-syntax-table
                         #'nxml-syntax-propertize)))

(defvar hurl-fontify-filter-functions-alist
  '(("xml" . hurl-fontify-region-as-xml)))

(defun hurl-highlight-filter (limit)
  "Highlight any body region found in the text up to LIMIT."
  (when (re-search-forward hurl-mode-body-block-regexp limit t)
    ;; fontify the filter name
    (let (
	  (code-start (+ (match-beginning 0) 7)) 
	  (code-end (- (match-end 0) 4)))
      (message "code start %s" code-start)
      (message "code end %s" code-end)
      (save-match-data
        (hurl-fontify-region-as-xml code-start code-end))
      (goto-char (match-end 0)))))

  

(defconst hurl-mode-keywords
  (list (list hurl-mode--http-method-regexp
	      '(1 'hurl-mode-method-face) '(2 'hurl-mode-url-face))
	(list hurl-mode--expected-response-regexp
	      '(1 'hurl-mode-method-face) '(2 'hurl-mode-url-face))
	(list hurl-mode--header-regexp
	      '(1 'hurl-mode-header-names-face) '(2 'hurl-mode-header-value-face))
	'hurl-highlight-filter
	(list hurl-mode-body-block-regexp '(0 'hurl-mode-body-face))
	(list hurl-mode--section-header-regexp '(0 'hurl-mode-section-face))))

(defconst hurl-mode-keywords
  `((hurl-highlight-filter)))

(defconst hurl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

(defun haml-find-containing-block (re)
  "If point is inside a block matching RE, return (start . end) for the block."
  (save-excursion
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
        (cons start end)))))

		    
;; need to fix this similar to how haml-maybe-extend-region-works
(defun hurl-mode-font-lock-extend-region ()
  "Extend the search region for request body"
  (let* ((old-beg font-lock-beg)
	(old-end font-lock-end)
	(new-beg (save-excursion (goto-char old-beg) (re-search-backward "```" nil t)))
	(new-end (save-excursion (goto-char old-end) (re-search-forward "```" nil t))))
    (message "oldb %s olde %s newb %s newe %s" old-beg old-end new-beg new-end)
    (when new-end (setq font-lock-end new-end))
    (when new-beg (setq font-lock-beg new-beg))
    t
    )
  )


(define-derived-mode hurl-mode text-mode "Hurl"
  "Enable hurl mode"
  (setq-local font-lock-defaults '((hurl-mode-keywords) t t))
  (setq-local font-lock-multiline t)
  (setq-local font-lock-extend-region-functions '(hurl-mode-font-lock-extend-region font-lock-extend-region-wholelines font-lock-extend-region-multiline))
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  )

(provide 'hurl-mode)
