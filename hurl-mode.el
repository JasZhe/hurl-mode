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
(require 'js)
(require 'shell)
(require 'outline)

(eval-when-compile
  (require 'rx))

(defgroup hurl nil
  "A major mode for editing Hurl files."
  :group 'tools)

(defgroup hurl-faces nil
  "Faces used in Hurl Mode."
  :group 'hurl
  :group 'faces)

(defface hurl-mode-method-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for HTTP method."
  :group 'hurl-faces)

(defface hurl-mode-url-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for urls."
  :group 'hurl-faces)

(defface hurl-mode-section-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for hurl sections."
  :group 'hurl-faces)

(defface hurl-mode-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for hurl variables."
  :group 'hurl-faces)

(defface hurl-mode-variable-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl variable values."
  :group 'hurl-faces)

(defface hurl-mode-query-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for hurl query keywords."
  :group 'hurl-faces)

(defface hurl-mode-query-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl query arguments."
  :group 'hurl-faces)

(defface hurl-mode-filter-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for hurl filters."
  :group 'hurl-faces)

(defface hurl-mode-filter-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl filter arguments."
  :group 'hurl-faces)

(defface hurl-mode-pred-negation-face
  '((t (:inherit font-lock-negation-char-face)))
  "Face for hurl predicate not prefix."
  :group 'hurl-faces)

(defface hurl-mode-pred-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for hurl predicates."
  :group 'hurl-faces)

(defface hurl-mode-pred-arg-face
  '((t (:inherit font-lock-string-face)))
  "Face for hurl predicate arguments."
  :group 'hurl-faces)

(defface hurl-mode-body-face
  '((t (:inherit font-lock-string-face)))
  "Face for body values."
  :group 'hurl-faces)

(defface hurl-mode-template-face
  '((t (:inherit font-lock-type-face)))
  "Face for templates."
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
Essentially creates a temporary buffer with the specified major mode and inserts the request body text into it.
Then we can copy the font properties from that temporary buffer to our real hurl buffer.

This is basically a simplified version of the bomination of \"org-fontify-meta-lines-and-blocks-1\" and \"org-src-font-lock-fontify-block\"
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

;; Idea: only match " that doesn't have a \ immediate before it, or the empty string ""
;; key is using the non greedy *?
(defconst string-arg-with-escaped-quote
  `(or (: "\"" (*? (category ascii) ) (not "\\") "\"") (: "\"\"")))

(defconst hurl--query-regexp
  `(or (group (or ,@hurl-mode--no-arg-queries))
       (: (group (or ,@hurl-mode--arg-queries)) blank
          (group ,string-arg-with-escaped-quote))
       (: (group "certificate") blank
          (group (or ,@hurl-mode--certificate-attrs))
          ))
  )

;; need to go to beginning of line so we can match the other filters with other arg requirements
(defun hurl--string-arg-filter-matcher ()
  (list
   (rx-to-string `(: (group (or ,@hurl-mode--single-string-arg-filters))
                   blank
                   (group ,string-arg-with-escaped-quote)))
   nil
   '(beginning-of-line)
   '(1 'hurl-mode-filter-face t)
   '(2 'hurl-mode-query-arg-face t)
   )
  )

(defun hurl--double-string-arg-filter-matcher ()
  (list
    (rx-to-string `(: (group (or ,@hurl-mode--double-string-arg-filters)) blank
                    (group ,string-arg-with-escaped-quote) blank
                    (group ,string-arg-with-escaped-quote)))
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
(defun hurl-variable-and-capture-matcher ()
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

(defun hurl-assert-matcher ()
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
   (list (rx-to-string `(: (or (not "\\") line-start) "#" (* any))) '(0 'font-lock-comment-face t t))
   `(,hurl-mode--expected-response-regexp (1 'hurl-mode-method-face) (2 'hurl-mode-url-face))
   `(,hurl-mode--section-header-regexp (0 'hurl-mode-section-face))
   '(hurl-fontify-src-blocks)
   (hurl-variable-and-capture-matcher)
   (hurl-assert-matcher)
   `(,hurl-mode--template-regexp (0 'hurl-mode-template-face t t))
   )
  )

(defconst hurl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">#" table)
    table))

(defconst hurl-mode-options
  '("aws-sigv4" "cacert" "cert" "color" "compressed" "connect-timeout" "connect-to"
        "continue-on-error" "cookie" "cookie-jar" "delay" "error-format" "file-root"
        "glob" "http1.0" "http1.1" "http2" "http3" "ignore-asserts" "include" "insecure"
        "interactive" "ipv4" "ipv6" "json" "key" "location" "location-trusted" "max-redirs"
        "max-time" "no-color" "no-output" "noproxy" "output" "path-as-is" "proxy"
        "report-html" "report-junit" "report-tap" "resolve" "retry" "retry-interval"
        "ssl-no-revoke" "test" "to-entry" "unix-socket" "user" "user-agent" "variable"
        "variables-file" "verbose" "very-verbose" "help" "version")
  )

;;;###autoload
(define-derived-mode hurl-mode text-mode "Hurl"
  "Enable hurl mode."
  ;; backward paragraph is set as the 'font-lock-mark-block-function see:
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Other-Font-Lock-Variables.html
  ;; and https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
  ;; this is same as org-set-font-lock-defaults
  (setq-local font-lock-defaults '(hurl-mode-keywords t nil nil backward-paragraph))
  (setq-local font-lock-multiline t)
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  ;; honestly I think this works good enough for us and its built in without us doing anything crazy
  (setq-local indent-line-function 'js-indent-line)
  )

;; so we don't get auto ` pairing if smartparens mode exists
(eval-after-load 'smartparens-mode
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'hurl-mode "`" nil :actions nil)))

(defun hurl-response-outline-level ()
  (or 
   (cdr
    (cl-find-if
     (lambda (h)
       (string-match-p (car h) (match-string 0)))
     hurl-outline-heading-alist
     ))
   (- (match-end 0) (match-beginning 0))))

;;;###autoload
(define-derived-mode hurl-response-mode shell-mode "HurlRes"
  "Mode for the hurl response buffer.
\"outline-minor-mode\" is enabled in this buffer.
Each hurl request is its own block.
Each req/resp body is also its own block."
  (setq-local outline-blank-line t)
  (setq-local outline-regexp "\\* ---+\\|\\* Executing entry\\|\\* Request:\\|\\* Request body:\\|\\* Response:\\|\\* Response body:")
  (setq hurl-outline-heading-alist
        '(("\\* Executing entry" . 1)
          ("\\* -------" . 1)
          ("\\* Request:" . 2)
          ("\\* Request body:" . 2)
          ("\\* Response:" . 2)
          ("\\* Response body:" . 2)
          ))
  (setq-local outline-level #'hurl-response-outline-level)
  (outline-minor-mode))

(defcustom hurl-variables-file ".hurl-variables"
  "Name of variables file to automatically use in hurl requests."
  :type '(string))

(defun hurl-options-completion-at-point ()
  (let* ((bds (bounds-of-thing-at-point 'word))
         (start (car bds))
         (end (cdr bds)))
    (message "%s %s" start end)
    (if (and start end)
        (list start end hurl-mode-options . nil)
      (list (point) (point) hurl-mode-options . nil)
      )))

(defun hurl--read-args ()
  "Read user input until empty.
Prefixes every string with -- for convenience."
  (let ((option)
        (all-options ""))
    (while (not (string-empty-p
                 (setq option (minibuffer-with-setup-hook
                                  (lambda ()
                                    (add-hook 'completion-at-point-functions
                                              #'hurl-options-completion-at-point nil t))
                                (read-string "cli option (empty input finishes): --")))))
      (setq all-options (concat all-options " --" option))
      )
    all-options))
    
(defun hurl-mode--send-request (&optional args file-name proc-sentinel)
  (let ((args (concat args
                      (when (and current-prefix-arg
                                 (>= (car current-prefix-arg) 4))
                        " --very-verbose")
                      (when (and current-prefix-arg
                                 (>= (car current-prefix-arg) 16))
                        (hurl--read-args))
                      (when (file-exists-p hurl-variables-file)
                        (concat " --variables-file " hurl-variables-file))))
        (buf-name "*hurl-response*"))
    (save-buffer)
    ;; stole this from:
    ;; https://emacs.stackexchange.com/questions/16617/interpret-terminal-escape-codes-in-generic-process-output
    (when-let ((buf (get-buffer buf-name))) (kill-buffer buf))
    ;; https://stackoverflow.com/questions/41599314/ignore-unparseable-json-with-jq
    (let* ((jq-filtering-1 (when (executable-find "jq")
                             " | jq -R '. as $line | try (fromjson) catch $line'"))
           (jq-filtering (if (and current-prefix-arg
                                  (>= (car current-prefix-arg) 64))
                             (minibuffer-with-setup-hook
                                 (lambda () (insert jq-filtering-1))
                               (read-string "Edit jq command: "))
                           jq-filtering-1))
           (proc (apply 'start-process-shell-command
                        (append '("hurl") `(,buf-name)
                                `(,(concat "hurl " args " " (if file-name file-name (buffer-file-name)) jq-filtering)))))
           (proc-buffer (process-buffer proc)))

      (when proc-sentinel
        (set-process-sentinel proc proc-sentinel))
      
      (with-current-buffer proc-buffer
        (display-buffer proc-buffer)
        (beginning-of-line)
        (visual-line-mode)
        (require 'shell)
        (hurl-response-mode)
        (set-process-filter proc 'comint-output-filter)
        ;; WIP: right now we need to execute the request with the detailed view
        ;; then we just do a simple regex search for "* Captures:" and grab the variables from there
        ;; and write to the variables file
        (save-excursion
          (when (search-backward "Captures:" nil t)
            ;; match beginning *, then a group for .* for the capture name, a colon, a space and another .* group for the value
            (while (re-search-forward "\\* \\(.*\\).*: \\(.*\\)" nil t)
              (let ((variable (concat (match-string 1) "=" (match-string 2))))
                (with-temp-file hurl-variables-file
                  (insert variable))
                )
              )
            )
          )
        )
      )
    ))


(defun hurl-mode-send-request-file (arg)
  "Simple thin wrapper which sends the contents of the current file to hurl.
With one prefix ARGs, execute with --very-verbose.
With two, also prompt for arbitrary additional options to hurl command.
With three, also prompt to edit the jq command filtering"
  (interactive "P")
  (hurl-mode--send-request))

(defconst hurl-mode--temp-file-name ".temp-request.hurl")
(defun hurl-mode-send-request-single (arg)
  "Simple thin wrapper which sends the request at point to hurl.
With one prefix ARGs, execute with --very-verbose.
With two, also prompt for arbitrary additional options to hurl command.
With three, also prompt to edit the jq command filtering"
  (interactive "P")
  
  (let* ((beg (save-excursion
               (forward-line)
               (re-search-backward hurl-mode--http-method-regexp)
               (line-beginning-position)))
        (end (save-excursion
               (forward-line)
               ;; if we're at the last request in the file, there's no next to search forward for
               (re-search-forward hurl-mode--http-method-regexp nil 1)
               (line-beginning-position)))
        (req (buffer-substring-no-properties beg end)))
    (write-region beg end hurl-mode--temp-file-name)
    (hurl-mode--send-request nil hurl-mode--temp-file-name
                             (lambda (p e)
                               (when (not (equal (process-status p) 'run))
                                 (delete-file hurl-mode--temp-file-name))))
    )
  )


(defun hurl-mode-test-request-file (arg)
  "Hurl wrapper function to send file for testing.
With one prefix ARGs, execute with --very-verbose.
With two, also prompt for arbitrary additional options to hurl command.
With three, also prompt to edit the jq command filtering"
  (interactive "P")
  (hurl-mode--send-request "--test")
  )

(defun hurl-mode-test-request-single (arg)
  "Hurl wrapper function to send the request at point for testing.
With one prefix ARGs, execute with --very-verbose.
With two, also prompt for arbitrary additional options to hurl command.
With three, also prompt to edit the jq command filtering"
  (interactive "P")
  (hurl-mode--send-request "--test")
  )

(setq hurl-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-M-i") nil)
        (define-key map (kbd "M-<tab>") nil)
        (define-key map (kbd "C-c T") 'hurl-mode-test-request-file)
        (define-key map (kbd "C-c t") 'hurl-mode-test-request-single)
        
        (define-key map (kbd "C-c X") 'hurl-mode-send-request-file)
        (define-key map (kbd "C-c x") 'hurl-mode-send-request-single)
        map))

(setq hurl-response-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "q") #'quit-window)
        (define-key map (kbd "TAB") #'outline-toggle-children)
        (define-key map (kbd "<tab>") #'outline-toggle-children)
        
        (define-key map (kbd "S-TAB") #'outline-cycle-buffer)
        (define-key map (kbd "S-<tab>") #'outline-cycle-buffer)

        (define-key map (kbd "C-c ]") #'outline-next-heading)
        (define-key map (kbd "C-c >") #'outline-next-heading)
        
        (define-key map (kbd "C-c [") #'outline-previous-heading)
        (define-key map (kbd "C-c <") #'outline-previous-heading)
        map))

(provide 'hurl-mode)
;;; hurl-mode.el ends here
