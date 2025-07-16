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
(require 'js)
(require 'sgml-mode)
(require 'json-mode nil 'no-error) ;; if json-mode isn't available we'll try to fallback to jq or something
(require 'json-ts-mode nil 'no-error) ;; optionally require json-ts-mode
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

(defcustom hurl-secrets-file ".secrets.gpg"
  "Name of secrets file to use in hurl requests.

Format should be NAME=VALUE as specified in hurl's --secret option.
All variables in here will be sent via the --secret option in hurl.

This file is HIGHLY encouraged to be encrypted.
Emacs has built in support for handling GPG files.

Other encryption methods will need custom handling.
TODO: Need a way for users to hook into the request to decrypt secrets file."
  :type '(string))

(defcustom hurl-variables-file ".hurl-variables"
  "Name of variables file to automatically use in hurl requests."
  :type '(string))

(defcustom hurl-mode-use-netrc-file nil
  "Whether or not to use the --netrc-file option.
See: `hurl-mode-netrc-file'"
  :type '(boolean))

(defcustom hurl-mode-netrc-file "~/.netrc"
  "Location for netrc file for usernams and passwords.
To be used with the --netrc-file option."
  :type '(file))

(defcustom hurl-mode-use-json-ts-mode t
  "Whether or not to use `json-ts-mode' for json fontification in response body if applicable."
  :type '(boolean))

(defcustom hurl-use-fast-process-settings t
  "Whether or not to speed up processes via changing some Emacs vars.
`read-process-output-max' and `process-adaptive-read-buffering' can be tuned
to improve process speed.
Setting this will adjust those settings only for the hurl process we create."
  :type '(boolean))


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

;; Hurl Filter Reference: https://hurl.dev/docs/filters.html
(defconst hurl-mode--no-arg-filters
  '("count" "daysAfterNow" "daysBeforeNow" "decode" "format" "htmlEscape" "htmlUnescape" "toDate" "toInt" "urlDecode" "urlEncode"))
(defconst hurl-mode--single-int-arg-filters '("nth"))
(defconst hurl-mode--single-string-arg-filters '("regex" "split" "xpath"))
(defconst hurl-mode--double-string-arg-filters `("replace"))


(defun hurl-fontify-src-blocks (limit)
  "Fontifies body blocks for detected languages.
Essentially creates a temporary buffer with the specified major
mode and inserts the request body text into it.

Then we can copy the font properties from that
temporary buffer to our real hurl buffer.

This is basically a simplified version of the combination of
`org-fontify-meta-lines-and-blocks-1' and `org-src-font-lock-fontify-block'
since we don't need to care about the other block types in org.

From the elisp docs:
LIMIT is the limit of the search; it should begin searching at point,
and not search beyond the limit"
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
              )))
        ;; this t is really important to indicate that the match succeeded, or else the match data gets unset.
        ;; see: https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
        ;; for reference on function based fontification.
        t))))

(defconst string-arg-with-escaped-quote
  `(or (: "\"" (*? (category ascii) ) (not "\\") "\"") (: "\"\""))
  "Regex to match a string argument.
Quote followed by ascii terminated by a NON-ESCAPED quote.
To achieve this, the ascii matching portion MUST BE non-greedy via *?.
Otherwise, the match never terminates.
An empty string (no chars in between the quotes) is matched separately.")

(defconst hurl--query-regexp
  `(or (group (or ,@hurl-mode--no-arg-queries))
       (: (group (or ,@hurl-mode--arg-queries)) blank
          (group ,string-arg-with-escaped-quote))
       (: (group "certificate") blank
          (group (or ,@hurl-mode--certificate-attrs))
          ))
  "Regex to match queries.
See: https://hurl.dev/docs/grammar.html#query
Some do not require any arguments. Some require a single argument.
Certificate query has a specific set of valid arguments.")

;; need to go to beginning of line so we can match the other filters with other arg requirements
(defun hurl--string-arg-filter-matcher ()
  "Intended to be used as anchored-highlighter fontification for \"filters\".
See `hurl-variable-and-capture-matcher' for more details."
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
  "Intended to be used as anchored-highlighter fontification for \"filters\".
See `hurl-variable-and-capture-matcher' for more details."
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
  "Intended to be used as anchored-highlighter fontification for \"filters\".
See `hurl-variable-and-capture-matcher' for more details."
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
  "Intended to be used as anchored-highlighter fontification for \"filters\".
See `hurl-variable-and-capture-matcher' for more details."
  (list
   (rx-to-string `(: (group (or ,@hurl-mode--no-arg-filters))))
   nil
   '(beginning-of-line)
   '(0 'hurl-mode-filter-face t)
   )
  )

(defun hurl-variable-and-capture-matcher ()
  "Matcher-highlighter fontification for \"captures\".

`hurl--query-regexp' has 5 groups that we can target for font-locking.
Argumentless queries. (1 group)
Queries with a single argument. (2 groups)
Certificate query. (2 groups)
See: https://hurl.dev/docs/capturing-response.html

We target each with a subexp-highligher.

For Hurl filters however, we use anchored-highlighting.
In this case, the capture acts as our \"anchor\".
See: https://hurl.dev/docs/filters.html

All of our anchored-highlighters move to `beginning-of-line' in their POST-FORM.
We need this to be able to re-anchor on the capture name so that we can
try subsequent filter matchers.

Hurl grammar reference: https://hurl.dev/docs/grammar.html#capture

Matcher-highlighter, subexp-highlighter and anchored-highlighter reference:
https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html"
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
  "Matcher-highlighter fontification for \"asserts\".
Hurl assert reference: https://hurl.dev/docs/asserting-response.html
See `hurl-variable-and-capture-matcher' for a more detailed explanation on how
the fontification is done."
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
    '(3 'font-lock-constant-face t t))))

(defconst hurl-mode-keywords
  (list
   `(,hurl-mode--http-method-regexp (1 'hurl-mode-method-face) (2 'hurl-mode-url-face))
   (list (rx-to-string `(: (or (not "\\") line-start) "#" (* any))) '(0 'font-lock-comment-face t t))
   `(,hurl-mode--expected-response-regexp (1 'hurl-mode-method-face) (2 'hurl-mode-url-face))
   `(,hurl-mode--section-header-regexp (0 'hurl-mode-section-face))
   '(hurl-fontify-src-blocks)
   (hurl-variable-and-capture-matcher)
   (hurl-assert-matcher)
   `(,hurl-mode--template-regexp (0 'hurl-mode-template-face t t))))

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

(defun hurl-inside-block-p ()
  "Return t if inside hurl body block, nil otherwise.
If point is on the block delimiters themselves, return nil."
  ;;(rx bol (group "```" (group (zero-or-more (any "a-zA-Z"))))))
  (condition-case nil
      (let ((block-start (save-excursion (re-search-backward "```") (point)))
            ;; need the end-of-line here cause we don't want to return true when point is ON the block delimiter
            (block-end (save-excursion (end-of-line) (re-search-forward "```") (point)))
            (prev-req (save-excursion
                        (condition-case nil
                            (progn (re-search-backward hurl-mode--http-method-regexp) (point))
                          (error (point-min)))))
            (next-req (save-excursion
                        (condition-case nil
                            (progn (re-search-forward hurl-mode--http-method-regexp) (point))
                          (error (point-max))))))
        (and (> block-start prev-req) (< block-end next-req)))
    (error nil)))

(defun hurl-indent-line ()
  "Indent line using `js-indent-line' if we're inside a block.
Otherwise do nothing."
  ;; TODO: capture the block lang, and have some mapping of langs to indent functions.
  (if (hurl-inside-block-p)
      (js-indent-line)
    'noindent))

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
  (setq-local indent-line-function 'hurl-indent-line)
  (setq-local js-indent-level 2)
  ;; HACK: for some reason font lock is broken when there are comments after
  ;; reverting the buffer, font-lock-update seems to fix it
  (add-hook 'after-revert-hook (lambda () (font-lock-update)) nil t)
  )

;; so we don't get auto ` pairing if smartparens mode exists
(eval-after-load 'smartparens-mode
  (when (fboundp 'sp-local-pair)
    (sp-local-pair 'hurl-mode "`" nil :actions nil)))

(defun hurl-response-outline-level ()
  "Hurl response mode `outline-level' function."
  (or
   (cdr
    (cl-find-if
     (lambda (h)
       (string-match-p (car h) (match-string 0)))
     hurl-outline-heading-alist
     ))
   (- (match-end 0) (match-beginning 0))))

;;;###autoload
(define-derived-mode hurl-response-mode text-mode "HurlRes"
  "Mode for the hurl response buffer.
`outline-minor-mode' is enabled in this buffer.
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

  ;; Don't want any fontification cause we're manually setting text properties with hurl--fontify-using-faces
  ;; and ansi-color-apply-on-region.
  ;; Otherwise, our json fontification will get overriden by font-lock-string-face
  (setq-local font-lock-defaults '())

  (setq-local outline-level #'hurl-response-outline-level)
  (outline-minor-mode))

(defvar hurl-response--buffer-name "*hurl-response*"
  "Name of output buffer to display to user.")

(defvar hurl-response--output-buffer-name "*hurl-temp-output*"
  "Name of temporary buffer to hold process output for parsing later.")

(defvar hurl-response--process-buffer-name "*hurl-process*"
  "Name of buffer where the hurl process runs in.")

(defconst hurl-mode--temp-file-name ".temp-request.hurl")

(defun hurl-options-completion-at-point ()
  "Mainly for providing additional options via executing a request with prefix arg."
  (let* ((bds (bounds-of-thing-at-point 'word))
         (start (car bds))
         (end (cdr bds)))
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

(define-error 'hurl-json-parse-error "Hurl-mode: Error parsing response using json.el")
(define-error 'hurl-parse-error "Error parsing hurl response")

(defun hurl--fontify-using-faces (text)
  "This is for using another major mode's fontification on TEXT.
Reference: https://emacs.stackexchange.com/questions/5400/fontify-a-region-of-a-buffer-with-another-major-mode"
  (let ((pos 0))
    (while (setq next (next-single-property-change pos 'face text))
      (put-text-property pos next 'font-lock-face (get-text-property pos 'face text) text)
      (setq pos next))
    (add-text-properties 0  (length text) '(fontified t) text)
    text))

(defun hurl-response--format-json (json)
  "Format JSON string keeping json-mode's fontification. Assumes `json-mode' or `json-ts-mode' are available."
  (with-temp-buffer
    (erase-buffer)
    (insert json)
    ;; json-pretty-print has been around for a while (since emacs 25 I think?)
    (condition-case err
        (json-pretty-print (point-min) (point-max))
      (json-readtable-error
       (signal 'hurl-json-parse-error json)))
    (if (and hurl-mode-use-json-ts-mode (fboundp 'json-ts-mode) (treesit-available-p) (treesit-language-available-p 'json))
        (progn
          (delay-mode-hooks (json-ts-mode))
          (font-lock-default-function 'json-ts-mode)
          )
      (when (fboundp 'jsonc-mode)
        (delay-mode-hooks (jsonc-mode))
        (font-lock-default-function 'jsonc-mode)
        )
      )
    (font-lock-default-fontify-region (point-min) (point-max) nil)
    (hurl--fontify-using-faces (buffer-string))))

(defun hurl-response--format-json-with-jq (resp)
  "Format http RESP using jq.
If not possible, return the resp as is."
  (let* ((jq-command (executable-find "jq" (file-remote-p default-directory)))
         (jq-output
          (when jq-command
            (ansi-color-apply
             (let ((cmd (shell-command-to-string
                         ;; -C to colorize with ansi codes, then we apply using 'ansi-color-apply'
                         ;; -R reads each line as string instead of JSON
                         ;; the 'try' lets us output erroneous lines as is, while continuouing to parse the rest
                         ;; https://stackoverflow.com/questions/41599314/ignore-unparseable-json-with-jq
                         (format "timeout 3 echo %s | %s -C -R '. as $line | try (fromjson) catch $line'"
                                 (shell-quote-argument resp)
                                 jq-command))))
               cmd)
             )
            )))
    (if (string-empty-p jq-output) resp jq-output)
    )
  )

(defun hurl-response--format-xml (resp)
  "Format xml RESP using `sgml-pretty-print' and keeping sgml mode's fontification."
  (with-temp-buffer
    (erase-buffer)
    (insert resp)
    (delay-mode-hooks (sgml-mode))
    (font-lock-default-function 'sgml-mode)
    (font-lock-default-fontify-region (point-min) (point-max) nil)
    (sgml-pretty-print (point-min) (point-max))
    (hurl--fontify-using-faces (buffer-string)))
  )

(defun hurl-response--parse-and-filter-output (&optional variables-filename)
  "Filters the hurl --verbose output STR from PROC to what we care about.
Namely, response headers/body, and captures (to save to variables file).
If VARIABLES-FILENAME is set, save the captures as hurl-variables to that file.
Otherwise use the default `hurl-variables-file'."

  (condition-case err
      ;; filter using capture groups, anychar is used because we want newlines
      ;; there's always a Response and Response body luckily
      (let* ((str (with-current-buffer hurl-response--output-buffer-name
                    (buffer-string)))
             (output-without-response
              (with-current-buffer hurl-response--output-buffer-name
                (goto-char (point-min))
                (concat
                 ;; stuff before response
                 (buffer-substring (point-min)
                                   (progn (re-search-forward "* Response body:") (line-beginning-position)))
                 ;; stuff after response
                 (buffer-substring (progn (re-search-forward "* Timings:") (line-beginning-position))
                                   (progn (re-search-forward "*$") (line-beginning-position))))))
             (resp-head
              (with-current-buffer hurl-response--output-buffer-name
                (goto-char (point-min))
                (buffer-substring (progn (re-search-forward "* Response:")
                                         (line-beginning-position))
                                  (progn
                                    (re-search-forward
                                     "* Response body:")
                                    (line-end-position)))))
             (resp1
              (with-current-buffer hurl-response--output-buffer-name
                (goto-char (point-min))
                (buffer-substring (progn (re-search-forward "* Response body:")
                                         (line-end-position))
                                  (progn (re-search-forward "* Timings:")
                                         (line-beginning-position)))))
             ;; get rid of leading stars
             (resp (mapconcat (lambda (s) (string-trim-left (string-trim-left s "\\* ") "\\*"))
                              ;; split on real newlines only
                              ;; sometimes replacing escaped newlines with real ones can really trip up json parsing i.e. {"x": "long\r\nline"}, we'd split up value for x which would be a mistake
                              (split-string resp1 (rx-to-string `(: (or "\n"))) t)
                              "\n"))
             (formatted-resp
              (condition-case nil
                  (with-temp-buffer
                    (cond ((string-match "Content-Type: application/json" resp-head)
                           (condition-case err
                               (insert (hurl-response--format-json resp))
                             ;; on error i.e if json-mode or json-ts-mode aren't available tries to fall back to jq
                             (error
                              ;; disable fallback to jq on remote. Talking to remote jq process can be very slow
                              ;; most of the time formatting with json-mode should be good enough and is much faster
                              (if (not (file-remote-p default-directory))
                                  (insert (hurl-response--format-json-with-jq resp))

                                ;; one last try just getting formatting without font lock via json-pretty-print (> emacs25)
                                (insert (with-temp-buffer
                                          (erase-buffer)
                                          (insert resp)
                                          (condition-case err
                                              (progn
                                                (json-pretty-print (point-min) (point-max))
                                                (buffer-string))
                                            (error resp))))
                                )
                              )
                             )
                           )
                          ((or (string-match "Content-Type: text/xml" resp-head)
                               (string-match "Content-Type: application/xml" resp-head)
                               (string-match "Content-Type: text/html" resp-head))
                           (insert (hurl-response--format-xml resp)))
                          (t (insert resp)))
                    (buffer-substring (point-min) (point-max)))
                (error (signal 'hurl-parse-error str))))
             (captures1 (with-current-buffer hurl-response--output-buffer-name
                          (goto-char (point-min))
                          ;; if there's no capture we'll just return nil here since re-search-forward will error out
                          (ignore-errors
                            (buffer-substring (progn (re-search-forward "* Captures:") (forward-line) (line-beginning-position))
                                              (progn (re-search-forward "^*\n") (line-beginning-position))))))
             ;; mainly get rid of *'s, and convert the : to an = for the variables file
             ;; becomes a list of name to val
             (captures (when captures1
                         (cl-map 'list
                                 (lambda (s)
                                   ;; We only want to split on the FIRST :, since for example we can get something like this in the json
                                   ;; "message": "https://images.dog.ceo/breeds/australian-shepherd/leroy.jpg"
                                   ;;                  ^ note the colon here in the value
                                   (let ((splits (split-string s ":")))
                                     (cons (nth 0 splits) (list (string-join (cdr splits) ":")))))
                                 (seq-filter #'identity
                                             ;; get rid of leading *'s
                                             ;; TODO maybe we extract this part out cause its same code as for resp
                                             (cl-map 'list (lambda (s)
                                                             (when (string-match (rx-to-string `(: bol "*"  (group (0+ nonl)))) s)
                                                               (substring s (match-beginning 1) (match-end 1)))
                                                             )
                                                     (split-string captures1 "\n"))))))
             (captures (seq-filter (lambda (c)
                                     (and
                                      ;; NOTE: sometimes a newline gets put into captures1 or something
                                      ;; which puts something like ("" . nil) when we do the split-string on ':'
                                      ;; we want to make sure not to have any of those cause it can mess up the
                                      ;; functionality below for replacing existing variables
                                      (not (string-empty-p (car c)))
                                      (cdr c)))
                                   captures))
             )
        (with-current-buffer (get-buffer-create hurl-response--buffer-name)
          (erase-buffer)
          (hurl-response-mode)
          (insert output-without-response)
          (when captures
            (with-temp-file hurl-variables-file
              ;; insert hurl-variables-file contents into buffer created by with-temp-file
              ;; so that we can replace existing variables with updated ones from the new captures
              (when (file-exists-p hurl-variables-file)
                (insert-file-contents hurl-variables-file))
              (seq-do
               (lambda (e)
                 ;; reuse variables file and replace values if needed
                 (save-excursion
                   (goto-char (point-min))
                   (when (re-search-forward (concat (string-trim (car e)) "=") nil t)
                     ;; NOTE: this bit might be error prone, see the note above with the seq-filter on 'captures'
                     (delete-line)))
                 (insert (mapconcat #'string-trim e "=") "\n")
                 )
               captures
               )
              )
            )
          (insert formatted-resp "\n")
          ))
    ;; something wrong happened with parsing so just output everything
    (error
     (with-current-buffer (get-buffer-create hurl-response--buffer-name)
       (erase-buffer)
       (hurl-response-mode)
       (insert (with-current-buffer hurl-response--output-buffer-name (buffer-string)))
       (insert (concat "\nhurl lisp error: " (prin1-to-string err)))))
    )
  )

(defun hurl-response--verbose-filter (proc str)
  "Simple process filter for use in `set-process-filter'.
Just applies ansi color to the STR that come in from the hurl PROC."
  (with-current-buffer (get-buffer-create hurl-response--output-buffer-name)
    (insert (ansi-color-apply str))))



(defun hurl-mode--read-secrets-file ()
  "Read `hurl-secrets-file' and return a list of secret key value pairs."
  (with-temp-buffer
    (when (file-exists-p hurl-secrets-file)
      (insert-file-contents hurl-secrets-file))
    (let ((secrets '()))
      (while (not (eobp))
        (cl-pushnew (buffer-substring-no-properties (line-beginning-position) (line-end-position)) secrets)
        (forward-line 1))
      secrets)))


(defun hurl-mode--send-request (&optional args file-name proc-sentinel)
  "Spawns a process to execute the hurl request.
`ARGS' are hurl cli arguments.
This function will always include --very-verbose.
A prefix arg will prompt for additional args.
If a `hurl-variables-file' exists, include --variables-file.

If `FILE-NAME' is provided, then use it.
Otherwise use the current buffer file.

If `PROC-SENTINEL' is provided, then set it for the hurl process."
  (save-buffer) ;; this saves the current hurl file
  (let* ((args (concat args
                       " --very-verbose"
                       (when hurl-mode-use-netrc-file
                         (concat " --netrc-file " hurl-mode-netrc-file))
                       (mapconcat
                        (lambda (secret) (concat " --secret " secret))
                        (hurl-mode--read-secrets-file))
                       (when current-prefix-arg
                         (hurl--read-args))
                       (when (file-exists-p hurl-variables-file)
                         (concat " --variables-file " hurl-variables-file))))
         (cmd (concat "hurl" " " args " " (if file-name file-name (buffer-file-name)))))
    (ignore-errors (kill-buffer hurl-response--output-buffer-name))
    (get-buffer-create hurl-response--output-buffer-name)
    (when-let ((buf (get-buffer hurl-response--process-buffer-name))) (kill-buffer buf))
    (let* ((read-process-output-max (if hurl-use-fast-process-settings (* 64 1024 1024) read-process-output-max))
           (process-adaptive-read-buffering (if hurl-use-fast-process-settings nil process-adaptive-read-buffering))
           (proc (start-file-process "hurl" (get-buffer-create hurl-response--process-buffer-name)
                                     "/bin/sh" "-c" cmd))
           (proc-buffer (process-buffer proc)))

      (with-current-buffer proc-buffer
        (when hurl-use-fast-process-settings
          (setq-local read-process-output-max (* 64 1024 1024)
                      process-adaptive-read-buffering nil)))

      (when proc-sentinel
        (set-process-sentinel proc proc-sentinel))
      (set-process-filter proc #'hurl-response--verbose-filter))))


(defun hurl-mode-send-request-single (arg)
  "Simple thin wrapper which sends the request at point to hurl.
With prefix ARG, prompts for additional arguments to send to hurl."
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
    (hurl-mode--send-request
     nil hurl-mode--temp-file-name
     (lambda (p e) (when (not (process-live-p p))
                     (with-current-buffer (get-buffer-create hurl-response--output-buffer-name)
                       (ansi-color-apply-on-region (point-min) (point-max)))
                     (with-current-buffer hurl-response--output-buffer-name
                       (hurl-response--parse-and-filter-output))
                     (display-buffer hurl-response--buffer-name)
                     (delete-file hurl-mode--temp-file-name))))))

(defun hurl-mode-send-request-file (arg)
  "Simple thin wrapper which sends the contents of the current file to hurl.
With prefix ARG, prompts for additional arguments to send to hurl."
  (interactive "P")
  (write-region (point-min) (point-max) hurl-mode--temp-file-name)
  (hurl-mode--send-request nil hurl-mode--temp-file-name
                           (lambda (p e) (when (not (process-live-p p))
                                           (hurl-response--parse-and-filter-output)
                                           (with-current-buffer hurl-response--output-buffer-name
                                             (ansi-color-apply-on-region (point-min) (point-max)))
                                           (display-buffer hurl-response--buffer-name)
                                           (delete-file hurl-mode--temp-file-name)))
                           ))


(defun hurl-mode-test-request-file (arg)
  "Hurl wrapper function to send file for testing.
With prefix ARG, prompts for additional arguments to send to hurl."
  (interactive "P")
  (hurl-mode--send-request "--test")
  )

(defun hurl-mode-test-request-single (arg)
  "Hurl wrapper function to send the request at point for testing.
With prefix ARG, prompts for additional arguments to send to hurl."
  (interactive "P")
  (hurl-mode--send-request "--test")
  )

(setq hurl-mode-map
      (let ((map (make-sparse-keymap)))
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
;; Local Variables:
;; byte-compile-warnings: (not docstrings docstrings-wide)
;; End:
