;;; hurl-mode-test.el --- Tests for hurl-mode

(require 'ert)
(require 'hurl-mode)

;; Test helpers
(defun hurl-test-with-temp-file (content &rest body)
  "Execute BODY with a temporary hurl file containing CONTENT."
  (let ((temp-file (make-temp-file "hurl-test" nil ".hurl")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert content))
          (with-temp-buffer
            (insert-file-contents temp-file)
            (hurl-mode)
            (setq buffer-file-name temp-file)
            (eval `(progn ,@body))))
      (delete-file temp-file))))

(defmacro hurl-test-with-mock-process (&rest body)
  "Execute BODY with mocked process functions."
  `(cl-letf (((symbol-function 'start-file-process)
              (lambda (name buffer program &rest args)
                (setq hurl-test-captured-command (string-join args " "))
                ;; Return a mock process
                (let ((proc (make-process :name name :buffer buffer :command '("echo" "mock"))))
                  (set-process-query-on-exit-flag proc nil)
                  proc)))
             ((symbol-function 'set-process-filter) #'ignore)
             ((symbol-function 'set-process-sentinel) #'ignore))
     (let ((hurl-test-captured-command nil))
       ,@body)))

;; Variables for test state
(defvar hurl-test-captured-command nil)

;;; Basic functionality tests

(ert-deftest hurl-test-mode-activation ()
  "Test that hurl-mode activates correctly."
  (with-temp-buffer
    (hurl-mode)
    (should (eq major-mode 'hurl-mode))
    (should (boundp 'hurl-mode-map))))

(ert-deftest hurl-test-http-method-fontification ()
  "Test that HTTP methods are properly fontified."
  (with-temp-buffer
    (insert "GET http://example.com\nPOST http://api.com")
    (hurl-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'hurl-mode-method-face))))

(ert-deftest hurl-test-section-header-fontification ()
  "Test that section headers are properly fontified."
  (with-temp-buffer
    (insert "[Asserts]\n[Captures]\n[Options]")
    (hurl-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'hurl-mode-section-face))))

;;; Request extraction tests

(ert-deftest hurl-test-single-request-extraction ()
  "Test extracting a single request from buffer."
  (hurl-test-with-temp-file 
   "GET http://localhost:8080/api/users
HTTP/1.1 200

POST http://localhost:8080/api/users
Content-Type: application/json
{\"name\": \"test\"}
HTTP/1.1 201"
   (goto-char (point-min))
   (hurl-test-with-mock-process
    (hurl-mode-send-request-single nil)
    ;; Verify the temp file was created
    (should (file-exists-p hurl-mode--temp-file-name)))))

;;; Variable capture parsing tests

(ert-deftest hurl-test-parse-captures-from-server-response ()
  "Test parsing variable captures from realistic server response."
  (let ((mock-output "* Executing entry 1
* Request:
> POST http://localhost:8080/api/users
> Content-Type: application/json
> 
> {\"name\": \"Alice Johnson\", \"email\": \"alice@example.com\"}
* Response:
< HTTP/1.1 201
< Content-Type: application/json
< 
* Response body:
{\"id\": 3, \"name\": \"Alice Johnson\", \"email\": \"alice@example.com\"}
* Captures:
* user_id: 3
* user_name: Alice Johnson
* Timings:
* Total: 45ms
*"))
    (with-temp-buffer
      (insert mock-output)
      (let ((hurl-response--output-buffer-name (buffer-name))
            (hurl-variables-file (make-temp-file "hurl-vars")))
        (unwind-protect
            (progn
              (should-not-error (hurl-response--parse-and-filter-output))
              ;; Check that variables file was created
              (should (file-exists-p hurl-variables-file))
              (with-temp-buffer
                (insert-file-contents hurl-variables-file)
                (should (string-match-p "user_id=3" (buffer-string)))
                (should (string-match-p "user_name=Alice Johnson" (buffer-string)))))
          (delete-file hurl-variables-file))))))

(ert-deftest hurl-test-parse-json-response ()
  "Test parsing and formatting JSON responses."
  (let ((json-response "{\"users\":[{\"id\":1,\"name\":\"John Doe\",\"email\":\"john@example.com\"}]}"))
    (should (stringp (hurl-response--format-json json-response)))
    ;; Should format the JSON nicely
    (should (string-match-p "{\n" (hurl-response--format-json json-response)))))

(ert-deftest hurl-test-json-parse-error-handling ()
  "Test handling of malformed JSON responses."
  (should-error 
   (hurl-response--format-json "invalid json {")
   :type 'hurl-json-parse-error))

(ert-deftest hurl-test-variables-file-update ()
  "Test that existing variables are updated in variables file."
  (let ((hurl-variables-file (make-temp-file "hurl-vars"))
        (mock-output "* Captures:
* user_id: 456
* token: new_token_value
* Timings:
* Total: 30ms
*"))
    (unwind-protect
        (progn
          ;; Create initial variables file
          (with-temp-file hurl-variables-file
            (insert "user_id=123\napi_key=secret\n"))
          (with-temp-buffer
            (insert "* Executing entry 1
* Request:
> GET http://localhost:8080/api/users/123
* Response:
< HTTP/1.1 200
< Content-Type: application/json
* Response body:
{\"id\": 456, \"token\": \"new_token_value\"}
" mock-output)
            (let ((hurl-response--output-buffer-name (buffer-name)))
              (hurl-response--parse-and-filter-output)
              ;; Check that user_id was updated but api_key preserved
              (with-temp-buffer
                (insert-file-contents hurl-variables-file)
                (let ((content (buffer-string)))
                  (should (string-match-p "user_id=456" content))
                  (should (string-match-p "token=new_token_value" content))
                  (should (string-match-p "api_key=secret" content)))))))
      (delete-file hurl-variables-file))))

;;; Command building tests

(ert-deftest hurl-test-command-building-basic ()
  "Test that hurl commands are built correctly."
  (hurl-test-with-temp-file
   "GET http://localhost:8080/health"
   (hurl-test-with-mock-process
    (hurl-mode-send-request-file nil)
    (should (string-match-p "--very-verbose" hurl-test-captured-command))
    (should (string-match-p "\\.hurl" hurl-test-captured-command)))))

(ert-deftest hurl-test-variables-file-inclusion ()
  "Test that variables file is included when it exists."
  (let ((hurl-variables-file (make-temp-file "hurl-vars")))
    (unwind-protect
        (progn
          (with-temp-file hurl-variables-file
            (insert "api_key=test123\nuser_id=42\n"))
          (hurl-test-with-temp-file
           "GET http://localhost:8080/api/users/{{user_id}}"
           (hurl-test-with-mock-process
            (hurl-mode-send-request-file nil)
            (should (string-match-p "--variables-file" hurl-test-captured-command))
            (should (string-match-p (regexp-quote hurl-variables-file) hurl-test-captured-command)))))
      (delete-file hurl-variables-file))))

(ert-deftest hurl-test-test-mode-command ()
  "Test that test mode adds --test flag."
  (hurl-test-with-temp-file
   "GET http://localhost:8080/health
HTTP/1.1 200
[Asserts]
jsonpath \"$.status\" == \"ok\""
   (hurl-test-with-mock-process
    (hurl-mode-test-request-file nil)
    (should (string-match-p "--test" hurl-test-captured-command)))))

;;; Response parsing tests

(ert-deftest hurl-test-response-parsing-with-headers ()
  "Test parsing response with headers and JSON body."
  (let ((mock-output "* Executing entry 1
* Request:
> GET http://localhost:8080/api/users
* Response:
< HTTP/1.1 200
< Content-Type: application/json
< X-Custom-Header: test-value
< 
* Response body:
[{\"id\": 1, \"name\": \"John Doe\", \"email\": \"john@example.com\"}, {\"id\": 2, \"name\": \"Jane Smith\", \"email\": \"jane@example.com\"}]
* Timings:
* Total: 25ms
*"))
    (with-temp-buffer
      (insert mock-output)
      (let ((hurl-response--output-buffer-name (buffer-name)))
        (should-not-error (hurl-response--parse-and-filter-output))
        ;; Check that response buffer was created
        (should (get-buffer hurl-response--buffer-name))))))

(ert-deftest hurl-test-response-parsing-with-delay ()
  "Test parsing response from delay endpoint."
  (let ((mock-output "* Executing entry 1
* Request:
> GET http://localhost:8080/delay/2
* Response:
< HTTP/1.1 200
< Content-Type: application/json
< 
* Response body:
{\"delayed\": 2, \"message\": \"Delayed for 2 seconds\"}
* Timings:
* Total: 2050ms
*"))
    (with-temp-buffer
      (insert mock-output)
      (let ((hurl-response--output-buffer-name (buffer-name)))
        (should-not-error (hurl-response--parse-and-filter-output))))))

;;; Utility function tests

(ert-deftest hurl-test-inside-block-detection ()
  "Test detection of being inside code blocks."
  (with-temp-buffer
    (insert "POST http://localhost:8080/api/users
Content-Type: application/json
```json
{\"name\": \"Alice\", \"email\": \"alice@example.com\"}
```
HTTP/1.1 201")
    (hurl-mode)
    (goto-char (point-min))
    (search-forward "Alice")
    (should (hurl-inside-block-p))
    (goto-char (point-min))
    (should-not (hurl-inside-block-p))))

(ert-deftest hurl-test-template-fontification ()
  "Test that template variables are properly fontified."
  (with-temp-buffer
    (insert "GET http://localhost:8080/api/users/{{user_id}}")
    (hurl-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "{{")
    (should (eq (get-text-property (point) 'face) 'hurl-mode-template-face))))

;;; Full request cycle tests

(ert-deftest hurl-test-full-user-creation-cycle ()
  "Test a complete user creation request cycle with captures."
  (let ((hurl-variables-file (make-temp-file "hurl-vars"))
        (mock-response "* Executing entry 1
* Request:
> POST http://localhost:8080/api/users
> Content-Type: application/json
> 
> {\"name\": \"Test User\", \"email\": \"test@example.com\"}
* Response:
< HTTP/1.1 201
< Content-Type: application/json
< 
* Response body:
{\"id\": 42, \"name\": \"Test User\", \"email\": \"test@example.com\"}
* Captures:
* new_user_id: 42
* new_user_name: Test User
* Timings:
* Total: 150ms
*"))
    (unwind-protect
        (hurl-test-with-temp-file
         "POST http://localhost:8080/api/users
Content-Type: application/json
```json
{\"name\": \"Test User\", \"email\": \"test@example.com\"}
```
HTTP/1.1 201
[Captures]
new_user_id: jsonpath \"$.id\"
new_user_name: jsonpath \"$.name\""
         ;; Mock the process output
         (cl-letf (((symbol-function 'hurl-response--verbose-filter)
                    (lambda (proc str)
                      (with-current-buffer (get-buffer-create hurl-response--output-buffer-name)
                        (insert mock-response)))))
           (hurl-test-with-mock-process
            (hurl-mode-send-request-single nil)
            ;; Verify temp file was created
            (should (file-exists-p hurl-mode--temp-file-name)))))
      (ignore-errors (delete-file hurl-variables-file)))))

(ert-deftest hurl-test-status-code-endpoint ()
  "Test request to status code endpoint."
  (hurl-test-with-temp-file
   "GET http://localhost:8080/status/404
HTTP/1.1 404"
   (hurl-test-with-mock-process
    (hurl-mode-send-request-single nil)
    (should (string-match-p "status/404" hurl-test-captured-command)))))

(provide 'hurl-mode-test)
;;; hurl-mode-test.el ends here
