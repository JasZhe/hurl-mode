;;; integration-test.el --- Integration tests with test server

(require 'ert)
(require 'hurl-mode)

(defvar hurl-test-server-port 8080)
(defvar hurl-test-server-process nil)

(defun hurl-test-start-server ()
  "Start the test server for integration tests."
  (let ((default-directory (expand-file-name "test-server" 
                                             (file-name-directory (locate-library "hurl-mode")))))
    (setq hurl-test-server-process
          (start-process "hurl-test-server" "*hurl-test-server*"
                         "go" "run" "main.go"))))

(defun hurl-test-stop-server ()
  "Stop the test server."
  (when (and hurl-test-server-process (process-live-p hurl-test-server-process))
    (kill-process hurl-test-server-process)
    (setq hurl-test-server-process nil)))

(defun hurl-test-wait-for-server (&optional timeout)
  "Wait for test server to be ready. TIMEOUT defaults to 10 seconds."
  (let ((timeout (or timeout 10))
        (start-time (current-time)))
    (while (and (< (float-time (time-subtract (current-time) start-time)) timeout)
                (not (hurl-test-server-ready-p)))
      (sleep-for 0.5))
    (hurl-test-server-ready-p)))

(defun hurl-test-server-ready-p ()
  "Check if test server is ready."
  (condition-case nil
      (with-current-buffer 
          (url-retrieve-synchronously "http://localhost:8080/health" nil nil 2)
        (goto-char (point-min))
        (search-forward "200 OK" nil t))
    (error nil)))

;; Integration tests
(ert-deftest hurl-integration-server-startup ()
  "Test that we can start and reach the test server."
  :tags '(integration)
  (skip-unless (executable-find "go"))
  (hurl-test-start-server)
  (unwind-protect
      (progn
        (should (hurl-test-wait-for-server))
        (should (hurl-test-server-ready-p)))
    (hurl-test-stop-server)))

(ert-deftest hurl-integration-health-endpoint ()
  "Test health endpoint with actual hurl execution."
  :tags '(integration)
  (skip-unless (executable-find "go"))
  (skip-unless (executable-find "hurl"))
  (hurl-test-start-server)
  (unwind-protect
      (when (hurl-test-wait-for-server)
        (let ((hurl-variables-file (make-temp-file "hurl-vars"))
              (response-received nil))
          (unwind-protect
              (hurl-test-with-temp-file
               "GET http://localhost:8080/health
HTTP/1.1 200
[Asserts]
jsonpath \"$.status\" == \"ok\""
               (cl-letf (((symbol-function 'display-buffer) #'ignore))
                 (let ((original-sentinel 
                        (lambda (p e) 
                          (when (not (process-live-p p))
                            (setq response-received t)
                            (with-current-buffer hurl-response--output-buffer-name
                              (ansi-color-apply-on-region (point-min) (point-max)))
                            (hurl-response--parse-and-filter-output)))))
                   (hurl-mode--send-request nil (buffer-file-name) original-sentinel)
                   ;; Wait for response
                   (let ((timeout 10)
                         (start-time (current-time)))
                     (while (and (not response-received)
                                 (< (float-time (time-subtract (current-time) start-time)) timeout))
                       (sleep-for 0.1)))
                   (should response-received)
                   ;; Check response buffer was created
                   (should (get-buffer hurl-response--buffer-name)))))
            (ignore-errors (delete-file hurl-variables-file)))))
    (hurl-test-stop-server)))

(ert-deftest hurl-integration-user-crud-operations ()
  "Test complete CRUD operations on users endpoint."
  :tags '(integration)
  (skip-unless (executable-find "go"))
  (skip-unless (executable-find "hurl"))
  (hurl-test-start-server)
  (unwind-protect
      (when (hurl-test-wait-for-server)
        (let ((hurl-variables-file (make-temp-file "hurl-vars"))
              (response-received nil))
          (unwind-protect
              (hurl-test-with-temp-file
               "# Get all users
GET http://localhost:8080/api/users
HTTP/1.1 200
[Asserts]
jsonpath \"$\" count == 2

# Create new user
POST http://localhost:8080/api/users
Content-Type: application/json
```json
{\"name\": \"Alice Johnson\", \"email\": \"alice@example.com\"}
```
HTTP/1.1 201
[Captures]
user_id: jsonpath \"$.id\"
[Asserts]
jsonpath \"$.name\" == \"Alice Johnson\"

# Get the created user
GET http://localhost:8080/api/users/{{user_id}}
HTTP/1.1 200
[Asserts]
jsonpath \"$.name\" == \"Alice Johnson\"

# Delete the user
DELETE http://localhost:8080/api/users/{{user_id}}
HTTP/1.1 204"
               (cl-letf (((symbol-function 'display-buffer) #'ignore))
                 (let ((original-sentinel 
                        (lambda (p e) 
                          (when (not (process-live-p p))
                            (setq response-received t)
                            (with-current-buffer hurl-response--output-buffer-name
                              (ansi-color-apply-on-region (point-min) (point-max)))
                            (hurl-response--parse-and-filter-output)))))
                   (hurl-mode--send-request nil (buffer-file-name) original-sentinel)
                   ;; Wait for response
                   (let ((timeout 15)
                         (start-time (current-time)))
                     (while (and (not response-received)
                                 (< (float-time (time-subtract (current-time) start-time)) timeout))
                       (sleep-for 0.1)))
                   (should response-received)
                   ;; Check that variables file was created with user_id
                   (should (file-exists-p hurl-variables-file))
                   (with-temp-buffer
                     (insert-file-contents hurl-variables-file)
                     (should (string-match-p "user_id=" (buffer-string)))))))
            (ignore-errors (delete-file hurl-variables-file)))))
    (hurl-test-stop-server)))

(ert-deftest hurl-integration-echo-endpoint ()
  "Test echo endpoint with headers and body."
  :tags '(integration)
  (skip-unless (executable-find "go"))
  (skip-unless (executable-find "hurl"))
  (hurl-test-start-server)
  (unwind-protect
      (when (hurl-test-wait-for-server)
        (let ((response-received nil))
          (hurl-test-with-temp-file
           "POST http://localhost:8080/echo
Content-Type: application/json
X-Test-Header: test-value
```json
{\"message\": \"hello world\"}
```
HTTP/1.1 200
[Asserts]
jsonpath \"$.method\" == \"POST\"
jsonpath \"$.body\" contains \"hello world\""
           (cl-letf (((symbol-function 'display-buffer) #'ignore))
             (let ((original-sentinel 
                    (lambda (p e) 
                      (when (not (process-live-p p))
                        (setq response-received t)
                        (with-current-buffer hurl-response--output-buffer-name
                          (ansi-color-apply-on-region (point-min) (point-max)))
                        (hurl-response--parse-and-filter-output)))))
               (hurl-mode--send-request nil (buffer-file-name) original-sentinel)
               ;; Wait for response
               (let ((timeout 10)
                     (start-time (current-time)))
                 (while (and (not response-received)
                             (< (float-time (time-subtract (current-time) start-time)) timeout))
                   (sleep-for 0.1)))
               (should response-received))))))
    (hurl-test-stop-server)))

(ert-deftest hurl-integration-delay-endpoint ()
  "Test delay endpoint functionality."
  :tags '(integration)
  (skip-unless (executable-find "go"))
  (skip-unless (executable-find "hurl"))
  (hurl-test-start-server)
  (unwind-protect
      (when (hurl-test-wait-for-server)
        (let ((response-received nil)
              (start-time (current-time)))
          (hurl-test-with-temp-file
           "GET http://localhost:8080/delay/1
HTTP/1.1 200
[Asserts]
jsonpath \"$.delayed\" == 1"
           (cl-letf (((symbol-function 'display-buffer) #'ignore))
             (let ((original-sentinel 
                    (lambda (p e) 
                      (when (not (process-live-p p))
                        (setq response-received t)
                        (hurl-response--parse-and-filter-output)))))
               (hurl-mode--send-request nil (buffer-file-name) original-sentinel)
               ;; Wait for response (should take at least 1 second)
               (let ((timeout 10)
                     (request-start (current-time)))
                 (while (and (not response-received)
                             (< (float-time (time-subtract (current-time) request-start)) timeout))
                   (sleep-for 0.1))
                 (should response-received)
                 ;; Should have taken at least 1 second
                 (should (>= (float-time (time-subtract (current-time) start-time)) 1.0))))))))
    (hurl-test-stop-server)))

(provide 'integration-test)
;;; integration-test.el ends here
