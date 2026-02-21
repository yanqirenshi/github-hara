;;; github-api.el --- GitHub GraphQL API client for github.el -*- lexical-binding: t; -*-

;; Author: yanqirenshi
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: github tools vc
;; URL: https://github.com/yanqirenshi/github-hara

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GitHub GraphQL API (v4) の非同期リクエスト基盤を提供する。
;; 各機能モジュールから (require 'github-api) して使う。

;;; Code:

(require 'url)
(require 'json)
(require 'github-variables)

(defun github-api--ensure-token ()
  "Ensure that `github-variable-token' is set."
  (unless github-variable-token
    (error "`github-variable-token' is not set")))

(defun github-api--parse-response (buffer)
  "Parse HTTP response from BUFFER and return JSON.
Signal an error if the response contains errors."
  (with-current-buffer buffer
    (goto-char (point-min))
    (re-search-forward "\n\n" nil t)
    (let* ((response (json-read))
           (errors (alist-get 'errors response)))
      (when errors
        (error "GitHub API error: %s"
               (mapconcat (lambda (e) (alist-get 'message e))
                          errors ", ")))
      response)))

(defun github-api--graphql-request-async (query variables callback &optional error-callback)
  "Send async request to GitHub GraphQL API.
QUERY is a GraphQL query string.  VARIABLES is an alist of variables.
CALLBACK is called with the response as (lambda (response) ...).
ERROR-CALLBACK is called with the error on failure.
If ERROR-CALLBACK is nil, error is signaled."
  (github-api--ensure-token)
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "bearer " github-variable-token))
            ("Content-Type" . "application/json")))
         (payload (json-encode `((query . ,query)
                                 (variables . ,(or variables :null)))))
         (url-request-data (encode-coding-string payload 'utf-8)))
    (url-retrieve
     "https://api.github.com/graphql"
     (lambda (status cb err-cb)
       (if (plist-get status :error)
           (progn
             (kill-buffer (current-buffer))
             (if err-cb
                 (funcall err-cb (plist-get status :error))
               (error "GitHub API request failed: %s"
                      (plist-get status :error))))
         (let ((response (github-api--parse-response (current-buffer))))
           (kill-buffer (current-buffer))
           (funcall cb response))))
     (list callback error-callback)
     t t)))

(provide 'github-api)
;;; github-api.el ends here
