;;; test-habitrpg.el --- Tests for habitrpg.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Ali Ghaffaari <ali.ghaffaari at gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; How to run this test ?
;; $ Emacs -batch -L . -l test-habitrpg.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'habitrpg)

(ert-deftest test-habitrpg-headers-default ()
  "Test that `habitrpg-headers' returns the correct structure by default."
  (let ((habitrpg-api-user "test-user-id")
        (habitrpg-api-token "test-token")
        (habitrpg-api-client "habitrpg"))
    (let ((headers (habitrpg-headers)))
      (should (equal (assoc-default "X-API-User" headers) "test-user-id"))
      (should (equal (assoc-default "X-API-Key" headers) "test-token"))
      (should (equal (assoc-default "X-Client" headers) "test-user-id-habitrpg")))))

(ert-deftest test-habitrpg-headers-extra ()
  "Test that extra headers are correctly prepended."
  (let ((habitrpg-api-user "test-user-id")
        (habitrpg-api-token "test-token")
        (habitrpg-api-client "habitrpg"))
    (let ((headers (habitrpg-headers '(("Accept" . "application/json")))))
      (should (equal (assoc-default "Accept" headers) "application/json"))
      (should (equal (assoc-default "X-API-User" headers) "test-user-id"))
      (should (equal (assoc-default "X-API-Key" headers) "test-token"))
      (should (equal (assoc-default "X-Client" headers) "test-user-id-habitrpg")))))

(ert-deftest test-habitrpg-refresh-status ()
  "Test that `habitrpg-refresh-status` runs without throwing an error."
  (let ((status-buf (generate-new-buffer "*habitrpg:status*"))
        (other-buf (generate-new-buffer "*other-buffer*"))
        (request-buf (get-buffer-create "*request*"))
        (mock-user-data '((data . ((stats . ((exp . 10)
                                              (gp . 15.5)
                                              (hp . 50)
                                              (maxHealth . 50)
                                              (lvl . 3)
                                              (toNextLevel . 100)))
                                    (auth . ((local . ((username . "test-username")))))
                                    (id . "test-uid")
                                    (flags . ((rest . :json-false)))
                                    (preferences . ((dayStart . 0)))
                                    (items . ((eggs . ((wolf . 1)))
                                              (hatchingPotions . ((base . 1)))
                                              (pets . ((wolf-base . t)))))))))
        (mock-tasks-data '((data . [((completed . :json-false)
                                     (type . "todo")
                                     (text . "test-todo")
                                     (id . "todo-1")
                                     (value . 0)
                                     (notes . "todo notes"))
                                    ((completed . :json-false)
                                     (type . "habit")
                                     (text . "test-habit")
                                     (id . "habit-1")
                                     (value . 2)
                                     (notes . "habit notes"))
                                    ((completed . :json-false)
                                     (type . "daily")
                                     (text . "test-daily")
                                     (id . "daily-1")
                                     (value . 0)
                                     (notes . "daily notes"))
                                    ((completed . :json-false)
                                     (type . "reward")
                                     (text . "test-reward")
                                     (id . "reward-1")
                                     (value . 10)
                                     (notes . "reward notes"))])))
        (mock-store-data '((data . [((key . "potion-base") (value . 15))]))))
    (unwind-protect
        (with-current-buffer status-buf
          (habitrpg-status-mode)
          ;; Mock request function
          (cl-letf (((symbol-function 'request)
                     (lambda (url &rest plist)
                       (let ((success-cb (plist-get plist :success))
                             (data (cond
                                    ((string-suffix-p habitrpg-api-usertask-path url)
                                     mock-tasks-data)
                                    ((string-suffix-p (concat habitrpg-api-inventory-path "/buy") url)
                                     mock-store-data)
                                    ((string-suffix-p habitrpg-api-user-path url)
                                     mock-user-data)
                                    (t (error "Unknown mock URL: %s" url)))))
                         ;; Switch to other-buf to simulate request's callback execution context
                         (with-current-buffer other-buf
                           (funcall success-cb :data data))))))
            ;; Executing habitrpg-refresh-status should succeed and not throw error
            (should (progn
                      (habitrpg-refresh-status)
                      t))))
      (when (buffer-live-p status-buf) (kill-buffer status-buf))
      (when (buffer-live-p other-buf) (kill-buffer other-buf))
      (when (buffer-live-p request-buf) (kill-buffer request-buf)))))

(ert-deftest test-habitrpg-get-id-error ()
  "Test habitrpg-get-id to reproduce 'it' void variable error."
  (let ((mock-tasks-data '((data . [((completed . :json-false)
                                     (type . "todo")
                                     (text . "test-task")
                                     (id . "todo-1"))]))))
    (cl-letf* (((symbol-function 'request)
                (lambda (url &rest plist)
                  (let ((complete-cb (plist-get plist :complete))
                        (mock-response (make-request-response :data mock-tasks-data)))
                    (funcall complete-cb :response mock-response)))))
      (should (progn
                (habitrpg-get-id "test-task" (lambda (id) (message "Id: %s" id)))
                t)))))

(provide 'test-habitrpg)
;;; test-habitrpg.el ends here
