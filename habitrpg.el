;;; habitrpg.el --- org-mode interface to habitrpg using pyhabit

;; Copyright (C) 2013

;; Author:  ryjm <jraydermiller@gmail.com>
;; Keywords:

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

;; 
;; This is just a quick hack to get org-mode working with habitrpg.  It
;; requires the command line tool pyhabit
;; (https://github.com/xeross/pyhabit) and pyhabit-cli
;; (https://github.com/xeross/pyhabit-cli)
;; 
;; Add this to your .emacs:
;;
;; (add-to-list 'process-environment "HABIT_USER_ID=putidhere")
;; (add-to-list 'process-environment "HABIT_API_KEY=putkeyhere")
;; 
;; (add-hook 'org-after-todo-state-change-hook 'habitrpg-add)
;; (add-hook 'org-after-todo-state-change-hook 'habitrpg-done 'append)


;;; Code:

(provide 'habitrpg)

(require 'cl)
(require 'request)
(defcustom habitrpg-api-url "https://beta.habitrpg.com/api/v1"
  "API url"
  )
(defcustom habitrpg-api-user nil
  "API user"
  )
(defcustom habitrpg-api-token nil
  "API token"
  )

(defconst hrpg-repeat-interval 120)

(defvar hrpg-timer)  
(defvar hrpg-id "")
(defvar hrpg-status "")
(defvar hrpg-status-to-file nil)
(defvar hrpg-tags-list nil)

(defun habitrpg-add ()
  "Add to habitrpg.
With point on an `org-mode' headline add TASK if it isn't already there."
  (setq task (nth 4 (org-heading-components)))
  (setq habitp (member "hrpghabit" (org-get-tags-at)))
  (setq dailyp (member "hrpgdaily" (org-get-tags-at)))
  (setq rewardp (member "hrpgreward" (org-get-tags-at)))
  (cond
   (habitp (setq type "habit"))
   (dailyp (setq type "daily"))
   (rewardp (setq type "reward"))
   (t (setq type "todo")))
  (save-excursion
    (let* ((beg 
	    (progn
	      (org-back-to-heading)
	      (forward-line 1)
	      (point)))
           (end
	    (progn
	      (org-end-of-subtree)
	      (point)))
	   (text 
	    (progn
	      (buffer-substring beg end))))
    (habitrpg-get-id task)
    (unless (string=(nth 2 (org-heading-components)) "DONE")
      (if (string= (symbol-name (car hrpg-id)) "nil")
	  (habitrpg-create type task text))))))

(defun habitrpg-create (type task text)
  (request
     (concat habitrpg-api-url "/user/task/") 
     :type "POST"
     :headers `(("Accept" . "application/json")
		("X-API-User" . ,habitrpg-api-user)
		("X-API-Key" . ,habitrpg-api-token))
     :data `(("type" . ,type)
	     ("text" . ,task)
	     ("notes" . ,text))
     :parser 'json-read
     :success (message "Created task!")))

(defun habitrpg-done ()
  "Update TASK on habitrpg."
  (setq task (nth 4 (org-heading-components)))
  (if (string= (nth 2 (org-heading-components)) "DONE")
      (progn
	(habitrpg-get-id task)
	(habitrpg-upvote hrpg-id))))

(defun habitrpg-get-id (task)
    (request
     (concat habitrpg-api-url "/user")
     :headers `(("Accept" . "application/json")
		("X-API-User" . ,habitrpg-api-user)
		("X-API-Key" . ,habitrpg-api-token))
     :parser 'json-read
     :success (function*
	       (lambda (&key data &allow-other-keys)
		 (let* ((tasks (assoc-default 'tasks data))
			(names (mapcar (lambda (task-id)
				        (list (assoc-default 'text task-id) (car task-id))) tasks))
			(id (assoc-default task names)))
		   (setq hrpg-id id))))))

(defun habitrpg-upvote (hrpg-id &optional task type text direction)
  (if (string= (symbol-name (car hrpg-id)) "nil")
      (progn
 	(habitrpg-create type task text)
 	(habitrpg-get-id task))
    (request
     (concat habitrpg-api-url "/user/tasks/" (symbol-name (car hrpg-id)) "/"
	     (unless direction "up") direction)
     :type "POST"
     :headers `(("Content-Type" . "application/json")
		("Content-Length" . 0)
		("X-API-User" . ,habitrpg-api-user)
		("X-API-Key" . ,habitrpg-api-token))
     :parser 'json-read
     :success (function* (lambda (&key data &allow-other-keys) 
			   (message "Updated task!")
			   (if hrpg-status-to-file
			       (with-temp-file "~/tmp/hrpg-status"
				 (let* ((exp (assoc-default 'exp data))
					(gp (assoc-default 'gp data))
					(hp (assoc-default 'hp data))
					(lvl (assoc-default 'lvl data)))
				   (insert (concat "exp: " (number-to-string (truncate exp))
						   " gp: " (number-to-string (truncate gp))
						   " hp: " (number-to-string (truncate hp))
						   " lvl: " (number-to-string (truncate lvl))))))))))))

(defun habitrpg-get-id-at-point ()
  (interactive)
  (let ((point-task (buffer-substring (line-beginning-position) (line-end-position))))
    (habitrpg-get-id point-task)))


(defun habitrpg-upvote-at-point ()
  "Upvote a task. Add task if it doesn't exist."
  (interactive)
  (habitrpg-get-id-at-point)
  (let ((id hrpg-id))
    (habitrpg-upvote hrpg-id)))

(defun habitrpg-downvote-at-point ()
  "Upvote a task. Add task if it doesn't exist."
  (interactive)
  (let ((id (habitrpg-get-id-at-point)))
    (habitrpg-upvote id nil nil nil "down"))
  (habitrpg-refresh-status))

(defun habitrpg-clock-in ()
  "Upvote a clocking task based on tags.
Continuously upvote habits associated with the currently clocking task, based on tags specified in `hrpg-tags-list'."
  (setq task (car (intersection (org-get-tags-at) hrpg-tags-list :test 'equal)))
  (if task
      (progn
	(habitrpg-get-id task)
	(setq hrpg-timer (run-at-time nil hrpg-repeat-interval 'habitrpg-upvote hrpg-id task "habit" "")))))

(defun habitrpg-clock-out ()
  "Stop upvoting."
  (cancel-function-timers 'habitrpg-upvote))
	  
;;; habitrpg.el ends here
