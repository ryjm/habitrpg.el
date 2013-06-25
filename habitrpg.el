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

(defconst hrpg-repeat-interval 120)

(defvar hrpg-timer)  
(defvar hrpg-id "")
(defvar hrpg-status "")
(defvar hrpg-status-to-file nil)
(defvar hrpg-tags-list nil)

(defun habitrpg-add ()
  "Add to habitrpg.
With point on an `org-mode' headline, use the shell command
   `habit` to add to habitrpg if TASK isn't already there."
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
	      (line-move 1)
	      (point)))
           (end
	    (progn
	      (org-end-of-subtree)
	      (point)))
	   (text 
	    (progn
	      (buffer-substring beg end))))
    (habitrpg-get-id)
    (unless (string=(nth 2 (org-heading-components)) "DONE")
      (if (> 1 (string-to-number (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "habit task " hrpg-id " | wc -l &")))))
	  (habitrpg-create type task text))))))

(defun habitrpg-create (type task text)
  (shell-command-to-string (concat "habit create_task " type " \"" task "\" False 0 \"" text "\" &")))

(defun habitrpg-done ()
  "Update TASK on habitrpg."
  (setq task (nth 4 (org-heading-components)))
  (if (string= (nth 2 (org-heading-components)) "DONE")
      (progn 
	(habitrpg-get-id)
	(habitrpg-upvote hrpg-id))))

(defun habitrpg-get-id ()
  (setq hrpg-id (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "habit tasks | egrep 'text|id' | grep -B 1 \"" task "\" | sed -e 'q' | cut -d\"'\" -f4 &")))))

(defun habitrpg-upvote (hrpg-id &optional task type text)
  "Upvote a task. Add task if it doesn't exist."
  (if (string= hrpg-id "")
      (progn
	(habitrpg-create type task text)
	(habitrpg-get-id))
    (setq hrpg-status (shell-command-to-string (concat "habit perform_task " hrpg-id " up &"))))
  (if hrpg-status-to-file
      (with-temp-file "~/tmp/hrpg-status"
	(insert hrpg-status))))


(defun habitrpg-clock-in ()
  "Upvote a clocking task based on tags.
Continuously upvote habits associated with the currently clocking task, based on tags specified in `hrpg-tags-list'."
  (setq task (car (intersection (org-get-tags-at) hrpg-tags-list :test 'equal)))
  (if task
      (progn
	(habitrpg-get-id)
	(setq hrpg-timer (run-at-time nil hrpg-repeat-interval 'habitrpg-upvote hrpg-id task "habit" "")))))

(defun habitrpg-clock-out ()
  "Stop upvoting."
  (cancel-function-timers 'habitrpg-upvote))
	  
;;; habitrpg.el ends here
