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

(defun habitrpg-add ()
  "Add to habitrpg.
With point on an `org-mode' headline, use the shell command
   `habit` to add to habitrpg if TASK isn't already there."
  (setq task (nth 4 (org-heading-components)))
  (setq habitp (member "hrpghabit" (org-get-tags-at)))
  (setq dailyp (member "hrpgdaily" (org-get-tags-at)))
  (cond
   (habitp (setq type "habit"))
   (dailyp (setq type "daily"))
   (t (setq type "todo")))
  (setq id (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "habit tasks | egrep 'text|id' | grep -B 1 \"" task "\" | sed -e 'q' | cut -d\"'\" -f4 &"))))
  (unless (string=(nth 2 (org-heading-components)) "DONE")
    (if (> 1 (string-to-number (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "habit task " id " | wc -l &")))))
	(shell-command (concat "habit create_task " type " \"" task "\" &")))))

(defun habitrpg-done ()
  "Update TASK on habitrpg."
  (setq task (nth 4 (org-heading-components)))
  (if (string= (nth 2 (org-heading-components)) "DONE")
      (progn
	(setq id (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "habit tasks | egrep 'text|id' | grep -B 1 \"" task "\" | sed -e 'q' | cut -d\"'\" -f4 &"))))
	(shell-command (concat "habit perform_task " id " up &")))))

;;; habitrpg.el ends here
