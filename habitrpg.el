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

;;; Code:



(provide 'habitrpg)


(defun habitrpg-add ()
  "With point on an org-mode headline, add to habitrpg if the task isn't already there."
   (setq task (nth 4 (org-heading-components)))
   (setq id (replace-regexp-in-string "\n$" "" 
				      (shell-command-to-string (concat "habit tasks | egrep 'text|id' | grep -B 1 \"" task "\" | sed -e 'q' | cut -d\"'\" -f4"))))
   (unless (string=(nth 2 (org-heading-components)) "DONE")
     (if (> 1 (string-to-number (replace-regexp-in-string "\n$" ""
					 (shell-command-to-string (concat "habit task " id " | wc -l")))))
       (shell-command (concat "habit create_task todo \"" task "\"")))))

(defun habitrpg-done ()
  "Update a task"
   (setq task (nth 4 (org-heading-components)))
  (if (string= (nth 2 (org-heading-components)) "DONE")
      (progn
	(setq id (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "habit tasks | egrep 'text|id' | grep -B 1 " task " | sed -e 'q' | cut -d\"'\" -f4"))))
	(shell-command (concat "habit perform_task " id " up")))))
;;; habitrpg.el ends here
