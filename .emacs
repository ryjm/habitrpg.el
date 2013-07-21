(add-to-list 'load-path "path/to/repo/habitrpg.el")
(setq habitrpg-api-user "ID-HERE")
(setq habitrpg-api-token "TOKEN-HERE")
(add-hook 'org-after-todo-state-change-hook 'habitrpg-add 'append)

;; For adding tasks from org mode
(global-set-key (kbd "C-c C-x h") 'habitrpg-add)
;; Status buffer - use C-h m to see the keybindings 
;; C-c C-c - upvote task or buy reward
;; C-c C-d - downvote task 
;; t - bring up manage menu, which adds or deletes tasks
(global-set-key (kbd "<f9> a") 'habitrpg-status) 

;; Continuously update a habit attache to a clocking task
(add-hook 'org-clock-in-hook 'habitrpg-clock-in)
(add-hook 'org-clock-out-hook 'habitrpg-clock-out)

;; List of habits to check for when clocking a task
(add-to-list 'hrpg-tags-list "PROGRAMMING")
(add-to-list 'hrpg-tags-list "WORK")

