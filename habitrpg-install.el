;;; habitrpg-install.el --- installer for the lazy
;;
;; Copyright (C) 2010 Dimitri Fontaine <modified for habitrpg.el by ryjm>
;;
;; Keywords: emacs package elisp install
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; bootstrap your habitica installation
;;
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working habitica

(let ((habitica-root
       (file-name-as-directory
	(or (bound-and-true-p habitica-dir)
	    (concat (file-name-as-directory user-emacs-directory) "habitica")))))

  (when (file-directory-p habitica-root)
    (add-to-list 'load-path habitica-root))

  ;; try to require habitrpg, failure means we have to install it
  (unless (require 'habitrpg nil t)
    (unless (file-directory-p habitica-root)
      (make-directory habitica-root t))

    (let* ((package   "habitica")
	   (buf       (switch-to-buffer "*habitica bootstrap*"))
	   (pdir      (file-name-as-directory (concat habitica-root package)))
	   (git       (or (executable-find "git")
			  (error "Unable to find `git'")))
	   (url       (or (bound-and-true-p habitica-git-install-url)
			  "http://github.com/ryjm/habitrpg.el.git"))
	   (config 
"(add-hook 'org-after-todo-state-change-hook 'habitica-add 'append)
;; For adding tasks from org mode
(global-set-key (kbd \"C-c C-x h\") 'habitica-add)
;; Status buffer - use C-h m to see the keybindings
;; C-c C-c - upvote task or buy reward
;; C-c C-d - downvote task
;; t - bring up manage menu, which adds or deletes tasks
(global-set-key (kbd \"<f9> a\") 'habitica-status)
;; Continuously update a habit attache to a clocking task
(add-hook 'org-clock-in-hook 'habitica-clock-in)
(add-hook 'org-clock-out-hook 'habitica-clock-out)
;; List of habits to check for when clocking a task
(add-to-list 'hrpg-tags-list \"PROGRAMMING\")
(add-to-list 'hrpg-tags-list \"WORK\")
")
	   (default-directory habitica-root)
	   (process-connection-type nil)   ; pipe, no pty (--no-progress)
	   ;; First clone habitica
	   (status
	    (call-process
	     git nil `(,buf t) t "--no-pager" "clone" "-v" url package)))
      
      (unless (zerop status)
	(error "Couldn't clone habitica from the Git repository: %s" url))

      ;; switch branch if we have to
      (let* ((branch "master")
             (remote-branch (format "origin/%s" branch))
             (default-directory pdir)
             (bstatus
	      (if (string-equal branch "master")
		  0
		(call-process git nil (list buf t) t "checkout" "-t" remote-branch))))
        (unless (zerop bstatus)
          (error "Couldn't `git checkout -t %s`" branch)))
      (add-to-list 'load-path pdir)
      (load package)
      (let ((id (progn 
		  (insert 
		  "What is your API key for habitica (found in your settings)?\n")
		 (read-from-minibuffer "API Key: ")))
	    (token (progn
		     (insert 
		      "What is your API token for habitica (found in your settings)?\n")
		     (read-from-minibuffer "API Token: "))))
	(with-current-buffer buf
	  (goto-char (point-max))
	  (insert "\nCongrats, habitica is installed and ready to serve!\n" 
		  "Here is the default configuration to put in your .emacs:\n"
		  config (format "(setq habitica-api-user \"%s\")" id) "\n"
		  (format "(setq habitica-api-token \"%s\")" token)))))))

		
