;;; habitrpg-install.el --- installer for the lazy
;;
;; Copyright (C) 2010 Dimitri Fontaine <modified for habitrpg.el by ryjm>
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: http://www.emacswiki.org/emacs/el-get.el
;; Created: 2010-06-17
;; Keywords: emacs package elisp install elpa git git-svn bzr cvs apt-get fink http http-tar
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;
;; bootstrap your habitrpg installation
;;
;; So the idea is that you copy/paste this code into your *scratch* buffer,
;; hit C-j, and you have a working habitrpg

(let ((habitrpg-root
       (file-name-as-directory
	(or (bound-and-true-p habitrpg-dir)
	    (concat (file-name-as-directory user-emacs-directory) "habitrpg")))))

  (when (file-directory-p habitrpg-root)
    (add-to-list 'load-path habitrpg-root))

  ;; try to require habitrpg, failure means we have to install it
  (unless (require 'habitrpg nil t)
    (unless (file-directory-p habitrpg-root)
      (make-directory habitrpg-root t))

    (let* ((package   "habitrpg")
	   (buf       (switch-to-buffer "*habitrpg bootstrap*"))
	   (pdir      (file-name-as-directory (concat habitrpg-root package)))
	   (git       (or (executable-find "git")
			  (error "Unable to find `git'")))
	   (url       (or (bound-and-true-p habitrpg-git-install-url)
			  "http://github.com/ryjm/habitrpg.el.git"))
	   (config 
"(add-hook 'org-after-todo-state-change-hook 'habitrpg-add 'append)
;; For adding tasks from org mode
(global-set-key (kbd \"C-c C-x h\") 'habitrpg-add)
;; Status buffer - use C-h m to see the keybindings
;; C-c C-c - upvote task or buy reward
;; C-c C-d - downvote task
;; t - bring up manage menu, which adds or deletes tasks
(global-set-key (kbd \"<f9> a\") 'habitrpg-status)
;; Continuously update a habit attache to a clocking task
(add-hook 'org-clock-in-hook 'habitrpg-clock-in)
(add-hook 'org-clock-out-hook 'habitrpg-clock-out)
;; List of habits to check for when clocking a task
(add-to-list 'hrpg-tags-list \"PROGRAMMING\")
(add-to-list 'hrpg-tags-list \"WORK\")
")
	   (default-directory habitrpg-root)
	   (process-connection-type nil)   ; pipe, no pty (--no-progress)
	   ;; First clone habitrpg
	   (status
	    (call-process
	     git nil `(,buf t) t "--no-pager" "clone" "-v" url package)))
      
      (unless (zerop status)
	(error "Couldn't clone habitrpg from the Git repository: %s" url))

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
		  "What is your API key for habitrpg (found in your settings)?\n")
		 (read-from-minibuffer "API Key: ")))
	    (token (progn
		     (insert 
		      "What is your API token for habitrpg (found in your settings)?\n")
		     (read-from-minibuffer "API Token: "))))
	(with-current-buffer buf
	  (goto-char (point-max))
	  (insert "\nCongrats, habitrpg is installed and ready to serve!\n" 
		  "Here is the default configuration to put in your .emacs:\n"
		  config (format "(setq habitrpg-api-user \"%s\")" id) "\n"
		  (format "(setq habitrpg-api-token \"%s\")" token)))))))

		
