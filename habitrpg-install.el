;;; el-get-install.el --- installer for the lazy
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
      (with-current-buffer buf
	(goto-char (point-max))
	(insert "\nCongrats, habitrpg is installed and ready to serve!")))))
