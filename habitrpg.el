;;; habitrpg.el --- org-mode interface to habitrpg

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


;; habitrpg.el ===============
;;
;; Integrate org-mode and habitrpg.  Very much a work in progress.  Adds
;; a task to habitrpg.com after a TODO state change in org-mode or by
;; calling the function `habitrpg-add`.
;;
;; Tag (in org-mode) your habits and dailys with `hrpghabit`,
;; `hrpgdaily`, and `hrpgreward` to get them in the corresponding
;; categories.
;;
;; Easy Install -----------
;;
;; Put this in an empty buffer (`*scratch*`for example), move cursor
;; to bottom, and hit C-j.
;;
;; ```lisp ;; C-j at end of this block to install habitrpg.el
;; (url-retrieve
;; "https://raw.github.com/ryjm/habitrpg.el/master/habitrpg-install.el"
;; (lambda (s) (goto-char (point-max)) (eval-print-last-sexp))) ```
;;
;; That's it! You will be prompted for an API key and token, or you
;; can insert the default configuration into your .emacs (it will
;; print in a buffer so you can copy and paste).
;;
;; Installation ------------
;;
;; Install `request.el` and `deferred.el`, which you can get through
;; el-get or package.el (M-x package-list-packages).  I also included
;; them in this repo.
;;
;; Clone this repo: `git clone https://github.com/ryjm/habitrpg.el`
;;
;; Add to your .emacs (if you used the install script the path will be
;; in .emacs.d):
;;
;;     (add-to-list 'load-path "path/to/repo/habitrpg.el") (setq
;;     habitrpg-api-user "ID-HERE") (setq habitrpg-api-token
;;     "TOKEN-HERE")
;;
;; Add this hook if you want a DONE task to be marked as complete and
;; a todo state change to add a task to habitrpg.com
;;
;;      (add-hook 'org-after-todo-state-change-hook 'habitrpg-add
;;      'append)
;;
;; Add keybindings.
;;
;;     (global-set-key (kbd "C-c C-x h") 'habitrpg-add)
;;      (global-set-key (kbd "<f9> a") 'habitrpg-status)
;;
;; You can then bring up the habitrpg buffer with `<f9> a`, and do
;; `C-h m` to see the keybindings.
;;
;; ![buffer](http://i.imgur.com/M5EfSkd.png)
;; ![buffer2](http://i.imgur.com/w3XIzL9.gif)
;;
;; -------------------------------------------------------------------------------
;;
;; If you want to use the clocking feature:
;;
;;      (add-hook 'org-clock-in-hook 'habitrpg-clock-in) (add-hook
;;      'org-clock-out-hook 'habitrpg-clock-out)
;;
;; and set the variable `hrpg-tags-list` to the habits you want to
;; associate with the clocked task.
;;
;;     (add-to-list 'hrpg-tags-list "PROGRAMMING") (add-to-list
;;     'hrpg-tags-list "WORK")
;;
;; Then your habit will get upvoted every two minutes.

;; Most of the code for the status buffer was taken from the Magit
;;project.  I really like the way they set up the sections, it's very
;;modular so you can add different sections easily.  This will be
;;useful for when habitrpg gets more features.
;;; Code:



(require 'cl)
(require 'json)
(unless (require 'deferred nil t)
  (load-file "deferred.el"))
(unless (require 'request nil t)
  (load-file "request.el"))
(unless (require 'request-deferred nil t)
  (load-file "request-deferred.el"))
(require 'ansi-color)
(require 'thingatpt)
(require 'ring)

;; Silences byte-compiler warnings
(eval-and-compile
  (unless (fboundp 'declare-function)
    (defmacro declare-function (&rest args))))

(defgroup habitrpg nil
  "Controlling habitrpg from Emacs."
  :prefix "habitrpg-"
  :group 'tools)

(defcustom habitrpg-api-url "https://habitica.com/api/v3"
  "API url."
  :group 'habitrpg)
(defcustom habitrpg-api-user nil
  "API user id."
  :group 'habitrpg)
(defcustom habitrpg-api-token nil
  "API token."
  :group 'habitrpg)
(defcustom habitrpg-api-usertask-path "/tasks/user"
  "API Tasks Path"
  :group 'habitrpg)
(defcustom habitrpg-api-user-path "/user"
  "API User Path"
  :group 'habitrpg)
(defcustom habitrpg-api-tasks-path "/tasks"
  "API Task Path"
  :group 'habitrpg)
(defcustom habitrpg-api-inventory-path "/user/inventory"
  "API Inventory Path"
  :group 'habitrpg)

(cl-eval-when (load eval)
  (defalias 'habitrpg-set-variable-and-refresh 'set-default))


(defgroup habitrpg-faces nil
  "Customize the appearance of Habitrpg."
  :prefix "habitrpg-"
  :group 'faces
  :group 'habitrpg)

(defface habitrpg-header
  '((t :inherit header-line))
  "Face for generic header lines.
Many Habitrpg faces inherit from this one by default."
  :group 'habitrpg-faces)

(defface habitrpg-section-title
  '((t :inherit habitrpg-header))
  "Face for section titles."
  :group 'habitrpg-faces)

(defface habitrpg-tag
  '((t :inherit habitrpg-header))
  "Face for tags."
  :group 'habitrpg-faces)

(defface habitrpg-item-mark
  '((t :inherit secondary-selection))
  "Face for highlighting marked item."
  :group 'habitrpg-faces)

(defface habitrpg-header
  '((t :inherit header-line))
  "Face for generic header lines.

Many Habitrpg faces inherit from this one by default."
  :group 'habitrpg-faces)

(defface habitrpg-day
  '((((class color) (background light))
     :foreground "grey11")
    (((class color) (background dark))
     :foreground "grey80"))
  "face"
  :group 'habitrpg-faces)

(defface habitrpg-user
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "face"
  :group 'habitrpg-faces)

(defface habitrpg-nextlvl
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "face"
  :group 'habitrpg-faces)

(defface habitrpg-exp
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for exp."
  :group 'habitrpg-faces)

(defface habitrpg-lvl
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "OliveDrab4")
    (((class color) (background dark))
     :box t
     :background "Grey11"
     :foreground "DarkSeaGreen2"))
  "Face for level."
  :group 'habitrpg-faces)

(defface habitrpg-gold
  '((((class color) (background light))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4")
    (((class color) (background dark))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4"))
  "Face for gold."
  :group 'habitrpg-faces)

(defface habitrpg-hp
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for hp."
  :group 'habitrpg-faces)

(defface habitrpg-maxhp
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "LightSkyBlue4")
    (((class color) (background dark))
     :box t
     :background "Grey13"
     :foreground "LightSkyBlue1"))
  "face"
  :group 'habitrpg-faces)

(defvar habitrpg-tmp-buffer-name " *habitrpg-tmp*")
(defvar habitrpg-header-line-string nil
  "Header line that shows when you are clocked into a habit that is to be downvoted.")

(defconst hrpg-repeat-interval 120)
(defvar habitrpg-mode-hook nil "Hook run by `habitrpg-status-mode'.")

(defvar hrpg-timer)
(defvar hrpg-status-to-file nil)
(defvar hrpg-tags-list nil)
(defvar hrpg-bad-tags-list nil
  "List of org mode tags that specify a habit which should be downvoted.
This is an alist where each element is of the
  form (HABIT . TIME), where TIME is a string (like \"1 hour\")
  specifying when we should start downvoting this habit.")

(defvar hrpg-to-upvote-ids nil
  "List of IDs that need to be upvoted.")
(defvar hrpg-to-add nil
  "List of tasks that need to be added.")
(defvar habitrpg-refresh-function nil)
(make-variable-buffer-local 'habitrpg-refresh-function)
(put 'habitrpg-refresh-function 'permanent-local t)

(defvar habitrpg-refresh-args nil)
(make-variable-buffer-local 'habitrpg-refresh-args)
(put 'habitrpg-refresh-args 'permanent-local t)

(defvar habitrpg-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'habitrpg-goto-next-section)
    (define-key map (kbd "p") 'habitrpg-goto-previous-section)
    (define-key map (kbd "^") 'habitrpg-goto-parent-section)
    (define-key map (kbd "M-n") 'habitrpg-goto-next-sibling-section)
    (define-key map (kbd "M-p") 'habitrpg-goto-previous-sibling-section)
    (define-key map (kbd "TAB") 'habitrpg-toggle-section)
    (define-key map (kbd "RET") 'habitrpg-search-task-name)
    (define-key map (kbd "<backtab>") 'habitrpg-expand-collapse-section)
    (define-key map (kbd "C-c C-c") 'habitrpg-upvote-at-point)
    (define-key map (kbd "C-c C-x C-i") 'habitrpg-clock-in-status)
    (define-key map (kbd "C-c C-d") 'habitrpg-downvote-at-point)
    (define-key map (kbd "t") 'habitrpg-key-mode-popup-manage)
    (define-key map (kbd "g") 'habitrpg-refresh)
    (define-key map (kbd "G") 'habitrpg-refresh-all)
    (define-key map (kbd "q") 'habitrpg-quit-window)
    map))

(defcustom habitrpg-status-buffer-switch-function 'pop-to-buffer
  "Function for `habitrpg-status' to use for switching to the status buffer.

The function is given one argument, the status buffer."
  :group 'habitrpg
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

(defvar habitrpg-status-line-align-to 9)
(defun habitrpg-insert-status-line (heading info-string)
  (insert heading "/"
          (make-string (max 1 (- habitrpg-status-line-align-to
                                 (length heading))) ?\ )
          info-string "\n"))

;;
;; HabitRPG Status Buffer
;;

;;;###autoload
(defun habitrpg-status ()
  (interactive)
  (when (and (not habitrpg-api-user) (not habitrpg-api-token))
    (setq habitrpg-api-user (read-from-minibuffer "API User ID: ")
	  habitrpg-api-token (read-from-minibuffer "API Token: ")))
  (let ((buf (or (habitrpg-find-status-buffer 'habitrpg-status-mode)
		 (generate-new-buffer
		  "*habitrpg:status*"))))
    (funcall habitrpg-status-buffer-switch-function buf)
    (habitrpg-mode-init 'habitrpg-status-mode #'habitrpg-refresh-status)))

(defun habitrpg-find-status-buffer (submode)
  (cl-find-if (lambda (buf)
		(with-current-buffer buf
		  (eq major-mode submode)))
	      (buffer-list)))

(defun habitrpg-mode-init (submode refresh-func &rest refresh-args)
  (setq habitrpg-refresh-function refresh-func
        habitrpg-refresh-args refresh-args)
  (funcall submode)
  (habitrpg-refresh-buffer))

(defun habitrpg-refresh-status ()
  (habitrpg-do-backlog)
  (setq header-line-format habitrpg-header-line-string)
  (habitrpg-create-buffer-sections
    (habitrpg-with-section 'status nil
      (request
       (concat habitrpg-api-url habitrpg-api-user-path)
       :type "GET"
       :parser 'json-read
       :headers `(("Accept" . "application/json")
		  ("X-API-User" . ,habitrpg-api-user)
		  ("X-API-Key" . ,habitrpg-api-token))
       :sync t
       :success (function*
		 (lambda (&key data &allow-other-keys)
		   (let* ((data (assoc-default 'data data))
			  (stats (assoc-default 'stats data))
			  ;; stats
			  (exp (assoc-default 'exp stats))
			  (gp (assoc-default 'gp stats))
			  (hp (assoc-default 'hp stats))
			  (maxhp (assoc-default 'maxHealth stats))
			  (lvl (assoc-default 'lvl stats))
			  (nextlvl (assoc-default 'toNextLevel stats))
			  ;; auth info
			  (auth (assoc-default 'auth data))
			  (local (assoc-default 'local auth))
			  (facebook (assoc-default 'facebook auth))
			  (timestamps (assoc-default 'timestamps auth))
			  (user (cond
				 ((assoc-default 'username local))
				 ((assoc-default 'username facebook))))
			  (born (assoc-default 'created timestamps))
			  (uid (assoc-default 'id data))
			  ;; flags - for inn
			  (flags (assoc-default 'flags data))
			  (rest (assoc-default 'rest flags))
			  ;; pref
			  (pref (assoc-default 'preferences data))
			  (day (assoc-default 'dayStart pref)))
		     (habitrpg-with-section 'stats 'stats
		       (habitrpg-set-section-info `(("gp" . ,(floor gp))))
		       (habitrpg-insert-status-line
			(propertize user 'face 'habitrpg-user)
			(concat (if (eq rest t)
				    (propertize "Resting" 'face 'font-lock-warning-face)
				  "")
				(propertize
				 (format " New day starts at %s:00"
					 (if (stringp day)
					     (if (eq (string-width day) 1)
						 (concat "0" day)
					       day)
					   (number-to-string day)))
				 'face 'habitrpg-day)))

		       (habitrpg-insert-status-line (concat "Experience: "
							    (propertize
							     (number-to-string (floor exp))
							     'face 'habitrpg-exp))
						    (propertize (number-to-string nextlvl) 'face 'habitrpg-nextlvl))
		       (habitrpg-insert-status-line (concat "Gold: "
							    (propertize (number-to-string (floor gp))
									'face 'habitrpg-gold)) "")
		       (habitrpg-insert-status-line (concat "Health: "
							    (propertize (number-to-string (floor hp))
									'face 'habitrpg-hp))
						    (propertize (number-to-string maxhp) 'face 'habitrpg-maxhp))
		       (habitrpg-insert-status-line (concat "Level: "
							    (propertize
							     (number-to-string (floor lvl))
							     'face 'habitrpg-lvl)) "\n")
		       (let ((habitrpg-section-hidden-default t))
			 (habitrpg-with-section uid 'auth
			   (insert (propertize "[UID]\n" 'face 'font-lock-comment-face))
			   (insert (propertize (concat uid "\n") 'face 'font-lock-keyword-face)))))))))
      (insert "\n")
      (habitrpg-insert-tasks)
      (habitrpg-insert-habits)
      (habitrpg-insert-dailys)
      (habitrpg-insert-rewards)
      (habitrpg-insert-inventory t)
      (habitrpg-insert-eggs)
      (habitrpg-insert-potions)
      (habitrpg-insert-pets)
      (habitrpg-insert-store t)
      (kill-buffer "*request*"))))

(defun habitrpg-mode ()
  "Review the status of your habitrpg characters.

\\{habitrpg-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'habitrpg-mode
        mode-name "Habitrpg"
        mode-line-process "")
  (use-local-map habitrpg-mode-map)
  (run-mode-hooks 'habitrpg-mode-hook))

(define-derived-mode habitrpg-status-mode habitrpg-mode "Habitrpg"
  "Mode for looking at status.

\\{habitrpg-status-mode-map}"
  :group 'habitrpg)

(defun habitrpg-json-output (new-request-p args)
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (and (get-buffer "*request*") (not new-request-p))
	(apply #'request
	       args))
      (insert-buffer-substring (get-buffer "*request*")))))

(defun habitrpg-string (&rest args)
  (habitrpg-trim-line (habitrpg-output args)))

(defun habitrpg-output (new-request-p args)
  (habitrpg-json-output new-request-p (append args) ))

(defun habitrpg-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
        (substring str 0 (- (length str) 1))
      str)))

(defun habitrpg-json-insert (cmd new-request-p args)
  (insert (habitrpg-json-output new-request-p args)))

(defun habitrpg-for-all-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (derived-mode-p 'habitrpg-mode)
               (or (null dir)
                   (equal default-directory dir)))
          (funcall func)))))

(defun habitrpg-buffer-switch (buf)
  (if (string-match "habitrpg" (buffer-name))
      (switch-to-buffer buf)
    (pop-to-buffer buf)))

;;; Sections

;; A buffer in habitrpg-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Habitrpg works with.
;; The 'type' of a section identifies what kind of object it
;; represents (if any), and the parent and grand-parent, etc provide
;; the context.

(cl-defstruct habitrpg-section
  parent title beginning end children hidden type info
  needs-refresh-on-show)

(defvar habitrpg-top-section nil
  "The top section of the current buffer.")
(make-variable-buffer-local 'habitrpg-top-section)
(put 'habitrpg-top-section 'permanent-local t)

(defvar habitrpg-old-top-section nil)

(defvar habitrpg-section-hidden-default nil)

(defun habitrpg-propertize-section (section)
  "Add text-property needed for SECTION."
  (put-text-property (habitrpg-section-beginning section)
                     (habitrpg-section-end section)
                     'habitrpg-section section)
  (dolist (s (habitrpg-section-children section))
    (habitrpg-propertize-section s)))

(defun habitrpg-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (habitrpg-section-hidden section) hidden)
  (if (and (not hidden)
           (habitrpg-section-needs-refresh-on-show section))
      (habitrpg-refresh)
    (let ((inhibit-read-only t)
          (beg (save-excursion
                 (goto-char (habitrpg-section-beginning section))
                 (forward-line)
                 (point)))
          (end (habitrpg-section-end section)))
      (if (< beg end)
          (put-text-property beg end 'invisible hidden)))
    (if (not hidden)
        (dolist (c (habitrpg-section-children section))
          (habitrpg-section-set-hidden c (habitrpg-section-hidden c))))))

(defun habitrpg-set-section-info (info &optional section)
  (setf (habitrpg-section-info (or section habitrpg-top-section)) info))

(defmacro habitrpg-with-section (title type &rest body)
  "Create a new section of title TITLE and type TYPE and evaluate BODY there.

Sections created inside BODY will become children of the new
section. BODY must leave point at the end of the created section.

If TYPE is nil, the section won't be highlighted."
  (declare (indent 2))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (habitrpg-new-section ,title ,type))
            (habitrpg-top-section ,s))
       (setf (habitrpg-section-beginning ,s) (point))
       ,@body
       (setf (habitrpg-section-end ,s) (point))
       (setf (habitrpg-section-children ,s)
             (nreverse (habitrpg-section-children ,s)))
       ,s)))

(defun habitrpg-set-section-needs-refresh-on-show (flag &optional section)
  (setf (habitrpg-section-needs-refresh-on-show
         (or section habitrpg-top-section))
        flag))

(defun habitrpg-new-section (title type)
  "Create a new section with title TITLE and type TYPE in current buffer.

If `habitrpg-top-section' buffer local value is nil, the new section
will be the new top-section; otherwise the new-section will be a
child of the current top-section.

If TYPE is nil, the section won't be highlighted."
  (let* ((s (make-habitrpg-section :parent habitrpg-top-section
				   :title title
				   :type type
				   :hidden habitrpg-section-hidden-default))
         (old (and habitrpg-old-top-section
                   (habitrpg-find-section (habitrpg-section-path s)
					  habitrpg-old-top-section))))
    (if habitrpg-top-section
        (push s (habitrpg-section-children habitrpg-top-section))
      (setq habitrpg-top-section s))
    (if old
        (setf (habitrpg-section-hidden s) (habitrpg-section-hidden old)))
    s))

(defun habitrpg-cancel-section (section)
  "Delete the section SECTION."
  (delete-region (habitrpg-section-beginning section)
                 (habitrpg-section-end section))
  (let ((parent (habitrpg-section-parent section)))
    (if parent
        (setf (habitrpg-section-children parent)
              (delq section (habitrpg-section-children parent)))
      (setq habitrpg-top-section nil))))

(defun habitrpg-insert-section (section-title-and-type
				buffer-title washer cmd new-request-p &rest args)
  "Run CMD and put its result in a new section.

SECTION-TITLE-AND-TYPE is either a string that is the title of the section
or (TITLE . TYPE) where TITLE is the title of the section and TYPE is its type.

If there is no type, or if type is nil, the section won't be highlighted.

BUFFER-TITLE is the inserted title of the section

WASHER is a function that will be run after CMD.
The buffer will be narrowed to the inserted text.
It should add sectioning as needed for Habitrpg interaction.

CMD is an external command that will be run with ARGS as arguments."
  (let* ((body-beg nil)
         (section-title (if (consp section-title-and-type)
                            (car section-title-and-type)
                          section-title-and-type))
         (section-type (if (consp section-title-and-type)
                           (cdr section-title-and-type)
                         nil))

         (section
          (habitrpg-with-section section-title section-type
            (if buffer-title
                (insert (propertize buffer-title 'face 'habitrpg-section-title)
                        "\n"))
	    (setq body-beg (point))
	    (habitrpg-json-insert cmd new-request-p args)
	    (if (not (eq (char-before) ?\n))
		(insert "\n"))
	    (if washer
		(save-restriction
		  (narrow-to-region body-beg (point))
		  (goto-char (point-min))
		  (funcall washer)
		  (goto-char (point-max)))))))
    (if (= body-beg (point))
	(habitrpg-cancel-section section)
      (insert "\n"))
    section))

(defun habitrpg-section (section-title-and-type
			 buffer-title washer new-request-p &rest args)
  "Run habit and put its result in a new section.
See `habitrpg-insert-section' for meaning of the arguments"
  (apply #'habitrpg-insert-section
         section-title-and-type
         buffer-title
         washer
	 habitrpg-api-url
	 new-request-p
         (append args)))

(defmacro habitrpg-create-buffer-sections (&rest body)
  "Empty current buffer of text and habitrpg's sections, and then evaluate BODY."
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     (erase-buffer)
     (let ((habitrpg-old-top-section habitrpg-top-section))
       (setq habitrpg-top-section nil)
       ,@body
       (when (null habitrpg-top-section)
         (habitrpg-with-section 'top nil
           (insert "(empty)\n")))
       (habitrpg-propertize-section habitrpg-top-section)
       (habitrpg-section-set-hidden habitrpg-top-section
				    (habitrpg-section-hidden habitrpg-top-section)))))

(defun habitrpg-find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (habitrpg-section-children top)))
      (while (and secs (not (equal (car path)
                                   (habitrpg-section-title (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (habitrpg-find-section (cdr path) (car secs))))))

(defun habitrpg-section-path (section)
  "Return the path of SECTION."
  (if (not (habitrpg-section-parent section))
      '()
    (append (habitrpg-section-path (habitrpg-section-parent section))
            (list (habitrpg-section-title section)))))

(defun habitrpg-find-section-after (pos)
  "Find the first section that begins after POS."
  (habitrpg-find-section-after* pos (list habitrpg-top-section)))

(defun habitrpg-find-section-after* (pos secs)
  "Find the first section that begins after POS in the list SECS
\(including children of sections in SECS)."
  (while (and secs
              (<= (habitrpg-section-beginning (car secs)) pos))
    (setq secs (if (habitrpg-section-hidden (car secs))
                   (cdr secs)
                 (append (habitrpg-section-children (car secs))
                         (cdr secs)))))
  (car secs))

(defun habitrpg-find-section-before (pos) "Return the last section that begins before POS."
       (let ((section (habitrpg-find-section-at pos)))
	 (cl-do* ((current (or (habitrpg-section-parent section)
			       section) next)
		  (next
		   (if (not (habitrpg-section-hidden current))
		       (habitrpg-find-section-before* pos (habitrpg-section-children current)))
		   (if (not (habitrpg-section-hidden current))
		       (habitrpg-find-section-before* pos (habitrpg-section-children current)))))
	     ((null next) current))))

(defun habitrpg-find-section-before* (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
                (< (habitrpg-section-beginning (car secs)) pos))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun habitrpg-find-section-at (pos)
  "Return the Habitrpg section at POS."
  (or (get-text-property pos 'habitrpg-section)
      habitrpg-top-section))

(defun habitrpg-goto-next-section ()
  "Go to the next section."
  (interactive)
  (let ((next (habitrpg-find-section-after (point))))
    (if next
        (habitrpg-goto-section next)
      (message "No next section"))))

(defun habitrpg-goto-previous-section ()
  "Go to the previous section."
  (interactive)
  (if (eq (point) 1)
      (message "No previous section")
    (let ((p (point)))
      (habitrpg-goto-section (habitrpg-find-section-before (point)))
      (forward-char)
      (when (eq p (point))
	(beginning-of-line)
	(habitrpg-goto-section (habitrpg-find-section-before (point)))
	(forward-char)))))

(defun habitrpg-goto-parent-section ()
  "Go to the parent section."
  (interactive)
  (let ((parent (habitrpg-section-parent (habitrpg-current-section))))
    (when parent
      (goto-char (habitrpg-section-beginning parent)))))

(defun habitrpg-goto-next-sibling-section ()
  "Go to the next sibling section."
  (interactive)
  (let* ((initial (point))
         (section (habitrpg-current-section))
         (end (- (habitrpg-section-end section) 1))
         (parent (habitrpg-section-parent section))
         (siblings (and parent (habitrpg-section-children parent)))
         (next-sibling (habitrpg-find-section-after* end siblings)))
    (if next-sibling
        (habitrpg-goto-section next-sibling)
      (habitrpg-goto-next-section))))

(defun habitrpg-goto-previous-sibling-section ()
  "Go to the previous sibling section."
  (interactive)
  (let* ((section (habitrpg-current-section))
         (beginning (habitrpg-section-beginning section))
         (parent (habitrpg-section-parent section))
         (siblings (and parent (habitrpg-section-children parent)))
         (previous-sibling (habitrpg-find-section-before* beginning siblings)))
    (if previous-sibling
        (habitrpg-goto-section previous-sibling)
      (habitrpg-goto-parent-section))))

(defun habitrpg-goto-section (section)
  (goto-char (habitrpg-section-beginning section)))

(defun habitrpg-goto-section-at-path (path)
  "Go to the section described by PATH."
  (let ((sec (habitrpg-find-section path habitrpg-top-section)))
    (if sec
        (goto-char (habitrpg-section-beginning sec))
      (message "No such section"))))

(defun habitrpg-for-all-sections (func &optional top)
  "Run FUNC on TOP and recursively on all its children.
Default value for TOP is `habitrpg-top-section'"
  (let ((section (or top habitrpg-top-section)))
    (when section
      (funcall func section)
      (dolist (c (habitrpg-section-children section))
        (habitrpg-for-all-sections func c)))))

(defun habitrpg-section-any-hidden (section)
  "Return true if SECTION or any of its children is hidden."
  (or (habitrpg-section-hidden section)
      (let ((kids (habitrpg-section-children section)))
        (while (and kids (not (habitrpg-section-any-hidden (car kids))))
          (setq kids (cdr kids)))
        kids)))

(defun habitrpg-section-collapse (section)
  "Show SECTION and hide all its children."
  (dolist (c (habitrpg-section-children section))
    (setf (habitrpg-section-hidden c) t))
  (habitrpg-section-set-hidden section nil))

(defun habitrpg-section-expand (section)
  "Show SECTION and all its children."
  (dolist (c (habitrpg-section-children section))
    (setf (habitrpg-section-hidden c) nil))
  (habitrpg-section-set-hidden section nil))

(defun habitrpg-section-expand-all-aux (section)
  "Show recursively all SECTION's children."
  (dolist (c (habitrpg-section-children section))
    (setf (habitrpg-section-hidden c) nil)
    (habitrpg-section-expand-all-aux c)))

(defun habitrpg-section-expand-all (section)
  "Show SECTION and all its children."
  (habitrpg-section-expand-all-aux section)
  (habitrpg-section-set-hidden section nil))

(defun habitrpg-section-hideshow (flag-or-func)
  "Show or hide current section depending on FLAG-OR-FUNC.

If FLAG-OR-FUNC is a function, it will be ran on current section.
IF FLAG-OR-FUNC is a boolean, the section will be hidden if it is
true, shown otherwise."
  (let ((section (habitrpg-current-section)))
    (when (habitrpg-section-parent section)
      (goto-char (habitrpg-section-beginning section))
      (if (functionp flag-or-func)
          (funcall flag-or-func section)
        (habitrpg-section-set-hidden section flag-or-func)))))

(defun habitrpg-show-section ()
  "Show current section."
  (interactive)
  (habitrpg-section-hideshow nil))

(defun habitrpg-hide-section ()
  "Hide current section."
  (interactive)
  (habitrpg-section-hideshow t))

(defun habitrpg-collapse-section ()
  "Hide all subsection of current section."
  (interactive)
  (habitrpg-section-hideshow #'habitrpg-section-collapse))

(defun habitrpg-expand-section ()
  "Show all subsection of current section."
  (interactive)
  (habitrpg-section-hideshow #'habitrpg-section-expand))

(defun habitrpg-toggle-file-section () 
  "Like `habitrpg-toggle-section' but toggle at file granularity." 
  (interactive) 
  (when (eq 'hunk (car (habitrpg-section-context-type (habitrpg-current-section)))) 
    (habitrpg-goto-parent-section)) 
  (habitrpg-toggle-section))

(defun habitrpg-toggle-section ()
  "Toggle hidden status of current section."
  (interactive)
  (habitrpg-section-hideshow
   (lambda (s)
     (habitrpg-section-set-hidden s (not (habitrpg-section-hidden s))))))

(defun habitrpg-expand-collapse-section ()
  "Toggle hidden status of subsections of current section."
  (interactive)
  (habitrpg-section-hideshow
   (lambda (s)
     (cond ((habitrpg-section-any-hidden s)
            (habitrpg-section-expand-all s))
           (t
            (habitrpg-section-collapse s))))))

(defun habitrpg-cycle-section ()
  "Cycle between expanded, hidden and collapsed state for current section.

Hidden: only the first line of the section is shown
Collapsed: only the first line of the subsection is shown
Expanded: everything is shown."
  (interactive)
  (habitrpg-section-hideshow
   (lambda (s)
     (cond ((habitrpg-section-hidden s)
            (habitrpg-section-collapse s))
           ((with-no-warnings
              (cl-notany #'habitrpg-section-hidden (habitrpg-section-children s)))
            (habitrpg-section-set-hidden s t))
           (t
            (habitrpg-section-expand s))))))

(defun habitrpg-section-lineage (section)
  "Return list of parent, grand-parents... for SECTION."
  (when section
    (cons section (habitrpg-section-lineage (habitrpg-section-parent section)))))

(defun habitrpg-section-show-level (section level threshold path)
  (habitrpg-section-set-hidden section (>= level threshold))
  (when (< level threshold)
    (if path
        (habitrpg-section-show-level (car path) (1+ level) threshold (cdr path))
      (dolist (c (habitrpg-section-children section))
        (habitrpg-section-show-level c (1+ level) threshold nil)))))

(defun habitrpg-show-level (level all)
  "Show section whose level is less than LEVEL, hide the others.
If ALL is non nil, do this in all sections, otherwise do it only
pon ancestors and descendants of current section."
  (habitrpg-with-refresh
    (if all
        (habitrpg-section-show-level habitrpg-top-section 0 level nil)
      (let ((path (reverse (habitrpg-section-lineage (habitrpg-current-section)))))
        (habitrpg-section-show-level (car path) 0 level (cdr path))))))

(defun habitrpg-current-section ()
  "Return the Habitrpg section at point."
  (habitrpg-find-section-at (point)))

(defvar habitrpg-highlighted-section t)
(defvar habitrpg-highlight-overlay nil)

(defun habitrpg-highlight-section ()
  "Highlight current section if it has a type."
  (let ((section (habitrpg-current-section)))
    (when (not (eq section habitrpg-highlighted-section))
      (setq habitrpg-highlighted-section section)
      (if (not habitrpg-highlight-overlay)
          (let ((ov (make-overlay 1 1)))
            (setq habitrpg-highlight-overlay ov)))
      (if (and section (habitrpg-section-type section))
          (progn
            (move-overlay habitrpg-highlight-overlay
                          (habitrpg-section-beginning section)
                          (habitrpg-section-end section)
                          (current-buffer)))
        (delete-overlay habitrpg-highlight-overlay)))))

(defun habitrpg-refresh-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((old-line (line-number-at-pos))
           (old-point (point))
           (old-section (habitrpg-current-section))
           (old-path (and old-section
                          (habitrpg-section-path (habitrpg-current-section)))))
      (beginning-of-line)
      (let ((section-line (and old-section
                               (count-lines
                                (habitrpg-section-beginning old-section)
                                (point))))
            (line-char (- old-point (point))))
        (if habitrpg-refresh-function
            (apply habitrpg-refresh-function
                   habitrpg-refresh-args))
        (let ((s (and old-path (habitrpg-find-section old-path habitrpg-top-section))))
          (cond (s
                 (goto-char (habitrpg-section-beginning s))
                 (forward-line section-line)
                 (forward-char line-char))
                (t
                 (habitrpg-goto-line old-line)))
          (dolist (w (get-buffer-window-list (current-buffer)))
            (set-window-point w (point)))
          (habitrpg-highlight-section))))))

(defun habitrpg-section-context-type (section)
  (when section
    (let ((c (or (habitrpg-section-type section)
                 (and (symbolp (habitrpg-section-title section))
                      (habitrpg-section-title section)))))
      (when c
        (cons c (habitrpg-section-context-type
                 (habitrpg-section-parent section)))))))

(defun habitrpg-string-has-prefix-p (string prefix)
  (eq (compare-strings string nil (length prefix) prefix nil nil) t))

(defun habitrpg-revert-buffers (dir &optional ignore-modtime)
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (not (buffer-modified-p buffer))
               ;; don't revert indirect buffers, as the parent will be reverted
               (not (buffer-base-buffer buffer))
               (habitrpg-string-has-prefix-p (buffer-file-name buffer) dir)
               (file-readable-p (buffer-file-name buffer))
               (or ignore-modtime (not (verify-visited-file-modtime buffer))))
      (with-current-buffer buffer
        (condition-case err
            (revert-buffer t t nil))))))

(defvar habitrpg-refresh-needing-buffers nil)
(defvar habitrpg-refresh-pending nil)

(defun habitrpg-refresh-wrapper (func)
  (if habitrpg-refresh-pending
      (funcall func)
    (let ((habitrpg-refresh-pending t)
          (habitrpg-refresh-needing-buffers nil)
          (status-buffer (habitrpg-find-status-buffer default-directory)))
      (unwind-protect
          (funcall func)
        (when habitrpg-refresh-needing-buffers
          (mapc 'habitrpg-refresh-buffer habitrpg-refresh-needing-buffers))
        (when (and status-buffer
                   (not (memq status-buffer habitrpg-refresh-needing-buffers)))
          (habitrpg-refresh-buffer status-buffer))
        (habitrpg-revert-buffers default-directory)))))

(defun habitrpg-need-refresh (&optional buffer)
  "Mark BUFFER as needing to be refreshed.
If optional BUFFER is nil, use the current buffer.  If the
buffer's mode doesn't derive from `habitrpg-mode' do nothing."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'habitrpg-mode)
      (cl-pushnew (current-buffer)
                  habitrpg-refresh-needing-buffers :test 'eq))))

(defun habitrpg-refresh ()
  "Refresh current buffer."
  (interactive)
  (habitrpg-with-refresh
    (habitrpg-need-refresh)))

(defun habitrpg-refresh-all ()
  "Refresh all habitrpg buffers.
"
  (interactive)
  (habitrpg-for-all-buffers #'habitrpg-refresh-buffer default-directory))

;;; Macros

(defmacro habitrpg-with-refresh (&rest body)
  (declare (indent 0))
  `(habitrpg-refresh-wrapper (lambda () ,@body)))

(defmacro habitrpg-define-level-shower-1 (level all)
  "Define an interactive function to show function of level LEVEL.

If ALL is non nil, this function will affect all section,
otherwise it will affect only ancestors and descendants of
current section."
  (let ((fun (intern (format "habitrpg-show-level-%s%s"
                             level (if all "-all" ""))))
        (doc (format "Show sections on level %s." level)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (habitrpg-show-level ,level ,all))))

(defmacro habitrpg-define-level-shower (level)
  "Define two interactive function to show function of level LEVEL.
One for all, one for current lineage."
  `(progn
     (habitrpg-define-level-shower-1 ,level nil)
     (habitrpg-define-level-shower-1 ,level t)))

(defmacro habitrpg-define-section-jumper (sym title)
  "Define an interactive function to go to section SYM.
TITLE is the displayed title of the section."
  (let ((fun (intern (format "habitrpg-jump-to-%s" sym)))
        (doc (format "Jump to section `%s'." title)))
    `(progn
       (defun ,fun ()
         ,doc
         (interactive)
         (habitrpg-goto-section-at-path '(,sym)))
       (put ',fun 'definition-name ',sym))))

(defmacro habitrpg-define-inserter (sym arglist &rest body)
  (declare (indent defun))
  (let ((fun (intern (format "habitrpg-insert-%s" sym)))
        (before (intern (format "habitrpg-before-insert-%s-hook" sym)))
        (after (intern (format "habitrpg-after-insert-%s-hook" sym)))
        (doc (format "Insert items for `%s'." sym)))
    `(progn
       (defvar ,before nil)
       (defvar ,after nil)
       (defun ,fun ,arglist
         ,doc
         (run-hooks ',before)
         ,@body
         (run-hooks ',after))
       (put ',before 'definition-name ',sym)
       (put ',after 'definition-name ',sym)
       (put ',fun 'definition-name ',sym))))

(habitrpg-define-inserter tasks ()
  (habitrpg-section 'todo
 		    "Todos:" 'habitrpg-wash-tasks nil
		    (concat habitrpg-api-url habitrpg-api-usertask-path)
		    :type "GET"
		    :parser 'json-read
		    :headers `(("Accept" . "application/json")
			       ("X-API-User" . ,habitrpg-api-user)
			       ("X-API-Key" . ,habitrpg-api-token))
		    :sync t
		    :success (function*
			      (lambda (&key data &allow-other-keys)
				(with-current-buffer (get-buffer-create "*request*")
				  (let* ((data (assoc-default 'data data))
					 (tasks (append data nil))
					 (names (dolist (task-id tasks)
						  (let* ((completed (assoc-default 'completed task-id))
							 (type (assoc-default 'type task-id))
							 (text (assoc-default 'text task-id))
							 (id (assoc-default 'id task-id))
							 (value (assoc-default 'value task-id))
							 (notes (if (string= "" (assoc-default 'notes task-id))
								    "0"
								  (assoc-default 'notes task-id))))
						
						    (if (and (string= completed "t")
							     (not (string= type "todo")))
							(progn
							  (insert (concat "type: "
									  type " " "COMPLETED "
									  text " "
									  "id: "
									  id " "
									  "notes: " notes " "))
							  (if value
							      (insert "value: "
								      (if (numberp value)
									  (number-to-string value)
									value)
								      "\n")
							    (insert "value: 0\n")))
						      (unless (string= completed "t")
							(insert (concat "type: "
									type " "
									text " "
									"id: "
									id " "
									"notes: " notes " "))
							(if value
							    (insert "value: "
								    (if (numberp value)
									(number-to-string value)
								      value)
								    "\n")
							  (insert "value: 0\n"))))))))))))))

(habitrpg-define-inserter inventory (new-request-p)
  (habitrpg-section 'inventory
 		    "Inventory:" 'habitrpg-wash-tasks new-request-p
		    (concat habitrpg-api-url habitrpg-api-user-path)
		    :type "GET"
		    :parser 'json-read
		    :headers `(("Accept" . "application/json")
			       ("X-API-User" . ,habitrpg-api-user)
			       ("X-API-Key" . ,habitrpg-api-token))
		    :sync t
		    :success (function*
			      (lambda (&key data &allow-other-keys)
				(with-current-buffer (get-buffer-create "*request*")
				  (goto-char (point-max))
				  (let* ((data (assoc-default 'data data))
					 (items (assoc-default 'items data))
					 (eggs (assoc-default 'eggs items))
					 (potions (assoc-default 'hatchingPotions items))
					 (pets (assoc-default 'pets items))
					 (eggnames (dotimes (i (length eggs))
						     (let ((egg (nth i eggs)))
						       (insert (concat "type: egg " (symbol-name (car egg)) " Egg"
								       " id: 0" " notes: 0" " value: 0" "\n")))))
					 (potnames (dotimes (i (length potions))
						     (let ((pot (nth i potions)))
						       (insert (concat "type: potion " (symbol-name (car pot))
								       " id: 0" " notes: 0" " value: 0" "\n")))))
					 (petnames (dotimes (i (length pets))
						     (let ((pet (nth i pets)))
						       (insert (concat "type: pet " (symbol-name (car pet))
								       " id: 0" " notes: 0" " value: 0" "\n"))))))))))))

(habitrpg-define-inserter store (new-request-p)
  (habitrpg-section 'store
  		    "Store:" 'habitrpg-wash-tasks new-request-p
  		    (concat habitrpg-api-url habitrpg-api-inventory-path "/buy")
  		    :type "GET"
  		    :parser 'json-read
  		    :headers `(("Accept" . "application/json")
  			       ("X-API-User" . ,habitrpg-api-user)
  			       ("X-API-Key" . ,habitrpg-api-token))
  		    :sync t
  		    :success (function*
  			      (lambda (&key data &allow-other-keys)
  				(with-current-buffer (get-buffer-create "*request*")
				  (goto-char (point-max))
  				  (let* ((data (assoc-default 'data data))
					 (names (seq-doseq (item data)
						  (let* ((name (assoc-default 'key item))
							 (value (number-to-string (assoc-default 'value item))))
						    (insert (concat "type: store "  name " id: " name " notes: 0" " value: " value "\n")))))))))))) 





(habitrpg-define-inserter habits ()
  (habitrpg-section 'habit
 		    "Habits:" 'habitrpg-wash-tasks nil))
(habitrpg-define-inserter dailys ()
  (habitrpg-section 'daily
 		    "Dailys:" 'habitrpg-wash-tasks nil))
(habitrpg-define-inserter rewards ()
  (habitrpg-section 'reward
 		    "Rewards:" 'habitrpg-wash-tasks nil))
(habitrpg-define-inserter eggs ()
  (habitrpg-section 'egg
 		    "Eggs:" 'habitrpg-wash-tasks nil))
(habitrpg-define-inserter potions ()
  (habitrpg-section 'potion
 		    "Potions:" 'habitrpg-wash-tasks nil))
(habitrpg-define-inserter pets ()
  (habitrpg-section 'pet
 		    "Stable:" 'habitrpg-wash-tasks nil))

(defvar habitrpg-indentation-level 1)

(defun habitrpg-wash-tasks ()
  (habitrpg-wash-sequence #'habitrpg-wash-task))

(defun habitrpg-wash-task ()
  (delete-blank-lines)
  (if (looking-at "type: \\([a-z]*\\) \\(.*\\) id: \\(.*\\) notes: \\([[:ascii:][:nonascii:]]+?\\) value: \\(.*\\)")
      (let* ((type (match-string-no-properties 1))
	     (task-name (match-string-no-properties 2))
	     (task-id (match-string-no-properties 3))
	     (notes (match-string-no-properties 4))
	     (value (match-string-no-properties 5))
	     (parent section-title))
	(if (string= type parent)
	    (let ((habitrpg-section-hidden-default t))
	      (habitrpg-with-section task-name 'tasks
		(delete-region (point) (match-end 0))
		(let ((p (point))	;task info
		      (color (habitrpg-task-color value))
		      (done nil))
		  (save-restriction
		    (narrow-to-region p (point))
		    (goto-char p)
		    (insert
		     (if (string-match "COMPLETED \\(.*\\)" task-name)
			 (progn
			   (setq task-name (match-string 1 task-name))
			   (setq done t) "")
		       (setq done nil) "")
		     (make-string habitrpg-indentation-level ?\t)
		     (propertize
		      task-name
		      'face `((:box t)
			      (:foreground ,(if (> 0.5 (habitrpg-x-color-luminance color))
						"white" "black"))
			      (:background ,color)
			      ,(if done
				   '(:strike-through t)
				 '(:strike-through nil)))) " "
				 (if (or (string= section-title 'reward) (string= section-title 'store))
				     (propertize value 'face 'habitrpg-gold)
				   "") "\n")
		    (unless (string= task-id "0")
		      (habitrpg-insert-info task-id notes))
		    (goto-char (point-max))))
		(habitrpg-set-section-info `((,task-name . ,task-id) ("value" . ,value)))))
	  (delete-region (point) (match-end 0)))
	t)
    (forward-line)))



(defun habitrpg-insert-info (task-id notes)
  (habitrpg-with-section nil 'notes
    (insert (propertize "[Notes]\n" 'face 'font-lock-comment-face))
    (insert (propertize (concat notes "\n") 'face 'font-lock-keyword-face))
    (goto-char (point-max)))
  (habitrpg-with-section nil 'id
    (insert (propertize "[ID]\n" 'face 'font-lock-comment-face))
    (insert (propertize (concat task-id "\n") 'face 'font-lock-keyword-face))
    (goto-char (point-max))))


(defun habitrpg-wash-sequence (func)
  "Run FUNC until end of buffer is reached.
FUNC should leave point at the end of the modified region"
  (while (and (not (eobp))
              (funcall func))))

;;
;; Colors determined by `value' defined by habitrpg.com
;;
(defun habitrpg-x-color-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\"). Taken from `rainbow'.
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
	 (r (/ (car values) 256.0))
         (g (/ (cadr values) 256.0))
	 (b (/ (caddr values) 256.0)))
    (habitrpg-color-luminance r g b)))

(defun habitrpg-color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, BLUE and GREEN. Taken from `rainbow'.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun habitrpg-task-color (value)
  (let* ((value (string-to-number value))
	 (worst "rgb(230, 184, 175)")
	 (worse "rgb(244, 204, 204)")
	 (bad "rgb(252, 229, 205)")
	 (neutral "rgb(255, 242, 204)")
	 (good "rgb(217, 234, 211)")
	 (better "rgb(208, 224, 227)")
	 (best "rgb(201, 218, 248)"))
    (cond
     ((< value -20)
      (hrgb worst))
     ((< value -10)
      (hrgb worse))
     ((< value -1)
      (hrgb bad))
     ((< value 1)
      (hrgb neutral))
     ((< value 5)
      (hrgb good))
     ((< value 10)
      (hrgb better))
     (t
      (hrgb best)))))

(defun hrgb (color)
  "Colorize a match with itself, with relative value."
  (string-match "rgb(\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*,\s*\\([0-9]\\{1,3\\}\\(?:\s*%\\)?\\)\s*)" color)
  (let ((r (* (string-to-number (match-string-no-properties 1 color)) 255.0))
        (g (* (string-to-number (match-string-no-properties 2 color)) 255.0))
        (b (* (string-to-number (match-string-no-properties 3 color)) 255.0)))
    (format "#%02X%02X%02X" r g b)))

(defun habitrpg-goto-line (line)
  "Like `goto-line' but doesn't set the mark."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

(defun habitrpg-quit-window (&optional kill-buffer)
  "Bury the buffer and delete its window.
With a prefix argument, kill the buffer instead."
  (interactive "P")
  (quit-window kill-buffer (selected-window)))



;;
;; API interface
;;

;; set up advice for adding task
(defadvice org-store-log-note (after delay-log)
  (ad-deactivate 'org-store-log-note)
  (save-excursion
    (habitrpg-add)))
(defun habitrpg-setup ()
  (save-excursion (save-window-excursion
		    (if (string= major-mode 'org-agenda-mode) (org-agenda-switch-to))
		    (lexical-let* ((in-habit (org-entry-get-with-inheritance "IN_HABITRPG")))
		      (cond
		       ((string= in-habit "unknown")
			(habitrpg-add))
		       ((string= in-habit "yes")
			(habitrpg-add))
		       ((string= in-habit "no")
			t)
		       ((not in-habit)
			(habitrpg-add))))))
  (ad-activate 'org-store-log-note))

(defun habitrpg-do-backlog ()
  (interactive)
  (when hrpg-to-add
    (message "HabitRPG: Getting task backlog.")
    (dolist (queued-task hrpg-to-add)
      (setq hrpg-to-add (cl-delete queued-task hrpg-to-add))
      (habitrpg-get-id queued-task
		       (lambda (id)
			 (when (string= id "nil")
			   (habitrpg-create "todo" queued-task ""))))))
  (when hrpg-to-upvote-ids
    (message "HabitRPG: Completing task backlog.")
    (dolist (task-id hrpg-to-upvote-ids)
      (setq hrpg-to-upvote-ids (cl-delete task-id hrpg-to-upvote-ids))
      (habitrpg-upvote task-id))))

(defun habitrpg-add ()
  "Add to habitrpg.
With point on an `org-mode' headline add TASK if it isn't already
there.  If its state is DONE, update."
  (interactive)
  (habitrpg-do-backlog)
  (save-excursion (save-window-excursion
		    (if (string= major-mode 'org-agenda-mode) (org-agenda-switch-to))
		    (lexical-let* ((task (nth 4 (org-heading-components)))
				   (state (nth 2 (org-heading-components)))
				   (in-habit (org-entry-get-with-inheritance "IN_HABITRPG"))
				   (last-done-string (if (org-is-habit-p (point))
							 (car (sort 
							       (org-habit-done-dates
								(org-habit-parse-todo))
							       '>)))
						     nil)
				   (last-done-day 
				    (if (and (member "hrpgdaily" (org-get-tags-at))
					     last-done-string)
					(butlast
					 (nthcdr 3
						 (decode-time 
						  (days-to-time last-done-string
								))) 4)
				      nil))
				   type)

		      (habitrpg-get-id task
				       (lambda (id)
					 (save-excursion (save-window-excursion
							   (if (string= major-mode 'org-agenda-mode) (org-agenda-switch-to))
							   (when (not (string= state "DONE"))
							     (progn
							       (cond
								((member "hrpghabit" (org-get-tags-at))
								 (setq type "habit"))
								((member "hrpgdaily" (org-get-tags-at))
								 (setq type "daily"))
								((member "hrpgreward" (org-get-tags-at))
								 (setq type "reward"))
								(t (setq type "todo")))
							       (let ((text
								      (progn
									(org-back-to-heading)
									(org-agenda-get-some-entry-text (point-marker) 20))))
								 (org-back-to-heading)
								 (if (and (string= id "nil") 
									  (not (string= state "CANCELLED")))
								     (progn
								       (habitrpg-create type task text)
								       (if (string= in-habit "unknown")
									   (org-entry-put (point) "IN_HABITRPG" "yes")))
								   (if (string= in-habit "unknown")
								       (org-entry-put (point) "IN_HABITRPG" "yes"))))))))
					 (when (and (equal last-done-day 
							   (reverse (butlast (calendar-current-date))))
						    (not (string= state "DONE")))
					   (habitrpg-upvote id)
					   (message "Task \"%s\" completed!" task))
					 (when (string= state "DONE")
					   (habitrpg-upvote id)
					   (message "Task \"%s\" completed!" task))))))))

(defun habitrpg-create (type task text &optional value)
  (setq value (or value ""))
  (request
   (concat habitrpg-api-url habitrpg-api-usertask-path "/")
   :type "POST"
   :headers `(("Accept" . "application/json")
	      ("X-API-User" . ,habitrpg-api-user)
	      ("X-API-Key" . ,habitrpg-api-token))
   :data `(("type" . ,type)
	   ("text" . ,task)
	   ("notes" . ,text)
	   ("value" . ,value))
   :parser 'json-read
   :success (function*
	     (lambda (&key data &allow-other-keys)
	       (message "Task created.")))))

(defun habitrpg-new-task (&optional type)
  (lexical-let* ((type (or type "todo"))
		 (task (read-from-minibuffer "Task Name: "))
		 (notes (read-from-minibuffer "Notes: "))
		 (value (when (string= type "reward") (read-from-minibuffer "Cost: ")))
		 (p (point)))
    (if (string= type 'reward)
	(habitrpg-create type task notes value)
      (habitrpg-create type task notes)
      (habitrpg-refresh-status)
      (goto-char p))))

(defvar hrpg-id nil "ID for a habitrpg task.")
(defvar hrpg-task nil "Habitrpg task.")

(defun habitrpg-revive ()
  (deferred:$
    (request-deferred
     (concat habitrpg-api-url habitrpg-api-user-path "/revive")
     :type "POST"
     :headers `(("Content-Type" . "application/json")
		("Content-Length" . 0)
		("X-API-User" . ,habitrpg-api-user)
		("X-API-Key" . ,habitrpg-api-token))
     :parser 'json-read
     :error  (function* (lambda (&key error-thrown &allow-other-keys&rest _)
			  (message "HabitRPG: Error in getting id for task [%s]" t))))
    (deferred:nextc it
      (lambda (response)
	(if (request-response-error-thrown response)
	    (progn
	      (message "HabitRPG: Error reviving")))))))

(defun habitrpg-get-id (task func)
  (lexical-let ((t task) (func func))
    (deferred:$
      (request-deferred
       (concat habitrpg-api-url habitrpg-api-usertask-path)
       :headers `(("Accept" . "application/json")
		  ("X-API-User" . ,habitrpg-api-user)
		  ("X-API-Key" . ,habitrpg-api-token))
       :parser 'json-read
       :error  (function* (lambda (&key error-thrown &allow-other-keys&rest _)
			    (message "HabitRPG: Error in getting id for task [%s]" t))))
      (deferred:nextc it
	(lambda (response)
	  (if (request-response-error-thrown response)
	      (progn
		(message "HabitRPG: Error in getting id for task [%s]" t)
		(setq hrpg-to-add (cl-adjoin t hrpg-to-add)))
	    (let* ((data (assoc-default 'data (request-response-data response)))
		   (tasks (append data nil))
		   (names (mapcar
			   (lambda (task-id)
			     (let* ((completed (assoc-default 'completed task-id)))
			       (when (not (stringp completed))
				 (setq completed (symbol-name completed)))
			       (when (and
				      (or
				       (string= completed "False")
				       (string= completed ":json-false")
				       (string=
					(assoc-default 'type task-id) "habit"))
				      (string= (assoc-default
						'text task-id)
					       t))
				 (list (assoc-default 'text task-id) (assoc-default 'id task-id))))) tasks))
		   ;; Completed tasks should not be upvoted, so
		   ;; we should gather a list of those tasks and
		   ;; set id to `completed' so the function will
		   ;; know. The tasks which are completed are
		   ;; those that are in `tasks' but not in `names'
		   (cnames (mapcar
			    (lambda (task-id)
			      (let* ((name (assoc-default 'text task-id)))
				(when (not (assoc-default name names))
				  (list name (car task-id))))) tasks)))
	      (if (assoc-default t cnames)
		  (progn
		    (setq id "completed")
		    (message "Task %S has already been done!" t))
		(setq id (car (assoc-default t names)))
		(message "Got id %S for task %S" id t))
	      (funcall func id))))))))


(defun habitrpg-upvote (id &optional task type text direction)
  (lexical-let ((direction direction) (task task) (type type))
    (request
     (if (string= type "store")
	 (concat habitrpg-api-url habitrpg-api-inventory-path "/buy/" id "/")
       (concat habitrpg-api-url habitrpg-api-tasks-path "/" id "/score/"
	       (unless direction "up") direction))
     :type "POST"
     :headers `(("Content-Type" . "application/json")
		("Content-Length" . 0)
		("X-API-User" . ,habitrpg-api-user)
		("X-API-Key" . ,habitrpg-api-token))
     :parser 'json-read
     :success (function* (lambda (&key data &allow-other-keys)
			   (if hrpg-status-to-file
			       (with-temp-file "~/tmp/hrpg-status"
				 (let* ((exp (assoc-default 'exp data))
					(gp (assoc-default 'gp data))
					(hp (assoc-default 'hp data))
					(lvl (assoc-default 'lvl data)))
				   (insert (concat "exp: " (number-to-string (truncate exp))
						   " gp: " (number-to-string (truncate gp))
						   " hp: " (number-to-string (truncate hp))
						   " lvl: " (number-to-string (truncate lvl)))))))
			   (cond ((or (string= type "reward") (string= type "store"))
				  (message "Purchased %s" id))
				 ((string= direction "down")
				  (message "Health lost for habit %s" task))
				 ((not (string= direction "up"))
				  (message "Experience gained!")))))
     :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
			 (message "HabitRPG: Error in completing [%s]" id)
			 (setq hrpg-to-upvote-ids (cl-adjoin id hrpg-to-upvote-ids)))))))


(defun habitrpg-get-id-at-point ()
  (let ((id (cdr (car (habitrpg-section-info (habitrpg-current-section))))))
    id))

(defun habitrpg-upvote-at-point ()
  "Upvote a task.  Add task if it doesn't exist."
  (interactive)

  (let* ((id (habitrpg-get-id-at-point))
	 (section (habitrpg-current-section))
	 (stats (habitrpg-section-info (habitrpg-find-section '(stats) habitrpg-top-section)))
	 (current-gp  (assoc-default "gp" stats))
	 (info (habitrpg-section-info section))
	 (type (habitrpg-section-title (habitrpg-section-parent section)))
	 (p (point)))
    (save-excursion
      (end-of-visible-line)
      (if (or (string= type "reward") (string= type "store")
	      (string= type "potion") (string= type "egg") (string= type "stable"))
	  (cond ((or (string= type "store") (string= type "reward"))
		 (if (> (string-to-number (assoc-default "value" info)) current-gp)  
		     (message "Not enough gold to purchase: %s"
			      (if (string= type "store")
				  id
				(car (car info))))
		   (habitrpg-upvote id nil type))))
	(progn
	  (goto-char p))
	(let ((inhibit-read-only t))
	  (let ((beg (save-excursion
		       (beginning-of-line)
		       (forward-char)
			   (point)))
		    (end (progn
			   (end-of-line)
			   (point))))
		(if (< beg end)
		    (put-text-property beg end 'face '(:inverse-video t)))))
      (goto-char p)
      (progn
	(habitrpg-upvote id)
	(message "Task updated: %s"
		 (car (car info)))
	
	(let ((inhibit-read-only t))
	  (if (or (string= type "habit") (string= type "reward"))
	      (progn
		(habitrpg-refresh-status)
		(goto-char p))
	    (let ((inhibit-read-only t))
	      (let ((beg (save-excursion
			   (beginning-of-line)
			   (forward-char)
			   (point)))
		    (end (progn
			   (end-of-line)
			   (point))))
		(if (< beg end)
		    (put-text-property beg end 'face '(:strike-through t)))))))
	(goto-char p))))
    (unless (or (string= type "reward") (string= type "store")
		(string= type "potion") (string= type "egg") (string= type "stable"))
    (save-excursion (save-window-excursion
		      (forward-char)
		      (let ((title (habitrpg-section-title (habitrpg-current-section)))
			    (foundFlag nil))
			(if (not (org-occur-in-agenda-files title))
			    (with-current-buffer "*Occur*"
			      (while (and (not (condition-case nil
						   (occur-next)
						 (error t)))
					  (not foundFlag))
				(occur-mode-goto-occurrence)
				(let* ((task (nth 4 (org-heading-components)))
				       (state (nth 2 (org-heading-components)))
				       type)
				  (if (and (string= title task) (or (string= state "TODO") (string= state "NEXT")))
				      (progn
					(setq foundFlag t)
					(if (member 'habitrpg-add org-after-todo-state-change-hook)
					    (progn
					      (remove-hook 'org-after-todo-state-change-hook 'habitrpg-add)
					      (org-todo 'done)
					      (add-hook 'org-after-todo-state-change-hook 'habitrpg-add))
					  (org-todo 'done)))))
				(switch-to-buffer "*Occur*")))
			  (error "No org-mode headline with title \"%s\"" title))))))))


(defun habitrpg-downvote-at-point ()
  "Downvote a task.  Add task if it doesn't exist."
  (interactive)
  (end-of-visible-line)
  (let ((id (habitrpg-get-id-at-point))
	(p (point)))
    (habitrpg-upvote id nil nil nil "down")
    (message "Task downvoted: %s" (car (car (habitrpg-section-info (habitrpg-current-section)))))
    (habitrpg-refresh-status)
    (goto-char p)))

(defun habitrpg-delete-at-point ()
  (save-excursion
    (end-of-visible-line)
    (let* ((id (habitrpg-get-id-at-point))
	   (section (habitrpg-current-section))
	   (info (habitrpg-section-info section))
	   (type (habitrpg-section-title (habitrpg-section-parent section))))
      (when id
	(request
	 (concat habitrpg-api-url habitrpg-api-tasks-path "/" id)
	 :type "DELETE"
	 :headers `(("Content-Type" . "application/json")
		    ("X-API-User" . ,habitrpg-api-user)
		    ("X-API-Key" . ,habitrpg-api-token))
	 :parser 'json-read
	 :complete (function*
		    (lambda (&key data &allow-other-keys)
		      (message "Task deleted!")))
	 :status-code '((208 . (lambda (&rest _) (message "Got 208.")))
			(418 . (lambda (&rest _) (message "Got 418."))))))
      (let ((inhibit-read-only t))
	(let ((beg (save-excursion
		     (goto-char (habitrpg-section-beginning section))
		     (point)))
	      (end (habitrpg-section-end section)))
	  (if (< beg end)
	      (put-text-property beg end 'invisible t))))
      (habitrpg-set-section-needs-refresh-on-show t (habitrpg-section-parent section)))))

(defun habitrpg-clock-in ()
  "Upvote a clocking task based on tags.
Continuously upvote habits associated with the currently clocking task, based on tags specified in `hrpg-tags-list'."
  (cancel-function-timers 'habitrpg-upvote)
  (when (get-buffer "*habitrpg:status*")
    (save-excursion (save-window-excursion
		      (with-current-buffer "*habitrpg:status*"
			(setq header-line-format nil)))))
  (lexical-let* ((tags (org-get-tags-at))
		 (habit (car (intersection tags hrpg-tags-list :test 'equal)))
		 (bad (unless (not hrpg-bad-tags-list)
			(mapcar
			 (lambda (tag)
			   (assoc tag hrpg-bad-tags-list))
			 tags)))
		 (badhabit (car (remove nil bad))))
    (when tags
      (cond (habit
	     (habitrpg-get-id habit
			      (lambda (id)
				(setq hrpg-timer (run-at-time nil hrpg-repeat-interval
							      'habitrpg-upvote id habit "habit" ""))
				(message "Clocked into habit \"%s\"" habit))))
	    (badhabit
	     (habitrpg-get-id (car badhabit)
			      (lambda (id)
				(when (not (string= id "nil"))
				  (setq hrpg-timer (run-at-time
						    (cdr badhabit)
						    hrpg-repeat-interval
						    'habitrpg-upvote
						    id (car badhabit)
						    "habit" "" "down"))
				  (message "Warning: Clocked into habit \"%s\""
					   (car badhabit)))))
	     (setq habitrpg-header-line-string (format "CLOCKED INTO BAD HABIT %s" (car badhabit)))
	     (when (get-buffer "*habitrpg:status*")
	       (save-excursion (save-window-excursion
				 (with-current-buffer "*habitrpg:status*"
				   (setq header-line-format habitrpg-header-line-string))))))))))


(defun habitrpg-clock-out ()
  "Stop upvoting."
  (cancel-function-timers 'habitrpg-upvote)
  (setq habitrpg-header-line-string nil)
  (when (get-buffer "*habitrpg:status*")
    (save-excursion (save-window-excursion
		      (with-current-buffer "*habitrpg:status*"
			(setq header-line-format nil))))))


(defun habitrpg-search-task-name ()
  "Try to find task in `org-mode'."
  (interactive)
  (org-occur-in-agenda-files (habitrpg-section-title (habitrpg-current-section))))

(defun habitrpg-clock-in-status ()
  "Clock in to an `org-mode' task from status buffer."
  (interactive)
  (save-excursion (save-window-excursion
		    (forward-char)
		    (let ((title (habitrpg-section-title (habitrpg-current-section))))
		      (org-occur-in-agenda-files title)
		      (with-current-buffer "*Occur*"
			(occur-next)
			(occur-mode-goto-occurrence)
			(let* ((task (nth 4 (org-heading-components)))
			       (state (nth 2 (org-heading-components)))
			       type)
			  (if (string= title task)
			      (org-clock-in)
			    (error "No org-mode headline with title \"%s\"" title))))))))

(defun habitrpg-change-server ()
  (interactive)
  (if (string= habitrpg-api-url "https://beta.habitrpg.com/api/v1")
      (setq habitrpg-api-url "https://www.habitrpg.com/api/v1")
    (setq habitrpg-api-url "https://beta.habitrpg.com/api/v1"))
  (message "HabitRPG api URL changed to %s" habitrpg-api-url))

(defun habitrpg-change-api-version ()
  (interactive)
  (if (string= habitrpg-api-url "https://www.habitrpg.com/api/v1")
      (setq habitrpg-api-url "https://habitrpg.com/api/v3")
    (setq habitrpg-api-url "https://www.habitrpg.com/api/v1"))
  (message "HabitRPG api URL changed to %s" habitrpg-api-url))

(provide 'habitrpg)
(require 'habitrpg-key-mode)
;;; habitrpg.el ends here
