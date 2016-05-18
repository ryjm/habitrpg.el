;;; habitrpg.el --- org-mode interface to habitica

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
;; Integrate org-mode and habitica.  Very much a work in progress.  Adds
;; a task to habitica.com after a TODO state change in org-mode or by
;; calling the function `habitica-add`.
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
;;     habitica-api-user "ID-HERE") (setq habitica-api-token
;;     "TOKEN-HERE")
;;
;; Add this hook if you want a DONE task to be marked as complete and
;; a todo state change to add a task to habitica.com
;;
;;      (add-hook 'org-after-todo-state-change-hook 'habitica-add
;;      'append)
;;
;; Add keybindings.
;;
;;     (global-set-key (kbd "C-c C-x h") 'habitica-add)
;;      (global-set-key (kbd "<f9> a") 'habitica-status)
;;
;; You can then bring up the habitica buffer with `<f9> a`, and do
;; `C-h m` to see the keybindings.
;;
;; ![buffer](http://i.imgur.com/M5EfSkd.png)
;; ![buffer2](http://i.imgur.com/w3XIzL9.gif)
;;
;; -------------------------------------------------------------------------------
;;
;; If you want to use the clocking feature:
;;
;;      (add-hook 'org-clock-in-hook 'habitica-clock-in) (add-hook
;;      'org-clock-out-hook 'habitica-clock-out)
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
;;useful for when habitica gets more features.
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

(defgroup habitica nil
  "Controlling habitica from Emacs."
  :prefix "habitica-"
  :group 'tools)

(defcustom habitica-api-url "https://habitica.com/api/v2"
  "API url."
  :group 'habitica)
(defcustom habitica-api-user nil
  "API user id."
  :group 'habitica)
(defcustom habitica-api-token nil
  "API token."
  :group 'habitica)

(cl-eval-when (load eval)
  (defalias 'habitica-set-variable-and-refresh 'set-default))


(defgroup habitica-faces nil
  "Customize the appearance of Habitica."
  :prefix "habitica-"
  :group 'faces
  :group 'habitica)

(defface habitica-header
  '((t :inherit header-line))
  "Face for generic header lines.
Many Habitica faces inherit from this one by default."
  :group 'habitica-faces)

(defface habitica-section-title
  '((t :inherit habitica-header))
  "Face for section titles."
  :group 'habitica-faces)

(defface habitica-tag
  '((t :inherit habitica-header))
  "Face for tags."
  :group 'habitica-faces)

(defface habitica-item-mark
  '((t :inherit secondary-selection))
  "Face for highlighting marked item."
  :group 'habitica-faces)

(defface habitica-header
  '((t :inherit header-line))
  "Face for generic header lines.

Many Habitica faces inherit from this one by default."
  :group 'habitica-faces)

(defface habitica-day
  '((((class color) (background light))
     :foreground "grey11")
    (((class color) (background dark))
     :foreground "grey80"))
  "face"
  :group 'habitica-faces)

(defface habitica-user
  '((((class color) (background light))
     :foreground "firebrick")
    (((class color) (background dark))
     :foreground "tomato"))
  "face"
  :group 'habitica-faces)

(defface habitica-nextlvl
  '((((class color) (background light))
     :box t
     :background "light green"
     :foreground "dark olive green")
    (((class color) (background dark))
     :box t
     :background "light green"
     :foreground "dark olive green"))
  "face"
  :group 'habitica-faces)

(defface habitica-exp
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for exp."
  :group 'habitica-faces)

(defface habitica-lvl
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "OliveDrab4")
    (((class color) (background dark))
     :box t
     :background "Grey11"
     :foreground "DarkSeaGreen2"))
  "Face for level."
  :group 'habitica-faces)

(defface habitica-gold
  '((((class color) (background light))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4")
    (((class color) (background dark))
     :box t
     :background "LemonChiffon1"
     :foreground "goldenrod4"))
  "Face for gold."
  :group 'habitica-faces)

(defface habitica-hp
  '((((class color) (background light))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4")
    (((class color) (background dark))
     :box t
     :background "IndianRed1"
     :foreground "IndianRed4"))
  "Face for hp."
  :group 'habitica-faces)

(defface habitica-maxhp
  '((((class color) (background light))
     :box t
     :background "Grey85"
     :foreground "LightSkyBlue4")
    (((class color) (background dark))
     :box t
     :background "Grey13"
     :foreground "LightSkyBlue1"))
  "face"
  :group 'habitica-faces)

(defvar habitica-tmp-buffer-name " *habitica-tmp*")
(defvar habitica-header-line-string nil
  "Header line that shows when you are clocked into a habit that is to be downvoted.")

(defconst hrpg-repeat-interval 120)
(defvar habitica-mode-hook nil "Hook run by `habitica-status-mode'.")

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
(defvar habitica-refresh-function nil)
(make-variable-buffer-local 'habitica-refresh-function)
(put 'habitica-refresh-function 'permanent-local t)

(defvar habitica-refresh-args nil)
(make-variable-buffer-local 'habitica-refresh-args)
(put 'habitica-refresh-args 'permanent-local t)

(defvar habitica-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "n") 'habitica-goto-next-section)
    (define-key map (kbd "p") 'habitica-goto-previous-section)
    (define-key map (kbd "^") 'habitica-goto-parent-section)
    (define-key map (kbd "M-n") 'habitica-goto-next-sibling-section)
    (define-key map (kbd "M-p") 'habitica-goto-previous-sibling-section)
    (define-key map (kbd "TAB") 'habitica-toggle-section)
    (define-key map (kbd "RET") 'habitica-search-task-name)
    (define-key map (kbd "<backtab>") 'habitica-expand-collapse-section)
    (define-key map (kbd "C-c C-c") 'habitica-upvote-at-point)
    (define-key map (kbd "C-c C-x C-i") 'habitica-clock-in-status)
    (define-key map (kbd "C-c C-d") 'habitica-downvote-at-point)
    (define-key map (kbd "t") 'habitica-key-mode-popup-manage)
    (define-key map (kbd "g") 'habitica-refresh)
    (define-key map (kbd "G") 'habitica-refresh-all)
    (define-key map (kbd "q") 'habitica-quit-window)
    map))

(defcustom habitica-status-buffer-switch-function 'pop-to-buffer
  "Function for `habitica-status' to use for switching to the status buffer.

The function is given one argument, the status buffer."
  :group 'habitica
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

(defvar habitica-status-line-align-to 9)
(defun habitica-insert-status-line (heading info-string)
  (insert heading "/"
          (make-string (max 1 (- habitica-status-line-align-to
                                 (length heading))) ?\ )
          info-string "\n"))

;;
;; Habitica Status Buffer
;;

;;;###autoload
(defun habitica-status ()
  (interactive)
  (when (and (not habitica-api-user) (not habitica-api-token))
    (setq habitica-api-user (read-from-minibuffer "API User ID: ")
	  habitica-api-token (read-from-minibuffer "API Token: ")))
  (let ((buf (or (habitica-find-status-buffer 'habitica-status-mode)
		 (generate-new-buffer
		  "*habitica:status*"))))
    (funcall habitica-status-buffer-switch-function buf)
    (habitica-mode-init 'habitica-status-mode #'habitica-refresh-status)))

(defun habitica-find-status-buffer (submode)
  (cl-find-if (lambda (buf)
		(with-current-buffer buf
		  (eq major-mode submode)))
	      (buffer-list)))

(defun habitica-mode-init (submode refresh-func &rest refresh-args)
  (setq habitica-refresh-function refresh-func
        habitica-refresh-args refresh-args)
  (funcall submode)
  (habitica-refresh-buffer))

(defun habitica-refresh-status ()
  (habitica-do-backlog)
  (setq header-line-format habitica-header-line-string)
  (habitica-create-buffer-sections
    (habitica-with-section 'status nil
      (request
       (concat habitica-api-url "/user")
       :type "GET"
       :parser 'json-read
       :headers `(("Accept" . "application/json")
		  ("X-API-User" . ,habitica-api-user)
		  ("X-API-Key" . ,habitica-api-token))
       :sync t
       :success (function*
		 (lambda (&key data &allow-other-keys)
		   (let* ((stats (assoc-default 'stats data))
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
		     (habitica-with-section 'stats 'stats
		       (habitica-set-section-info `(("gp" . ,(floor gp))))
		       (habitica-insert-status-line
			(propertize user 'face 'habitica-user)
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
				 'face 'habitica-day)))

		       (habitica-insert-status-line (concat "Experience: "
							    (propertize
							     (number-to-string (floor exp))
							     'face 'habitica-exp))
						    (propertize (number-to-string nextlvl) 'face 'habitica-nextlvl))
		       (habitica-insert-status-line (concat "Gold: "
							    (propertize (number-to-string (floor gp))
									'face 'habitica-gold)) "")
		       (habitica-insert-status-line (concat "Health: "
							    (propertize (number-to-string (floor hp))
									'face 'habitica-hp))
						    (propertize (number-to-string maxhp) 'face 'habitica-maxhp))
		       (habitica-insert-status-line (concat "Level: "
							    (propertize
							     (number-to-string (floor lvl))
							     'face 'habitica-lvl)) "\n")
		       (let ((habitica-section-hidden-default t))
			 (habitica-with-section uid 'auth
			   (insert (propertize "[UID]\n" 'face 'font-lock-comment-face))
			   (insert (propertize (concat uid "\n") 'face 'font-lock-keyword-face)))))))))
      (insert "\n")
      (habitica-insert-tasks)
      (habitica-insert-habits)
      (habitica-insert-dailys)
      (habitica-insert-rewards)
      (habitica-insert-eggs)
      (habitica-insert-potions)
      (habitica-insert-pets)
      (habitica-insert-store t)
      (kill-buffer "*request*")
      )))

(defun habitica-mode ()
  "Review the status of your habitica characters.

\\{habitica-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq buffer-read-only t
        truncate-lines t
        major-mode 'habitica-mode
        mode-name "Habitica"
        mode-line-process "")
  (use-local-map habitica-mode-map)
  (run-mode-hooks 'habitica-mode-hook))

(define-derived-mode habitica-status-mode habitica-mode "Habitica"
  "Mode for looking at status.

\\{habitica-status-mode-map}"
  :group 'habitica)

(defun habitica-json-output (new-request-p args)
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (and (get-buffer "*request*") (not new-request-p))
	(apply #'request
	       args))
      (insert-buffer-substring (get-buffer "*request*")))))

(defun habitica-string (&rest args)
  (habitica-trim-line (habitica-output args)))

(defun habitica-output (new-request-p args)
  (habitica-json-output new-request-p (append args) ))

(defun habitica-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
        (substring str 0 (- (length str) 1))
      str)))

(defun habitica-json-insert (cmd new-request-p args)
  (insert (habitica-json-output new-request-p args)))

(defun habitica-for-all-buffers (func &optional dir)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (derived-mode-p 'habitica-mode)
               (or (null dir)
                   (equal default-directory dir)))
          (funcall func)))))

(defun habitica-buffer-switch (buf)
  (if (string-match "habitica" (buffer-name))
      (switch-to-buffer buf)
    (pop-to-buffer buf)))

;;; Sections

;; A buffer in habitica-mode is organized into hierarchical sections.
;; These sections are used for navigation and for hiding parts of the
;; buffer.
;;
;; Most sections also represent the objects that Habitica works with.
;; The 'type' of a section identifies what kind of object it
;; represents (if any), and the parent and grand-parent, etc provide
;; the context.

(cl-defstruct habitica-section
  parent title beginning end children hidden type info
  needs-refresh-on-show)

(defvar habitica-top-section nil
  "The top section of the current buffer.")
(make-variable-buffer-local 'habitica-top-section)
(put 'habitica-top-section 'permanent-local t)

(defvar habitica-old-top-section nil)

(defvar habitica-section-hidden-default nil)

(defun habitica-propertize-section (section)
  "Add text-property needed for SECTION."
  (put-text-property (habitica-section-beginning section)
                     (habitica-section-end section)
                     'habitica-section section)
  (dolist (s (habitica-section-children section))
    (habitica-propertize-section s)))

(defun habitica-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise."
  (setf (habitica-section-hidden section) hidden)
  (if (and (not hidden)
           (habitica-section-needs-refresh-on-show section))
      (habitica-refresh)
    (let ((inhibit-read-only t)
          (beg (save-excursion
                 (goto-char (habitica-section-beginning section))
                 (forward-line)
                 (point)))
          (end (habitica-section-end section)))
      (if (< beg end)
          (put-text-property beg end 'invisible hidden)))
    (if (not hidden)
        (dolist (c (habitica-section-children section))
          (habitica-section-set-hidden c (habitica-section-hidden c))))))

(defun habitica-set-section-info (info &optional section)
  (setf (habitica-section-info (or section habitica-top-section)) info))

(defmacro habitica-with-section (title type &rest body)
  "Create a new section of title TITLE and type TYPE and evaluate BODY there.

Sections created inside BODY will become children of the new
section. BODY must leave point at the end of the created section.

If TYPE is nil, the section won't be highlighted."
  (declare (indent 2))
  (let ((s (make-symbol "*section*")))
    `(let* ((,s (habitica-new-section ,title ,type))
            (habitica-top-section ,s))
       (setf (habitica-section-beginning ,s) (point))
       ,@body
       (setf (habitica-section-end ,s) (point))
       (setf (habitica-section-children ,s)
             (nreverse (habitica-section-children ,s)))
       ,s)))

(defun habitica-set-section-needs-refresh-on-show (flag &optional section)
  (setf (habitica-section-needs-refresh-on-show
         (or section habitica-top-section))
        flag))

(defun habitica-new-section (title type)
  "Create a new section with title TITLE and type TYPE in current buffer.

If `habitica-top-section' buffer local value is nil, the new section
will be the new top-section; otherwise the new-section will be a
child of the current top-section.

If TYPE is nil, the section won't be highlighted."
  (let* ((s (make-habitica-section :parent habitica-top-section
				   :title title
				   :type type
				   :hidden habitica-section-hidden-default))
         (old (and habitica-old-top-section
                   (habitica-find-section (habitica-section-path s)
					  habitica-old-top-section))))
    (if habitica-top-section
        (push s (habitica-section-children habitica-top-section))
      (setq habitica-top-section s))
    (if old
        (setf (habitica-section-hidden s) (habitica-section-hidden old)))
    s))

(defun habitica-cancel-section (section)
  "Delete the section SECTION."
  (delete-region (habitica-section-beginning section)
                 (habitica-section-end section))
  (let ((parent (habitica-section-parent section)))
    (if parent
        (setf (habitica-section-children parent)
              (delq section (habitica-section-children parent)))
      (setq habitica-top-section nil))))

(defun habitica-insert-section (section-title-and-type
				buffer-title washer cmd new-request-p &rest args)
  "Run CMD and put its result in a new section.

SECTION-TITLE-AND-TYPE is either a string that is the title of the section
or (TITLE . TYPE) where TITLE is the title of the section and TYPE is its type.

If there is no type, or if type is nil, the section won't be highlighted.

BUFFER-TITLE is the inserted title of the section

WASHER is a function that will be run after CMD.
The buffer will be narrowed to the inserted text.
It should add sectioning as needed for Habitica interaction.

CMD is an external command that will be run with ARGS as arguments."
  (let* ((body-beg nil)
         (section-title (if (consp section-title-and-type)
                            (car section-title-and-type)
                          section-title-and-type))
         (section-type (if (consp section-title-and-type)
                           (cdr section-title-and-type)
                         nil))

         (section
          (habitica-with-section section-title section-type
            (if buffer-title
                (insert (propertize buffer-title 'face 'habitica-section-title)
                        "\n"))
	    (setq body-beg (point))
	    (habitica-json-insert cmd new-request-p args)
	    (if (not (eq (char-before) ?\n))
		(insert "\n"))
	    (if washer
		(save-restriction
		  (narrow-to-region body-beg (point))
		  (goto-char (point-min))
		  (funcall washer)
		  (goto-char (point-max)))))))
    (if (= body-beg (point))
	(habitica-cancel-section section)
      (insert "\n"))
    section))

(defun habitica-section (section-title-and-type
			 buffer-title washer new-request-p &rest args)
  "Run habit and put its result in a new section.
See `habitica-insert-section' for meaning of the arguments"
  (apply #'habitica-insert-section
         section-title-and-type
         buffer-title
         washer
	 habitica-api-url
	 new-request-p
         (append args)))

(defmacro habitica-create-buffer-sections (&rest body)
  "Empty current buffer of text and habitica's sections, and then evaluate BODY."
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     (erase-buffer)
     (let ((habitica-old-top-section habitica-top-section))
       (setq habitica-top-section nil)
       ,@body
       (when (null habitica-top-section)
         (habitica-with-section 'top nil
           (insert "(empty)\n")))
       (habitica-propertize-section habitica-top-section)
       (habitica-section-set-hidden habitica-top-section
				    (habitica-section-hidden habitica-top-section)))))

(defun habitica-find-section (path top)
  "Find the section at the path PATH in subsection of section TOP."
  (if (null path)
      top
    (let ((secs (habitica-section-children top)))
      (while (and secs (not (equal (car path)
                                   (habitica-section-title (car secs)))))
        (setq secs (cdr secs)))
      (and (car secs)
           (habitica-find-section (cdr path) (car secs))))))

(defun habitica-section-path (section)
  "Return the path of SECTION."
  (if (not (habitica-section-parent section))
      '()
    (append (habitica-section-path (habitica-section-parent section))
            (list (habitica-section-title section)))))

(defun habitica-find-section-after (pos)
  "Find the first section that begins after POS."
  (habitica-find-section-after* pos (list habitica-top-section)))

(defun habitica-find-section-after* (pos secs)
  "Find the first section that begins after POS in the list SECS
\(including children of sections in SECS)."
  (while (and secs
              (<= (habitica-section-beginning (car secs)) pos))
    (setq secs (if (habitica-section-hidden (car secs))
                   (cdr secs)
                 (append (habitica-section-children (car secs))
                         (cdr secs)))))
  (car secs))

(defun habitica-find-section-before (pos) "Return the last section that begins before POS."
       (let ((section (habitica-find-section-at pos)))
	 (cl-do* ((current (or (habitica-section-parent section)
			       section) next)
		  (next
		   (if (not (habitica-section-hidden current))
		       (habitica-find-section-before* pos (habitica-section-children current)))
		   (if (not (habitica-section-hidden current))
		       (habitica-find-section-before* pos (habitica-section-children current)))))
	     ((null next) current))))

(defun habitica-find-section-before* (pos secs)
  "Find the last section that begins before POS in the list SECS."
  (let ((prev nil))
    (while (and secs
                (< (habitica-section-beginning (car secs)) pos))
      (setq prev (car secs))
      (setq secs (cdr secs)))
    prev))

(defun habitica-find-section-at (pos)
  "Return the Habitica section at POS."
  (or (get-text-property pos 'habitica-section)
      habitica-top-section))

(defun habitica-goto-next-section ()
  "Go to the next section."
  (interactive)
  (let ((next (habitica-find-section-after (point))))
    (if next
        (habitica-goto-section next)
      (message "No next section"))))

(defun habitica-goto-previous-section ()
  "Go to the previous section."
  (interactive)
  (if (eq (point) 1)
      (message "No previous section")
    (let ((p (point)))
      (habitica-goto-section (habitica-find-section-before (point)))
      (forward-char)
      (when (eq p (point))
	(beginning-of-line)
	(habitica-goto-section (habitica-find-section-before (point)))
	(forward-char)))))

(defun habitica-goto-parent-section ()
  "Go to the parent section."
  (interactive)
  (let ((parent (habitica-section-parent (habitica-current-section))))
    (when parent
      (goto-char (habitica-section-beginning parent)))))

(defun habitica-goto-next-sibling-section ()
  "Go to the next sibling section."
  (interactive)
  (let* ((initial (point))
         (section (habitica-current-section))
         (end (- (habitica-section-end section) 1))
         (parent (habitica-section-parent section))
         (siblings (and parent (habitica-section-children parent)))
         (next-sibling (habitica-find-section-after* end siblings)))
    (if next-sibling
        (habitica-goto-section next-sibling)
      (habitica-goto-next-section))))

(defun habitica-goto-previous-sibling-section ()
  "Go to the previous sibling section."
  (interactive)
  (let* ((section (habitica-current-section))
         (beginning (habitica-section-beginning section))
         (parent (habitica-section-parent section))
         (siblings (and parent (habitica-section-children parent)))
         (previous-sibling (habitica-find-section-before* beginning siblings)))
    (if previous-sibling
        (habitica-goto-section previous-sibling)
      (habitica-goto-parent-section))))

(defun habitica-goto-section (section)
  (goto-char (habitica-section-beginning section)))

(defun habitica-goto-section-at-path (path)
  "Go to the section described by PATH."
  (let ((sec (habitica-find-section path habitica-top-section)))
    (if sec
        (goto-char (habitica-section-beginning sec))
      (message "No such section"))))

(defun habitica-for-all-sections (func &optional top)
  "Run FUNC on TOP and recursively on all its children.
Default value for TOP is `habitica-top-section'"
  (let ((section (or top habitica-top-section)))
    (when section
      (funcall func section)
      (dolist (c (habitica-section-children section))
        (habitica-for-all-sections func c)))))

(defun habitica-section-any-hidden (section)
  "Return true if SECTION or any of its children is hidden."
  (or (habitica-section-hidden section)
      (let ((kids (habitica-section-children section)))
        (while (and kids (not (habitica-section-any-hidden (car kids))))
          (setq kids (cdr kids)))
        kids)))

(defun habitica-section-collapse (section)
  "Show SECTION and hide all its children."
  (dolist (c (habitica-section-children section))
    (setf (habitica-section-hidden c) t))
  (habitica-section-set-hidden section nil))

(defun habitica-section-expand (section)
  "Show SECTION and all its children."
  (dolist (c (habitica-section-children section))
    (setf (habitica-section-hidden c) nil))
  (habitica-section-set-hidden section nil))

(defun habitica-section-expand-all-aux (section)
  "Show recursively all SECTION's children."
  (dolist (c (habitica-section-children section))
    (setf (habitica-section-hidden c) nil)
    (habitica-section-expand-all-aux c)))

(defun habitica-section-expand-all (section)
  "Show SECTION and all its children."
  (habitica-section-expand-all-aux section)
  (habitica-section-set-hidden section nil))

(defun habitica-section-hideshow (flag-or-func)
  "Show or hide current section depending on FLAG-OR-FUNC.

If FLAG-OR-FUNC is a function, it will be ran on current section.
IF FLAG-OR-FUNC is a boolean, the section will be hidden if it is
true, shown otherwise."
  (let ((section (habitica-current-section)))
    (when (habitica-section-parent section)
      (goto-char (habitica-section-beginning section))
      (if (functionp flag-or-func)
          (funcall flag-or-func section)
        (habitica-section-set-hidden section flag-or-func)))))

(defun habitica-show-section ()
  "Show current section."
  (interactive)
  (habitica-section-hideshow nil))

(defun habitica-hide-section ()
  "Hide current section."
  (interactive)
  (habitica-section-hideshow t))

(defun habitica-collapse-section ()
  "Hide all subsection of current section."
  (interactive)
  (habitica-section-hideshow #'habitica-section-collapse))

(defun habitica-expand-section ()
  "Show all subsection of current section."
  (interactive)
  (habitica-section-hideshow #'habitica-section-expand))

(defun habitica-toggle-file-section () 
  "Like `habitica-toggle-section' but toggle at file granularity." 
  (interactive) 
  (when (eq 'hunk (car (habitica-section-context-type (habitica-current-section)))) 
    (habitica-goto-parent-section)) 
  (habitica-toggle-section))

(defun habitica-toggle-section ()
  "Toggle hidden status of current section."
  (interactive)
  (habitica-section-hideshow
   (lambda (s)
     (habitica-section-set-hidden s (not (habitica-section-hidden s))))))

(defun habitica-expand-collapse-section ()
  "Toggle hidden status of subsections of current section."
  (interactive)
  (habitica-section-hideshow
   (lambda (s)
     (cond ((habitica-section-any-hidden s)
            (habitica-section-expand-all s))
           (t
            (habitica-section-collapse s))))))

(defun habitica-cycle-section ()
  "Cycle between expanded, hidden and collapsed state for current section.

Hidden: only the first line of the section is shown
Collapsed: only the first line of the subsection is shown
Expanded: everything is shown."
  (interactive)
  (habitica-section-hideshow
   (lambda (s)
     (cond ((habitica-section-hidden s)
            (habitica-section-collapse s))
           ((with-no-warnings
              (cl-notany #'habitica-section-hidden (habitica-section-children s)))
            (habitica-section-set-hidden s t))
           (t
            (habitica-section-expand s))))))

(defun habitica-section-lineage (section)
  "Return list of parent, grand-parents... for SECTION."
  (when section
    (cons section (habitica-section-lineage (habitica-section-parent section)))))

(defun habitica-section-show-level (section level threshold path)
  (habitica-section-set-hidden section (>= level threshold))
  (when (< level threshold)
    (if path
        (habitica-section-show-level (car path) (1+ level) threshold (cdr path))
      (dolist (c (habitica-section-children section))
        (habitica-section-show-level c (1+ level) threshold nil)))))

(defun habitica-show-level (level all)
  "Show section whose level is less than LEVEL, hide the others.
If ALL is non nil, do this in all sections, otherwise do it only
pon ancestors and descendants of current section."
  (habitica-with-refresh
    (if all
        (habitica-section-show-level habitica-top-section 0 level nil)
      (let ((path (reverse (habitica-section-lineage (habitica-current-section)))))
        (habitica-section-show-level (car path) 0 level (cdr path))))))

(defun habitica-current-section ()
  "Return the Habitica section at point."
  (habitica-find-section-at (point)))

(defvar habitica-highlighted-section t)
(defvar habitica-highlight-overlay nil)

(defun habitica-highlight-section ()
  "Highlight current section if it has a type."
  (let ((section (habitica-current-section)))
    (when (not (eq section habitica-highlighted-section))
      (setq habitica-highlighted-section section)
      (if (not habitica-highlight-overlay)
          (let ((ov (make-overlay 1 1)))
            (setq habitica-highlight-overlay ov)))
      (if (and section (habitica-section-type section))
          (progn
            (move-overlay habitica-highlight-overlay
                          (habitica-section-beginning section)
                          (habitica-section-end section)
                          (current-buffer)))
        (delete-overlay habitica-highlight-overlay)))))

(defun habitica-refresh-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((old-line (line-number-at-pos))
           (old-point (point))
           (old-section (habitica-current-section))
           (old-path (and old-section
                          (habitica-section-path (habitica-current-section)))))
      (beginning-of-line)
      (let ((section-line (and old-section
                               (count-lines
                                (habitica-section-beginning old-section)
                                (point))))
            (line-char (- old-point (point))))
        (if habitica-refresh-function
            (apply habitica-refresh-function
                   habitica-refresh-args))
        (let ((s (and old-path (habitica-find-section old-path habitica-top-section))))
          (cond (s
                 (goto-char (habitica-section-beginning s))
                 (forward-line section-line)
                 (forward-char line-char))
                (t
                 (habitica-goto-line old-line)))
          (dolist (w (get-buffer-window-list (current-buffer)))
            (set-window-point w (point)))
          (habitica-highlight-section))))))

(defun habitica-section-context-type (section)
  (when section
    (let ((c (or (habitica-section-type section)
                 (and (symbolp (habitica-section-title section))
                      (habitica-section-title section)))))
      (when c
        (cons c (habitica-section-context-type
                 (habitica-section-parent section)))))))

(defun habitica-string-has-prefix-p (string prefix)
  (eq (compare-strings string nil (length prefix) prefix nil nil) t))

(defun habitica-revert-buffers (dir &optional ignore-modtime)
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (not (buffer-modified-p buffer))
               ;; don't revert indirect buffers, as the parent will be reverted
               (not (buffer-base-buffer buffer))
               (habitica-string-has-prefix-p (buffer-file-name buffer) dir)
               (file-readable-p (buffer-file-name buffer))
               (or ignore-modtime (not (verify-visited-file-modtime buffer))))
      (with-current-buffer buffer
        (condition-case err
            (revert-buffer t t nil))))))

(defvar habitica-refresh-needing-buffers nil)
(defvar habitica-refresh-pending nil)

(defun habitica-refresh-wrapper (func)
  (if habitica-refresh-pending
      (funcall func)
    (let ((habitica-refresh-pending t)
          (habitica-refresh-needing-buffers nil)
          (status-buffer (habitica-find-status-buffer default-directory)))
      (unwind-protect
          (funcall func)
        (when habitica-refresh-needing-buffers
          (mapc 'habitica-refresh-buffer habitica-refresh-needing-buffers))
        (when (and status-buffer
                   (not (memq status-buffer habitica-refresh-needing-buffers)))
          (habitica-refresh-buffer status-buffer))
        (habitica-revert-buffers default-directory)))))

(defun habitica-need-refresh (&optional buffer)
  "Mark BUFFER as needing to be refreshed.
If optional BUFFER is nil, use the current buffer.  If the
buffer's mode doesn't derive from `habitica-mode' do nothing."
  (with-current-buffer (or buffer (current-buffer))
    (when (derived-mode-p 'habitica-mode)
      (cl-pushnew (current-buffer)
                  habitica-refresh-needing-buffers :test 'eq))))

(defun habitica-refresh ()
  "Refresh current buffer."
  (interactive)
  (habitica-with-refresh
    (habitica-need-refresh)))

(defun habitica-refresh-all ()
  "Refresh all habitica buffers.
"
  (interactive)
  (habitica-for-all-buffers #'habitica-refresh-buffer default-directory))

;;; Macros

(defmacro habitica-with-refresh (&rest body)
  (declare (indent 0))
  `(habitica-refresh-wrapper (lambda () ,@body)))

(defmacro habitica-define-level-shower-1 (level all)
  "Define an interactive function to show function of level LEVEL.

If ALL is non nil, this function will affect all section,
otherwise it will affect only ancestors and descendants of
current section."
  (let ((fun (intern (format "habitica-show-level-%s%s"
                             level (if all "-all" ""))))
        (doc (format "Show sections on level %s." level)))
    `(defun ,fun ()
       ,doc
       (interactive)
       (habitica-show-level ,level ,all))))

(defmacro habitica-define-level-shower (level)
  "Define two interactive function to show function of level LEVEL.
One for all, one for current lineage."
  `(progn
     (habitica-define-level-shower-1 ,level nil)
     (habitica-define-level-shower-1 ,level t)))

(defmacro habitica-define-section-jumper (sym title)
  "Define an interactive function to go to section SYM.
TITLE is the displayed title of the section."
  (let ((fun (intern (format "habitica-jump-to-%s" sym)))
        (doc (format "Jump to section `%s'." title)))
    `(progn
       (defun ,fun ()
         ,doc
         (interactive)
         (habitica-goto-section-at-path '(,sym)))
       (put ',fun 'definition-name ',sym))))

(defmacro habitica-define-inserter (sym arglist &rest body)
  (declare (indent defun))
  (let ((fun (intern (format "habitica-insert-%s" sym)))
        (before (intern (format "habitica-before-insert-%s-hook" sym)))
        (after (intern (format "habitica-after-insert-%s-hook" sym)))
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

(habitica-define-inserter tasks ()
  (habitica-section 'todo
 		    "Todos:" 'habitica-wash-tasks nil
		    (concat habitica-api-url "/user")
		    :type "GET"
		    :parser 'json-read
		    :headers `(("Accept" . "application/json")
			       ("X-API-User" . ,habitica-api-user)
			       ("X-API-Key" . ,habitica-api-token))
		    :sync t
		    :success (function*
			      (lambda (&key data &allow-other-keys)
				(with-current-buffer (get-buffer-create "*request*")
				  (let* ((tasks (append (assoc-default 'todos data) 
							(assoc-default 'dailys data)
							(assoc-default 'habits data)
							(assoc-default 'rewards data)
							'()))
					 (items (assoc-default 'items data))
					 (eggs (assoc-default 'eggs items))
					 (potions (assoc-default 'hatchingPotions items))
					 (pets (assoc-default 'pets items))
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
							  (insert "value: 0\n")))))))

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

(habitica-define-inserter store (new-request-p)
  (habitica-section 'store
  		    "Store:" 'habitica-wash-tasks new-request-p
  		    (concat habitica-api-url "/user/inventory/buy")
  		    :type "GET"
  		    :parser 'json-read
  		    :headers `(("Accept" . "application/json")
  			       ("X-API-User" . ,habitica-api-user)
  			       ("X-API-Key" . ,habitica-api-token))
  		    :sync t
  		    :success (function*
  			      (lambda (&key data &allow-other-keys)
  				(with-current-buffer (get-buffer-create "*request*")
				  (goto-char (point-max))
  				  (let* ( (names (seq-doseq (item data)
						   (let* ((name (assoc-default 'key item))
							  (value (number-to-string (assoc-default 'value item))))
						     (insert (concat "type: store "  name " id: " name " notes: 0" " value: " value "\n")))))))))))) 





(habitica-define-inserter habits ()
  (habitica-section 'habit
 		    "Habits:" 'habitica-wash-tasks nil))
(habitica-define-inserter dailys ()
  (habitica-section 'daily
 		    "Dailys:" 'habitica-wash-tasks nil))
(habitica-define-inserter rewards ()
  (habitica-section 'reward
 		    "Rewards:" 'habitica-wash-tasks nil))
(habitica-define-inserter eggs ()
  (habitica-section 'egg
 		    "Eggs:" 'habitica-wash-tasks nil))
(habitica-define-inserter potions ()
  (habitica-section 'potion
 		    "Potions:" 'habitica-wash-tasks nil))
(habitica-define-inserter pets ()
  (habitica-section 'pet
 		    "Stable:" 'habitica-wash-tasks nil))

(defvar habitica-indentation-level 1)

(defun habitica-wash-tasks ()
  (habitica-wash-sequence #'habitica-wash-task))

(defun habitica-wash-task ()
  (delete-blank-lines)
  (if (looking-at "type: \\([a-z]*\\) \\(.*\\) id: \\(.*\\) notes: \\([[:ascii:][:nonascii:]]+?\\) value: \\(.*\\)")
      (let* ((type (match-string-no-properties 1))
	     (task-name (match-string-no-properties 2))
	     (task-id (match-string-no-properties 3))
	     (notes (match-string-no-properties 4))
	     (value (match-string-no-properties 5))
	     (parent section-title))
	(if (string= type parent)
	    (let ((habitica-section-hidden-default t))
	      (habitica-with-section task-name 'tasks
		(delete-region (point) (match-end 0))
		(let ((p (point))	;task info
		      (color (habitica-task-color value))
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
		     (make-string habitica-indentation-level ?\t)
		     (propertize
		      task-name
		      'face `((:box t)
			      (:foreground ,(if (> 0.5 (habitica-x-color-luminance color))
						"white" "black"))
			      (:background ,color)
			      ,(if done
				   '(:strike-through t)
				 '(:strike-through nil)))) " "
				 (if (or (string= section-title 'reward) (string= section-title 'store))
				     (propertize value 'face 'habitica-gold)
				   "") "\n")
		    (unless (string= task-id "0")
		      (habitica-insert-info task-id notes))
		    (goto-char (point-max))))
		(habitica-set-section-info `((,task-name . ,task-id) ("value" . ,value)))))
	  (delete-region (point) (match-end 0)))
	t)
    (forward-line)))



(defun habitica-insert-info (task-id notes)
  (habitica-with-section nil 'notes
    (insert (propertize "[Notes]\n" 'face 'font-lock-comment-face))
    (insert (propertize (concat notes "\n") 'face 'font-lock-keyword-face))
    (goto-char (point-max)))
  (habitica-with-section nil 'id
    (insert (propertize "[ID]\n" 'face 'font-lock-comment-face))
    (insert (propertize (concat task-id "\n") 'face 'font-lock-keyword-face))
    (goto-char (point-max))))


(defun habitica-wash-sequence (func)
  "Run FUNC until end of buffer is reached.
FUNC should leave point at the end of the modified region"
  (while (and (not (eobp))
              (funcall func))))

;;
;; Colors determined by `value' defined by habitica.com
;;
(defun habitica-x-color-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\"). Taken from `rainbow'.
Return a value between 0 and 1."
  (let* ((values (x-color-values color))
	 (r (/ (car values) 256.0))
         (g (/ (cadr values) 256.0))
	 (b (/ (caddr values) 256.0)))
    (habitica-color-luminance r g b)))

(defun habitica-color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, BLUE and GREEN. Taken from `rainbow'.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(defun habitica-task-color (value)
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

(defun habitica-goto-line (line)
  "Like `goto-line' but doesn't set the mark."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))

(defun habitica-quit-window (&optional kill-buffer)
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
    (habitica-add)))
(defun habitica-setup ()
  (save-excursion (save-window-excursion
		    (if (string= major-mode 'org-agenda-mode) (org-agenda-switch-to))
		    (lexical-let* ((in-habit (org-entry-get-with-inheritance "IN_HABITICA")))
		      (cond
		       ((string= in-habit "unknown")
			(habitica-add))
		       ((string= in-habit "yes")
			(habitica-add))
		       ((string= in-habit "no")
			t)
		       ((not in-habit)
			(habitica-add))))))
  (ad-activate 'org-store-log-note))

(defun habitica-do-backlog ()
  (interactive)
  (when hrpg-to-add
    (message "Habitica: Getting task backlog.")
    (dolist (queued-task hrpg-to-add)
      (setq hrpg-to-add (cl-delete queued-task hrpg-to-add))
      (habitica-get-id queued-task
		       (lambda (id)
			 (when (string= id "nil")
			   (habitica-create "todo" queued-task ""))))))
  (when hrpg-to-upvote-ids
    (message "Habitica: Completing task backlog.")
    (dolist (task-id hrpg-to-upvote-ids)
      (setq hrpg-to-upvote-ids (cl-delete task-id hrpg-to-upvote-ids))
      (habitica-upvote task-id))))

(defun habitica-add ()
  "Add to habitica.
With point on an `org-mode' headline add TASK if it isn't already
there.  If its state is DONE, update."
  (interactive)
  (habitica-do-backlog)
  (save-excursion (save-window-excursion
		    (if (string= major-mode 'org-agenda-mode) (org-agenda-switch-to))
		    (lexical-let* ((task (nth 4 (org-heading-components)))
				   (state (nth 2 (org-heading-components)))
				   (in-habit (org-entry-get-with-inheritance "IN_HABITICA"))
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

		      (habitica-get-id task
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
								       (habitica-create type task text)
								       (if (string= in-habit "unknown")
									   (org-entry-put (point) "IN_HABITICA" "yes")))
								   (if (string= in-habit "unknown")
								       (org-entry-put (point) "IN_HABITICA" "yes"))))))))
					 (when (and (equal last-done-day 
							   (reverse (butlast (calendar-current-date))))
						    (not (string= state "DONE")))
					   (habitica-upvote id)
					   (message "Task \"%s\" completed!" task))
					 (when (string= state "DONE")
					   (habitica-upvote id)
					   (message "Task \"%s\" completed!" task))))))))

(defun habitica-create (type task text &optional value)
  (setq value (or value ""))
  (request
   (concat habitica-api-url "/user/tasks/")
   :type "POST"
   :headers `(("Accept" . "application/json")
	      ("X-API-User" . ,habitica-api-user)
	      ("X-API-Key" . ,habitica-api-token))
   :data `(("type" . ,type)
	   ("text" . ,task)
	   ("notes" . ,text)
	   ("value" . ,value))
   :parser 'json-read
   :success (function*
	     (lambda (&key data &allow-other-keys)
	       (message "Task created.")))))

(defun habitica-new-task (&optional type)
  (lexical-let* ((type (or type "todo"))
		 (task (read-from-minibuffer "Task Name: "))
		 (notes (read-from-minibuffer "Notes: "))
		 (value (when (string= type "reward") (read-from-minibuffer "Cost: ")))
		 (p (point)))
    (if (string= type 'reward)
	(habitica-create type task notes value)
      (habitica-create type task notes)
      (habitica-refresh-status)
      (goto-char p))))

(defvar hrpg-id nil "ID for a habitica task.")
(defvar hrpg-task nil "Habitica task.")

(defun habitica-revive ()
  (deferred:$
    (request-deferred
     (concat habitica-api-url "/user/revive")
     :type "POST"
     :headers `(("Content-Type" . "application/json")
		("Content-Length" . 0)
		("X-API-User" . ,habitica-api-user)
		("X-API-Key" . ,habitica-api-token))
     :parser 'json-read
     :error  (function* (lambda (&key error-thrown &allow-other-keys&rest _)
			  (message "Habitica: Error in getting id for task [%s]" t))))
    (deferred:nextc it
      (lambda (response)
	(if (request-response-error-thrown response)
	    (progn
	      (message "Habitica: Error reviving")))))))

(defun habitica-get-id (task func)
  (lexical-let ((t task) (func func))
    (deferred:$
      (request-deferred
       (concat habitica-api-url "/user")
       :headers `(("Accept" . "application/json")
		  ("X-API-User" . ,habitica-api-user)
		  ("X-API-Key" . ,habitica-api-token))
       :parser 'json-read
       :error  (function* (lambda (&key error-thrown &allow-other-keys&rest _)
			    (message "Habitica: Error in getting id for task [%s]" t))))
      (deferred:nextc it
	(lambda (response)
	  (if (request-response-error-thrown response)
	      (progn
		(message "Habitica: Error in getting id for task [%s]" t)
		(setq hrpg-to-add (cl-adjoin t hrpg-to-add)))
	    (let* ((data (request-response-data response))
		   (tasks (append (assoc-default 'todos data) 
				  (assoc-default 'dailys data)
				  (assoc-default 'habits data)
				  '()))
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


(defun habitica-upvote (id &optional task type text direction)
  (lexical-let ((direction direction) (task task) (type type))
    (request
     (if (string= type "store")
	 (concat habitica-api-url "/user/inventory/buy/" id "/")
       (concat habitica-api-url "/user/tasks/" id "/"
	       (unless direction "up") direction))
     :type "POST"
     :headers `(("Content-Type" . "application/json")
		("Content-Length" . 0)
		("X-API-User" . ,habitica-api-user)
		("X-API-Key" . ,habitica-api-token))
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
			 (message "Habitica: Error in completing [%s]" id)
			 (setq hrpg-to-upvote-ids (cl-adjoin id hrpg-to-upvote-ids)))))))


(defun habitica-get-id-at-point ()
  (let ((id (cdr (car (habitica-section-info (habitica-current-section))))))
    id))

(defun habitica-upvote-at-point ()
  "Upvote a task.  Add task if it doesn't exist."
  (interactive)

  (let* ((id (habitica-get-id-at-point))
	 (section (habitica-current-section))
	 (stats (habitica-section-info (habitica-find-section '(stats) habitica-top-section)))
	 (current-gp  (assoc-default "gp" stats))
	 (info (habitica-section-info section))
	 (type (habitica-section-title (habitica-section-parent section)))
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
		   (habitica-upvote id nil type))))
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
	(habitica-upvote id)
	(message "Task updated: %s"
		 (car (car info)))
	
	(let ((inhibit-read-only t))
	  (if (or (string= type "habit") (string= type "reward"))
	      (progn
		(habitica-refresh-status)
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
		      (let ((title (habitica-section-title (habitica-current-section)))
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
					(if (member 'habitica-add org-after-todo-state-change-hook)
					    (progn
					      (remove-hook 'org-after-todo-state-change-hook 'habitica-add)
					      (org-todo 'done)
					      (add-hook 'org-after-todo-state-change-hook 'habitica-add))
					  (org-todo 'done)))))
				(switch-to-buffer "*Occur*")))
			  (error "No org-mode headline with title \"%s\"" title))))))))


(defun habitica-downvote-at-point ()
  "Downvote a task.  Add task if it doesn't exist."
  (interactive)
  (end-of-visible-line)
  (let ((id (habitica-get-id-at-point))
	(p (point)))
    (habitica-upvote id nil nil nil "down")
    (message "Task downvoted: %s" (car (car (habitica-section-info (habitica-current-section)))))
    (habitica-refresh-status)
    (goto-char p)))

(defun habitica-delete-at-point ()
  (save-excursion
    (end-of-visible-line)
    (let* ((id (habitica-get-id-at-point))
	   (section (habitica-current-section))
	   (info (habitica-section-info section))
	   (type (habitica-section-title (habitica-section-parent section))))
      (when id
	(request
	 (concat habitica-api-url "/user/tasks/" id)
	 :type "DELETE"
	 :headers `(("Content-Type" . "application/json")
		    ("X-API-User" . ,habitica-api-user)
		    ("X-API-Key" . ,habitica-api-token))
	 :parser 'json-read
	 :complete (function*
		    (lambda (&key data &allow-other-keys)
		      (message "Task deleted!")))
	 :status-code '((208 . (lambda (&rest _) (message "Got 208.")))
			(418 . (lambda (&rest _) (message "Got 418."))))))
      (let ((inhibit-read-only t))
	(let ((beg (save-excursion
		     (goto-char (habitica-section-beginning section))
		     (point)))
	      (end (habitica-section-end section)))
	  (if (< beg end)
	      (put-text-property beg end 'invisible t))))
      (habitica-set-section-needs-refresh-on-show t (habitica-section-parent section)))))

(defun habitica-clock-in ()
  "Upvote a clocking task based on tags.
Continuously upvote habits associated with the currently clocking task, based on tags specified in `hrpg-tags-list'."
  (cancel-function-timers 'habitica-upvote)
  (when (get-buffer "*habitica:status*")
    (save-excursion (save-window-excursion
		      (with-current-buffer "*habitica:status*"
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
	     (habitica-get-id habit
			      (lambda (id)
				(setq hrpg-timer (run-at-time nil hrpg-repeat-interval
							      'habitica-upvote id habit "habit" ""))
				(message "Clocked into habit \"%s\"" habit))))
	    (badhabit
	     (habitica-get-id (car badhabit)
			      (lambda (id)
				(when (not (string= id "nil"))
				  (setq hrpg-timer (run-at-time
						    (cdr badhabit)
						    hrpg-repeat-interval
						    'habitica-upvote
						    id (car badhabit)
						    "habit" "" "down"))
				  (message "Warning: Clocked into habit \"%s\""
					   (car badhabit)))))
	     (setq habitica-header-line-string (format "CLOCKED INTO BAD HABIT %s" (car badhabit)))
	     (when (get-buffer "*habitica:status*")
	       (save-excursion (save-window-excursion
				 (with-current-buffer "*habitica:status*"
				   (setq header-line-format habitica-header-line-string))))))))))


(defun habitica-clock-out ()
  "Stop upvoting."
  (cancel-function-timers 'habitica-upvote)
  (setq habitica-header-line-string nil)
  (when (get-buffer "*habitica:status*")
    (save-excursion (save-window-excursion
		      (with-current-buffer "*habitica:status*"
			(setq header-line-format nil))))))


(defun habitica-search-task-name ()
  "Try to find task in `org-mode'."
  (interactive)
  (org-occur-in-agenda-files (habitica-section-title (habitica-current-section))))

(defun habitica-clock-in-status ()
  "Clock in to an `org-mode' task from status buffer."
  (interactive)
  (save-excursion (save-window-excursion
		    (forward-char)
		    (let ((title (habitica-section-title (habitica-current-section))))
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

(defun habitica-change-server ()
  (interactive)
  (if (string= habitica-api-url "https://beta.habitica.com/api/v1")
      (setq habitica-api-url "https://www.habitica.com/api/v1")
    (setq habitica-api-url "https://beta.habitica.com/api/v1"))
  (message "Habitica api URL changed to %s" habitica-api-url))

(defun habitica-change-api-version ()
  (interactive)
  (if (string= habitica-api-url "https://www.habitica.com/api/v1")
      (setq habitica-api-url "https://www.habitica.com/api/v2")
    (setq habitica-api-url "https://www.habitica.com/api/v1"))
  (message "Habitica api URL changed to %s" habitica-api-url))

(provide 'habitrpg)
(require 'habitrpg-key-mode)
;;; habitrpg.el ends here
