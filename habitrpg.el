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

;; Most of the code for the status buffer was taken from the Magit
;;project (I really like the way they set up the sections, it's very
;;modular so you can add different sections easily.
;;; Code:

(provide 'habitrpg)

(require 'cl)

(cl-eval-when (load eval)
  (defalias 'habitrpg-set-variable-and-refresh 'set-default))

(defcustom habitrpg-highlight-whitespace t
  "Specify where to highlight whitespace errors.
See `habitrpg-highlight-trailing-whitespace',
`habitrpg-highlight-indentation'.  The symbol t means in all diffs,
`status' means only in the status buffer, and nil means nowhere."
  :group 'habitrpg
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status))
  :set 'habitrpg-set-variable-and-refresh)

(defcustom habitrpg-highlight-trailing-whitespace t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `habitrpg-highlight-whitespace' is non-nil."
  :group 'habitrpg
  :type 'boolean
  :set 'habitrpg-set-variable-and-refresh)

(defcustom habitrpg-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `habitrpg-highlight-whitespace' is non-nil.

The value is a list of cons cells.  The car is a regular
expression, and the cdr is the value that applies to repositories
whose directory matches the regular expression.  If more than one
item matches, then the *last* item in the list applies.  So, the
default value should come first in the list.

If the value is `tabs', highlight indentation with tabs.  If the
value is an integer, highlight indentation with at least that
many spaces.  Otherwise, highlight neither."
  :group 'habitrpg
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil))))
  :set 'habitrpg-set-variable-and-refresh)

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

(defface habitrpg-item-highlight
  '((t :bold t))
  ;; We used to inherit from `highlight', but:
  "Face for highlighting the current item.

This face should not set the background if the `habitrpg-diff-*'
faces, respectively the faces they inherit from, also make use of
the `:background' face attribute.  Otherwise the diff faces won't
have any effect.

To disable highlighting of the current item completely, make this
face inherit from `default' and remove all other attributes."
  :group 'habitrpg-faces)

(defface habitrpg-item-mark
  '((t :inherit secondary-selection))
  "Face for highlighting marked item."
  :group 'habitrpg-faces)

(defface habitrpg-whitespace-warning-face
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors in Habitrpg diffs."
  :group 'habitrpg-faces)

(defvar habitrpg-tmp-buffer-name " *habitrpg-tmp*")

(defvar habitrpg-current-indentation nil
  "Indentation highlight used in the current buffer.
This is calculated from `habitrpg-highlight-indentation'.")
(make-variable-buffer-local 'habitrpg-current-indentation)

(defconst hrpg-repeat-interval 120)
(defvar habitrpg-mode-hook nil "Hook run by `habitrpg-status-mode'.")

(defcustom habitrpg-executable "habit"
  "The name of the habitrpg executable."
  :group 'habitrpg
  :type 'string)

(defvar hrpg-timer)  
(defvar hrpg-id "")
(defvar hrpg-status "")
(defvar hrpg-status-to-file nil)
(defvar hrpg-tags-list nil)



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
    (define-key map (kbd "<backtab>") 'habitrpg-expand-collapse-section)
    (define-key map (kbd "RET") 'habitrpg-visit-item)
    (define-key map "\C-c\C-c" 'habitrpg-upvote-at-point)
    (define-key map (kbd "C-c C-k") 'habitrpg-downvote-at-point)
    (define-key map (kbd "g") 'habitrpg-refresh)
    (define-key map (kbd "G") 'habitrpg-refresh-all)
    map))

(defcustom habitrpg-status-buffer-switch-function 'pop-to-buffer
  "Function for `habitrpg-status' to use for switching to the status buffer.

The function is given one argument, the status buffer."
  :group 'habitrpg
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

(defun habitrpg-status ()
  (interactive)
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
  (habitrpg-create-buffer-sections
    (habitrpg-with-section 'status nil
			  (habitrpg-insert-stats)
			  (habitrpg-insert-tasks)
			  (habitrpg-insert-habits)
			  (habitrpg-insert-dailies)
			  (habitrpg-insert-rewards))))
   
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
  ;; Emacs' normal method of showing trailing whitespace gives weird
  ;; results when `habitrpg-whitespace-warning-face' is different from
  ;; `trailing-whitespace'.
  (if (and habitrpg-highlight-whitespace habitrpg-highlight-trailing-whitespace)
      (setq show-trailing-whitespace nil))
  (run-mode-hooks 'habitrpg-mode-hook))

(define-derived-mode habitrpg-status-mode habitrpg-mode "Habitrpg"
  "Mode for looking at status.

\\{habitrpg-status-mode-map}"
  :group 'habitrpg)

(defun habitrpg-cmd-output (cmd args)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'process-file
             cmd
             nil (list t nil) nil
             args))))

(defun habitrpg-string (&rest args)
  (habitrpg-trim-line (habitrpg-output args)))

(defun habitrpg-output (args)
  (habitrpg-cmd-output habitrpg-executable (append args)))

(defun habitrpg-trim-line (str)
  (if (string= str "")
      nil
    (if (equal (elt str (- (length str) 1)) ?\n)
        (substring str 0 (- (length str) 1))
      str)))

(defun habitrpg-cmd-insert (cmd args)
  (insert (habitrpg-cmd-output cmd args)))
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

;;; Macros

(defmacro habitrpg-with-refresh (&rest body)
  (declare (indent 0))
  `(habitrpg-refresh-wrapper (lambda () ,@body)))

(defun habitrpg-current-section ()
  "Return the Habitrpg section at point."
  (habitrpg-find-section-at (point)))

(defvar habitrpg-highlighted-section nil)

(defun habitrpg-refine-section (section)
  "Apply temporary refinements to the display of SECTION.
Refinements can be undone with `habitrpg-unrefine-section'."
  (let ((type (and section (habitrpg-section-type section))))
    (cond ((save-excursion
             (goto-char (habitrpg-section-beginning habitrpg-highlighted-section))
             )))))

(defun habitrpg-unrefine-section (section)
  "Remove refinements to the display of SECTION done by `habitrpg-refine-section'."
  (let ((type (and section (habitrpg-section-type section))))
    (cond ((remove-overlays (habitrpg-section-beginning section)
                            (habitrpg-section-end section)
                            )))))

(defvar habitrpg-highlight-overlay nil)

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

(defun habitrpg-highlight-section ()
  "Highlight current section if it has a type."
  (let ((section (habitrpg-current-section)))
    (when (not (eq section habitrpg-highlighted-section))
      (when habitrpg-highlighted-section
        ;; remove any refinement from previous hunk
        (habitrpg-unrefine-section habitrpg-highlighted-section))
      (setq habitrpg-highlighted-section section)
      (if (not habitrpg-highlight-overlay)
          (let ((ov (make-overlay 1 1)))
            (overlay-put ov 'face 'habitrpg-item-highlight)
            (setq habitrpg-highlight-overlay ov)))
      (if (and section (habitrpg-section-type section))
          (progn
            (habitrpg-refine-section section)
            (move-overlay habitrpg-highlight-overlay
                          (habitrpg-section-beginning section)
                          (habitrpg-section-end section)
                          (current-buffer)))
        (delete-overlay habitrpg-highlight-overlay)))))

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
            (revert-buffer t t nil)
          )))))

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
  "Refresh current buffer to match repository state.
Also revert every unmodified buffer visiting files
in the corresponding directory."
  (interactive)
  (habitrpg-with-refresh
    (habitrpg-need-refresh)))

(defun habitrpg-refresh-all ()
  "Refresh all habitrpg buffers.
"
  (interactive)
  (habitrpg-for-all-buffers #'habitrpg-refresh-buffer default-directory))

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

(defun habitrpg-find-section-before (pos)
  "Return the last section that begins before POS."
  (let ((section (habitrpg-find-section-at pos)))
    (cl-do* ((current (or (habitrpg-section-parent section)
                          section)
                      next)
             (next (if (not (habitrpg-section-hidden current))
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
    (habitrpg-goto-section (habitrpg-find-section-before (point)))))

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
  (goto-char (habitrpg-section-beginning section))
  (cond
   (
    (forward-line -1)
    (habitrpg-goto-next-section))
   ((and (eq (habitrpg-section-type section) 'commit)
         (derived-mode-p 'habitrpg-log-mode))
    )))

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
on ancestors and descendants of current section."
  (habitrpg-with-refresh
    (if all
        (habitrpg-section-show-level habitrpg-top-section 0 level nil)
      (let ((path (reverse (habitrpg-section-lineage (habitrpg-current-section)))))
        (habitrpg-section-show-level (car path) 0 level (cdr path))))))

(defun habitrpg-show-only-files ()
  "Show section that are files, but not there subsection.

Do this in on ancestors and descendants of current section."
  (interactive)
  (if (derived-mode-p 'habitrpg-status-mode)
      (call-interactively 'habitrpg-show-level-2)
    (call-interactively 'habitrpg-show-level-1)))

(defun habitrpg-show-only-files-all ()
  "Show section that are files, but not there subsection.
Do this for all sections"
  (interactive)
  (if (derived-mode-p 'habitrpg-status-mode)
      (call-interactively 'habitrpg-show-level-2-all)
    (call-interactively 'habitrpg-show-level-1-all)))

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

(habitrpg-define-inserter stats ()
			  (habitrpg-section 'stats
					    "Stats:" 'habitrpg-wash-stats
					    "user")) 
(habitrpg-define-inserter tasks ()
			  (habitrpg-section 'tasks
					    "Tasks:" 'habitrpg-wash-tasks
					    "tasks")) 

(habitrpg-define-inserter habits ()
			  (habitrpg-section 'habits
					    "Habits:" 'habitrpg-wash-habits
					    "tasks")) 

(habitrpg-define-inserter dailies ()
			  (habitrpg-section 'dailies
					    "Dailies:" 'habitrpg-wash-dailies
					    "tasks")) 
(habitrpg-define-inserter rewards ()
			  (habitrpg-section 'rewards
					    "Rewards:" 'habitrpg-wash-rewards
					    "tasks")) 

;; (habitrpg-define-inserter items ()
;; 			  (habitrpg-section 'items
;; 					    "Items:" 'habitrpg-wash-items
;; 					    "user")) 


(defun habitrpg-wash-stat ()
  (let ((entry-regexp ".*u'\\(exp\\|gp\\|hp\\|lvl\\)': \\([0-9].*\\),"))
    (if (looking-at entry-regexp)
      (let ((name (match-string-no-properties 1))
	    (value (match-string-no-properties 2)))
        (delete-region (match-beginning 0) (match-end 0))
        (goto-char (match-beginning 0))
        (fixup-whitespace)
        (goto-char (line-beginning-position))
	(cond 
	 ((string= "exp" name) 
	  (setq name "Experience"))
	 ((string= "gp" name)
	  (setq name "Gold"))
	 ((string= "hp" name)
	  (setq name "Health"))
	 ((string= "lvl" name) 
	  (setq name "Level")))
	(insert (concat name ": " value))
        (goto-char (line-beginning-position))
        (habitrpg-with-section name 'stats
          (habitrpg-set-section-info name)
          (forward-line)))
      (kill-line))
    t))
(defun habitrpg-wash-stats ()
  (let ((habitrpg-old-top-section nil))
    (habitrpg-wash-sequence #'habitrpg-wash-stat)))

(defun habitrpg-wash-task ()
  (let ((entry-regexp ".*u'text': u'\\(.*\\)',")
	(type-regexp ".*u'type': u'\\(.*\\)',"))
    (if (looking-at entry-regexp)
	(let ((task-name (match-string-no-properties 1)))
	  (delete-region (line-beginning-position) (line-end-position))
	  (delete-blank-lines)
	  (search-forward-regexp type-regexp nil t)
	  (let ((type (match-string-no-properties 1)))
	    (if (string-equal type "todo")
		(progn
		  (delete-region (match-beginning 0) (match-end 0))
		  (goto-char (match-beginning 0))
		  (fixup-whitespace)
		  (goto-char (line-beginning-position))
		  (insert task-name)
		  (goto-char (line-beginning-position))
		  (habitrpg-with-section task-name 'tasks
		    (habitrpg-set-section-info task-name)
		    (forward-line))))))
      (kill-line))
    t))

(defun habitrpg-wash-tasks ()
  (let ((habitrpg-old-top-section nil))
    (habitrpg-wash-sequence #'habitrpg-wash-task)))

(defun habitrpg-wash-habit ()
  (let ((entry-regexp ".*u'text': u'\\(.*\\)',")
	(type-regexp ".*u'type': u'\\(.*\\)',"))
    (if (looking-at entry-regexp)
	(let ((habit-name (match-string-no-properties 1)))
	  (delete-region (line-beginning-position) (line-end-position))
	  (delete-blank-lines)
	  (search-forward-regexp type-regexp nil t)
	  (let ((type (match-string-no-properties 1)))
	    (if (string-equal type "habit")
		(progn
		  (delete-region (match-beginning 0) (match-end 0))
		  (goto-char (match-beginning 0))
		  (fixup-whitespace)
		  (goto-char (line-beginning-position))
		  (insert habit-name)
		  (goto-char (line-beginning-position))
		  (habitrpg-with-section habit-name 'habits
		    (habitrpg-set-section-info habit-name)
		    (forward-line))))))
      (kill-line))
    t))
		 
(defun habitrpg-wash-habits ()
  (let ((habitrpg-old-top-section nil))
    (habitrpg-wash-sequence #'habitrpg-wash-habit)))

(defun habitrpg-wash-daily ()
  (let ((entry-regexp ".*u'text': u'\\(.*\\)',")
	(type-regexp ".*u'type': u'\\(.*\\)',"))
    (if (looking-at entry-regexp)
	(let ((daily-name (match-string-no-properties 1)))
	  (delete-region (line-beginning-position) (line-end-position))
	  (delete-blank-lines)
	  (search-forward-regexp type-regexp nil t)
	  (let ((type (match-string-no-properties 1)))
	    (if (string-equal type "daily")
		(progn
		  (delete-region (match-beginning 0) (match-end 0))
		  (goto-char (match-beginning 0))
		  (fixup-whitespace)
		  (goto-char (line-beginning-position))
		  (insert daily-name)
		  (goto-char (line-beginning-position))
		  (habitrpg-with-section daily-name 'dailies
		    (habitrpg-set-section-info daily-name)
		    (forward-line))))))
      (kill-line))
    t))
		 
(defun habitrpg-wash-dailies ()
  (let ((habitrpg-old-top-section nil))
    (habitrpg-wash-sequence #'habitrpg-wash-daily)))

(defun habitrpg-wash-reward ()
  (let ((entry-regexp ".*u'text': u'\\(.*\\)',")
	(type-regexp ".*u'type': u'\\(.*\\)',"))
    (if (looking-at entry-regexp)
	(let ((reward-name (match-string-no-properties 1)))
	  (delete-region (line-beginning-position) (line-end-position))
	  (delete-blank-lines)
	  (search-forward-regexp type-regexp nil t)
	  (let ((type (match-string-no-properties 1)))
	    (if (string-equal type "reward")
		(progn
		  (delete-region (match-beginning 0) (match-end 0))
		  (goto-char (match-beginning 0))
		  (fixup-whitespace)
		  (goto-char (line-beginning-position))
		  (insert reward-name)
		  (goto-char (line-beginning-position))
		  (habitrpg-with-section reward-name 'rewards
		    (habitrpg-set-section-info reward-name)
		    (forward-line))))))
      (kill-line))
    t))
		 
(defun habitrpg-wash-rewards ()
  (let ((habitrpg-old-top-section nil))
    (habitrpg-wash-sequence #'habitrpg-wash-reward)))

;; (defun habitrpg-wash-item ()
;;   (let ((entry-regexp ".*u'items': ")) ;;u'\\(.*\\)',"))
;;     (if (looking-at entry-regexp)
;; 	(let ((item-name (match-string-no-properties 1)))
;; 	  (delete-region (line-beginning-position) (line-end-position))
;; 	  (delete-blank-lines)
;; 	  (delete-region (match-beginning 0) (match-end 0))
;; 	  (goto-char (match-beginning 0))
;; 	  (fixup-whitespace)
;; 	  (goto-char (line-beginning-position))
;; 	  (insert item-name)
;; 	  (goto-char (line-beginning-position))
;; 	  (habitrpg-with-section item-name 'items
;; 	    (habitrpg-set-section-info item-name)
;; 	    (forward-line)))
;;       (kill-line))
;;     t))
		 
(defun habitrpg-wash-items ()
  (let ((habitrpg-old-top-section nil))
    (habitrpg-wash-sequence #'habitrpg-wash-item)))


(defun habitrpg-wash-sequence (func)
  "Run FUNC until end of buffer is reached.
FUNC should leave point at the end of the modified region"
  (while (and (not (eobp))
              (funcall func))))

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
                             buffer-title washer cmd &rest args)
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
            (habitrpg-cmd-insert cmd args)
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
                          buffer-title washer &rest args)
  "Run habit and put its result in a new section.
See `habitrpg-insert-section' for meaning of the arguments"
  (apply #'habitrpg-insert-section
         section-title-and-type
         buffer-title
         washer
         habitrpg-executable
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

(defun habitrpg-goto-line (line)
  "Like `goto-line' but doesn't set the mark."
  (save-restriction
    (widen)
    (goto-char 1)
    (forward-line (1- line))))


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

(defun habitrpg-get-id-at-point ()
  (interactive)
  (let ((point-task (buffer-substring (line-beginning-position) (line-end-position))))
    (replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "habit tasks | egrep 'text|id' | grep -B 1 \"" point-task "\" | sed -e 'q' | cut -d\"'\" -f4 &")))))

(defun habitrpg-upvote-at-point ()
  "Upvote a task. Add task if it doesn't exist."
  (interactive)
  (let ((id (habitrpg-get-id-at-point)))
    (setq hrpg-status (shell-command-to-string (concat "habit perform_task " id " up &")))
    (if hrpg-status-to-file
	(with-temp-file "~/tmp/hrpg-status"
	  (insert hrpg-status))))
  (habitrpg-refresh-status))

(defun habitrpg-downvote-at-point ()
  "Upvote a task. Add task if it doesn't exist."
  (interactive)
  (let ((id (habitrpg-get-id-at-point)))
    (setq hrpg-status (shell-command-to-string (concat "habit perform_task " id " down &")))
    (if hrpg-status-to-file
	(with-temp-file "~/tmp/hrpg-status"
	  (insert hrpg-status))))
  (habitrpg-refresh-status))

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
