;;; habitrpg-key-mode.el --- interactively tune git invocation

;; Copyright (C) 2010  Phil Jackson (modified for habitrpg by ryjm)

;; Habitrpg is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Habitrpg is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Habitrpg.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements `habitrpg-key-mode' which is used throughout
;; Habitrpg to let the user interactively select the command, switches
;; and options to call Git with.  It can be though of as a way to
;; provide "postfix" arguments.

;;; Code:

(require 'habitrpg)

(eval-when-compile (require 'cl-lib))

(defvar habitrpg-key-mode-key-maps '()
  "This will be filled lazily with proper `define-key' built
keymaps as they're requested.")

(defvar habitrpg-key-mode-buf-name "*habitrpg-key: %s*"
  "Format string to create the name of the habitrpg-key buffer.")

(defvar habitrpg-key-mode-last-buffer nil
  "Store the last habitrpg-key buffer used.")

(defvar habitrpg-key-mode-current-args nil
  "A hash-table of current argument set")

(defvar habitrpg-key-mode-current-options '()
  "Current option set")

(defvar habitrpg-log-mode-window-conf nil
  "Will hold the pre-menu configuration of habitrpg.")

(defvar habitrpg-key-mode-groups
  '((manage
     (actions
      ("n" "Add" habitrpg-new-task)
      ("d" "Delete" habitrpg-delete-at-point))
      (switches
      ("-t" "Todo" "todo")
      ("-h" "Habit" "habit")
      ("-d" "Daily" "daily")
      ("-r" "Reward" "reward"))))
      "Holds the key, help, function mapping for the log-mode.
If you modify this make sure you reset `habitrpg-key-mode-key-maps'
to nil.")

(defun habitrpg-key-mode-delete-group (group)
  "Delete a group from `habitrpg-key-mode-key-maps'."
  (let ((items (assoc group habitrpg-key-mode-groups)))
    (when items
      ;; reset the cache
      (setq habitrpg-key-mode-key-maps nil)
      ;; delete the whole group
      (setq habitrpg-key-mode-groups
            (delq items habitrpg-key-mode-groups))
      ;; unbind the defun
      (habitrpg-key-mode-de-generate group))
    habitrpg-key-mode-groups))

(defun habitrpg-key-mode-add-group (group)
  "Add a new group to `habitrpg-key-mode-key-maps'.
If there already is a group of that name then this will
completely remove it and put in its place an empty one of the
same name."
  (when (assoc group habitrpg-key-mode-groups)
    (habitrpg-key-mode-delete-group group))
  (setq habitrpg-key-mode-groups
        (cons (list group (list 'actions) (list 'switches)) habitrpg-key-mode-groups)))

(defun habitrpg-key-mode-key-defined-p (for-group key)
  "Return t if KEY is defined as any option within FOR-GROUP.
The option may be a switch, argument or action."
  (catch 'result
    (let ((options (habitrpg-key-mode-options-for-group for-group)))
      (dolist (type '(actions switches arguments))
        (when (assoc key (assoc type options))
          (throw 'result t))))))

(defun habitrpg-key-mode-update-group (for-group thing &rest args)
  "Abstraction for setting values in `habitrpg-key-mode-key-maps'."
  (let* ((options (habitrpg-key-mode-options-for-group for-group))
         (things (assoc thing options))
         (key (car args)))
    (if (cdr things)
        (if (habitrpg-key-mode-key-defined-p for-group key)
            (error "%s is already defined in the %s group." key for-group)
          (setcdr (cdr things) (cons args (cddr things))))
      (setcdr things (list args)))
    (setq habitrpg-key-mode-key-maps nil)
    things))

(defun habitrpg-key-mode-insert-argument (for-group key desc arg read-func)
  "Add a new binding KEY in FOR-GROUP which will use READ-FUNC
to receive input to apply to argument ARG git is run.  DESC should
be a brief description of the binding."
  (habitrpg-key-mode-update-group for-group 'arguments key desc arg read-func))

(defun habitrpg-key-mode-insert-switch (for-group key desc switch)
  "Add a new binding KEY in FOR-GROUP which will add SWITCH to git's
command line when it runs.  DESC should be a brief description of
the binding."
  (habitrpg-key-mode-update-group for-group 'switches key desc switch))

(defun habitrpg-key-mode-insert-action (for-group key desc func)
  "Add a new binding KEY in FOR-GROUP which will run command FUNC.
DESC should be a brief description of the binding."
  (habitrpg-key-mode-update-group for-group 'actions key desc func))

(defun habitrpg-key-mode-options-for-group (for-group)
  "Retrieve the options for the group FOR-GROUP.
This includes switches, commands and arguments."
  (or (cdr (assoc for-group habitrpg-key-mode-groups))
      (error "Unknown group '%s'" for-group)))

(defun habitrpg-key-mode-help (for-group)
  "Provide help for a key within FOR-GROUP.
The user is prompted for the key."
  (let* ((opts (habitrpg-key-mode-options-for-group for-group))
         (man-page (cadr (assoc 'man-page opts)))
         (seq (read-key-sequence
               (format "Enter command prefix%s: "
                       (if man-page
                         (format ", `?' for man `%s'" man-page)
                         ""))))
         (actions (cdr (assoc 'actions opts))))
    (cond
      ;; if it is an action popup the help for the to-be-run function
      ((assoc seq actions) (describe-function (nth 2 (assoc seq actions))))
      ;; if there is "?" show a man page if there is one
      ((equal seq "?")
       (if man-page
           (man man-page)
         (error "No man page associated with `%s'" for-group)))
      (t (error "No help associated with `%s'" seq)))))

(defun habitrpg-key-mode-exec-at-point ()
  "Run action/args/option at point."
  (interactive)
  (let ((key (or (get-text-property (point) 'key-group-executor)
                 (error "Nothing at point to do."))))
    (call-interactively (lookup-key (current-local-map) key))))

(defun habitrpg-key-mode-build-exec-point-alist ()
  (save-excursion
    (goto-char (point-min))
    (let* ((exec (get-text-property (point) 'key-group-executor))
           (exec-alist (if exec `((,exec . ,(point))) nil)))
      (cl-do nil ((eobp) (nreverse exec-alist))
        (when (not (eq exec (get-text-property (point) 'key-group-executor)))
          (setq exec (get-text-property (point) 'key-group-executor))
          (when exec (push (cons exec (point)) exec-alist)))
        (forward-char)))))

(defun habitrpg-key-mode-jump-to-next-exec ()
  "Jump to the next action/args/option point."
  (interactive)
  (let* ((oldp (point))
         (old  (get-text-property oldp 'key-group-executor))
         (p    (if (= oldp (point-max)) (point-min) (1+ oldp))))
    (while (let ((new (get-text-property p 'key-group-executor)))
             (and (not (= p oldp)) (or (not new) (eq new old))))
      (setq p (if (= p (point-max)) (point-min) (1+ p))))
    (goto-char p)
    (skip-chars-forward " ")))

(defun habitrpg-key-mode-build-keymap (for-group)
  "Construct a normal looking keymap for the key mode to use.
Put it in `habitrpg-key-mode-key-maps' for fast lookup."
  (let* ((options (habitrpg-key-mode-options-for-group for-group))
         (actions (cdr (assoc 'actions options)))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    ;; ret dwim
    (define-key map (kbd "RET") 'habitrpg-key-mode-exec-at-point)
    ;; tab jumps to the next "button"
    (define-key map (kbd "TAB") 'habitrpg-key-mode-jump-to-next-exec)

    ;; all maps should `quit' with `C-g' or `q'
    (define-key map (kbd "C-g") `(lambda ()
                                   (interactive)
                                   (habitrpg-key-mode-command nil)))
    (define-key map (kbd "q")   `(lambda ()
                                   (interactive)
                                   (habitrpg-key-mode-command nil)))
    ;; run help
    (define-key map (kbd "?") `(lambda ()
                                 (interactive)
                                 (habitrpg-key-mode-help ',for-group)))

    (let ((defkey (lambda (k action)
                    (when (and (lookup-key map (car k))
                               (not (numberp (lookup-key map (car k)))))
                      (message "Warning: overriding binding for `%s' in %S"
                               (car k) for-group)
                      (ding)
                      (sit-for 2))
                    (define-key map (car k)
                      `(lambda () (interactive) ,action)))))
      (dolist (k actions)
        (funcall defkey k `(habitrpg-key-mode-command ',(nth 2 k))))
      (dolist (k switches)
        (funcall defkey k `(habitrpg-key-mode-add-option ',for-group ,(nth 2 k))))
      (dolist (k arguments)
        (funcall defkey k `(habitrpg-key-mode-add-argument
                            ',for-group ,(nth 2 k) ',(nth 3 k)))))

    (push (cons for-group map) habitrpg-key-mode-key-maps)
    map))

(defvar habitrpg-key-mode-prefix nil
  "For internal use.  Holds the prefix argument to the command
that brought up the key-mode window, so it can be used by the
command that's eventually invoked.")

(defun habitrpg-key-mode-command (func)
  (let ((current-prefix-arg (or current-prefix-arg habitrpg-key-mode-prefix))
        (habitrpg-custom-options habitrpg-key-mode-current-options))
    (maphash (lambda (k v)
               (push (concat k v) habitrpg-custom-options))
             habitrpg-key-mode-current-args)
    (set-window-configuration habitrpg-log-mode-window-conf)
    (kill-buffer habitrpg-key-mode-last-buffer)
    (when func
      (if habitrpg-custom-options 
	  (funcall func (mapconcat 'identity habitrpg-custom-options " "))
	(funcall func)))))
		   


(defun habitrpg-key-mode-add-argument (for-group arg-name input-func)
  (let ((input (funcall input-func (concat arg-name ": "))))
    (puthash arg-name input habitrpg-key-mode-current-args)
    (habitrpg-key-mode-redraw for-group)))

(defun habitrpg-key-mode-add-option (for-group option-name)
  "Toggles the appearance of OPTION-NAME in `habitrpg-key-mode-current-options'."
  (if (member option-name habitrpg-key-mode-current-options)
      (setq habitrpg-key-mode-current-options
            (delete option-name habitrpg-key-mode-current-options))
    (add-to-list 'habitrpg-key-mode-current-options option-name))
  (habitrpg-key-mode-redraw for-group))

(defun habitrpg-key-mode (for-group &optional original-opts)
  "Mode for habitrpg key selection.
All commands, switches and options can be toggled/actioned with
the key combination highlighted before the description."
  (interactive)
  ;; save the window config to restore it as was (no need to make this
  ;; buffer local)
  (setq habitrpg-log-mode-window-conf
        (current-window-configuration))
  ;; setup the mode, draw the buffer
  (let ((buf (get-buffer-create (format habitrpg-key-mode-buf-name
                                        (symbol-name for-group)))))
    (setq habitrpg-key-mode-last-buffer buf)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (set (make-local-variable
          'habitrpg-key-mode-current-options)
         original-opts)
    (set (make-local-variable
          'habitrpg-key-mode-current-args)
         (make-hash-table))
    (set (make-local-variable 'habitrpg-key-mode-prefix) current-prefix-arg)
    (habitrpg-key-mode-redraw for-group))
  (message
   (concat
    "Type a prefix key to toggle it. Run 'actions' with their prefixes. "
    "'?' for more help.")))

(defun habitrpg-key-mode-get-key-map (for-group)
  "Get or build the keymap for FOR-GROUP."
  (or (cdr (assoc for-group habitrpg-key-mode-key-maps))
      (habitrpg-key-mode-build-keymap for-group)))

(defun habitrpg-key-mode-redraw (for-group)
  "(re)draw the habitrpg key buffer."
  (let ((buffer-read-only nil)
        (current-exec (get-text-property (point) 'key-group-executor))
        (new-exec-pos)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (actions-p nil))
    (erase-buffer)
    (make-local-variable 'font-lock-defaults)
    (use-local-map (habitrpg-key-mode-get-key-map for-group))
    (setq actions-p (habitrpg-key-mode-draw for-group))
    (delete-trailing-whitespace)
    (setq mode-name "habitrpg-key-mode" major-mode 'habitrpg-key-mode)
    (when current-exec
      (setq new-exec-pos (cdr (assoc current-exec (habitrpg-key-mode-build-exec-point-alist)))))
    (if (and is-first actions-p)
      (progn (goto-char actions-p)
             (habitrpg-key-mode-jump-to-next-exec))
      (if new-exec-pos
          (progn (goto-char new-exec-pos)
                 (skip-chars-forward " "))
          (goto-char old-point))))
  (setq buffer-read-only t)
  (fit-window-to-buffer))

(defun habitrpg-key-mode-draw-header (header)
  "Draw a header with the correct face."
  (insert (propertize header 'face 'font-lock-keyword-face) "\n"))

(defvar habitrpg-key-mode-args-in-cols nil
  "When true, draw arguments in columns as with switches and options.")

(defun habitrpg-key-mode-draw-args (args)
  "Draw the args part of the menu."
  (habitrpg-key-mode-draw-buttons
   "Args"
   args
   (lambda (x)
     (format "(%s) %s"
             (nth 2 x)
             (propertize (gethash (nth 2 x) habitrpg-key-mode-current-args "")
                         'face 'widget-field)))
   (not habitrpg-key-mode-args-in-cols)))

(defun habitrpg-key-mode-draw-switches (switches)
  "Draw the switches part of the menu."
  (habitrpg-key-mode-draw-buttons
   "Switches"
   switches
   (lambda (x)
     (format "(%s)" (let ((s (nth 2 x)))
                      (if (member s habitrpg-key-mode-current-options)
                        (propertize s 'face 'font-lock-warning-face)
                        s))))))

(defun habitrpg-key-mode-draw-actions (actions)
  "Draw the actions part of the menu."
  (habitrpg-key-mode-draw-buttons "Actions" actions nil))

(defun habitrpg-key-mode-draw-buttons (section xs maker
                                    &optional one-col-each)
  (when xs
    (habitrpg-key-mode-draw-header section)
    (habitrpg-key-mode-draw-in-cols
     (mapcar (lambda (x)
               (let* ((head (propertize (car x) 'face 'font-lock-builtin-face))
                      (desc (nth 1 x))
                      (more (and maker (funcall maker x)))
                      (text (format " %s: %s%s%s"
                                    head desc (if more " " "") (or more ""))))
                 (propertize text 'key-group-executor (car x))))
             xs)
     one-col-each)))

(defun habitrpg-key-mode-draw-in-cols (strings one-col-each)
  "Given a list of strings, print in columns (using `insert').
If ONE-COL-EACH is true then don't columify, but rather, draw
each item on one line."
  (let ((longest-act (apply 'max (mapcar 'length strings))))
    (while strings
      (let ((str (car strings)))
        (let ((padding (make-string (- (+ longest-act 3) (length str)) ? )))
          (insert str)
          (if (or one-col-each
                  (and (> (+ (length padding) ;
                             (current-column)
                             longest-act)
                          (window-width))
                       (cdr strings)))
              (insert "\n")
            (insert padding))))
      (setq strings (cdr strings))))
  (insert "\n"))

(defun habitrpg-key-mode-draw (for-group)
  "Draw actions, switches and parameters.
Return the point before the actions part, if any, nil otherwise."
  (let* ((options (habitrpg-key-mode-options-for-group for-group))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (actions (cdr (assoc 'actions options)))
         (p nil))
    (habitrpg-key-mode-draw-switches switches)
    (habitrpg-key-mode-draw-args arguments)
    (when actions (setq p (point-marker)))
    (habitrpg-key-mode-draw-actions actions)
    (insert "\n")
    p))

(defun habitrpg-key-mode-de-generate (group)
  "Unbind the function for GROUP."
  (fmakunbound
   (intern (concat "habitrpg-key-mode-popup-" (symbol-name group)))))

(defun habitrpg-key-mode-generate (group)
  "Generate the key-group menu for GROUP."
  (let ((opts (habitrpg-key-mode-options-for-group group)))
    (eval
     `(defun ,(intern (concat "habitrpg-key-mode-popup-" (symbol-name group))) nil
        ,(concat "Key menu for " (symbol-name group))
        (interactive)
        (habitrpg-key-mode (quote ,group))))))

;; create the interactive functions for the key mode popups (which are
;; applied in the top-level key maps)
(mapc (lambda (g)
        (habitrpg-key-mode-generate (car g)))
      habitrpg-key-mode-groups)

(provide 'habitrpg-key-mode)
;;; habitrpg-key-mode.el ends here
