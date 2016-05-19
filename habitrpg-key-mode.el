;;; habitrpg-key-mode.el --- interactively tune git invocation

;; Copyright (C) 2010  Phil Jackson (modified for habitica by ryjm)

;; Habitica is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Habitica is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Habitica.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements `habitica-key-mode' which is used throughout
;; Habitica to let the user interactively select the command, switches
;; and options to call Git with.  It can be though of as a way to
;; provide "postfix" arguments.

;;; Code:

(require 'habitrpg)

(eval-when-compile (require 'cl-lib))

(defvar habitica-key-mode-key-maps '()
  "This will be filled lazily with proper `define-key' built
keymaps as they're requested.")

(defvar habitica-key-mode-buf-name "*habitica-key: %s*"
  "Format string to create the name of the habitica-key buffer.")

(defvar habitica-key-mode-last-buffer nil
  "Store the last habitica-key buffer used.")

(defvar habitica-key-mode-current-args nil
  "A hash-table of current argument set")

(defvar habitica-key-mode-current-options '()
  "Current option set")

(defvar habitica-log-mode-window-conf nil
  "Will hold the pre-menu configuration of habitica.")

(defvar habitica-key-mode-groups
  '((manage
     (actions
      ("r" "Revive" habitica-revive)
      ("n" "Add" habitica-new-task)
      ("d" "Delete" habitica-delete-at-point))
      (switches
      ("-t" "Todo" "todo")
      ("-h" "Habit" "habit")
      ("-d" "Daily" "daily")
      ("-r" "Reward" "reward"))))
      "Holds the key, help, function mapping for the log-mode.
If you modify this make sure you reset `habitica-key-mode-key-maps'
to nil.")

(defun habitica-key-mode-delete-group (group)
  "Delete a group from `habitica-key-mode-key-maps'."
  (let ((items (assoc group habitica-key-mode-groups)))
    (when items
      ;; reset the cache
      (setq habitica-key-mode-key-maps nil)
      ;; delete the whole group
      (setq habitica-key-mode-groups
            (delq items habitica-key-mode-groups))
      ;; unbind the defun
      (habitica-key-mode-de-generate group))
    habitica-key-mode-groups))

(defun habitica-key-mode-add-group (group)
  "Add a new group to `habitica-key-mode-key-maps'.
If there already is a group of that name then this will
completely remove it and put in its place an empty one of the
same name."
  (when (assoc group habitica-key-mode-groups)
    (habitica-key-mode-delete-group group))
  (setq habitica-key-mode-groups
        (cons (list group (list 'actions) (list 'switches)) habitica-key-mode-groups)))

(defun habitica-key-mode-key-defined-p (for-group key)
  "Return t if KEY is defined as any option within FOR-GROUP.
The option may be a switch, argument or action."
  (catch 'result
    (let ((options (habitica-key-mode-options-for-group for-group)))
      (dolist (type '(actions switches arguments))
        (when (assoc key (assoc type options))
          (throw 'result t))))))

(defun habitica-key-mode-update-group (for-group thing &rest args)
  "Abstraction for setting values in `habitica-key-mode-key-maps'."
  (let* ((options (habitica-key-mode-options-for-group for-group))
         (things (assoc thing options))
         (key (car args)))
    (if (cdr things)
        (if (habitica-key-mode-key-defined-p for-group key)
            (error "%s is already defined in the %s group." key for-group)
          (setcdr (cdr things) (cons args (cddr things))))
      (setcdr things (list args)))
    (setq habitica-key-mode-key-maps nil)
    things))

(defun habitica-key-mode-insert-argument (for-group key desc arg read-func)
  "Add a new binding KEY in FOR-GROUP which will use READ-FUNC
to receive input to apply to argument ARG git is run.  DESC should
be a brief description of the binding."
  (habitica-key-mode-update-group for-group 'arguments key desc arg read-func))

(defun habitica-key-mode-insert-switch (for-group key desc switch)
  "Add a new binding KEY in FOR-GROUP which will add SWITCH to git's
command line when it runs.  DESC should be a brief description of
the binding."
  (habitica-key-mode-update-group for-group 'switches key desc switch))

(defun habitica-key-mode-insert-action (for-group key desc func)
  "Add a new binding KEY in FOR-GROUP which will run command FUNC.
DESC should be a brief description of the binding."
  (habitica-key-mode-update-group for-group 'actions key desc func))

(defun habitica-key-mode-options-for-group (for-group)
  "Retrieve the options for the group FOR-GROUP.
This includes switches, commands and arguments."
  (or (cdr (assoc for-group habitica-key-mode-groups))
      (error "Unknown group '%s'" for-group)))

(defun habitica-key-mode-help (for-group)
  "Provide help for a key within FOR-GROUP.
The user is prompted for the key."
  (let* ((opts (habitica-key-mode-options-for-group for-group))
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

(defun habitica-key-mode-exec-at-point ()
  "Run action/args/option at point."
  (interactive)
  (let ((key (or (get-text-property (point) 'key-group-executor)
                 (error "Nothing at point to do."))))
    (call-interactively (lookup-key (current-local-map) key))))

(defun habitica-key-mode-build-exec-point-alist ()
  (save-excursion
    (goto-char (point-min))
    (let* ((exec (get-text-property (point) 'key-group-executor))
           (exec-alist (if exec `((,exec . ,(point))) nil)))
      (cl-do nil ((eobp) (nreverse exec-alist))
        (when (not (eq exec (get-text-property (point) 'key-group-executor)))
          (setq exec (get-text-property (point) 'key-group-executor))
          (when exec (push (cons exec (point)) exec-alist)))
        (forward-char)))))

(defun habitica-key-mode-jump-to-next-exec ()
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

(defun habitica-key-mode-build-keymap (for-group)
  "Construct a normal looking keymap for the key mode to use.
Put it in `habitica-key-mode-key-maps' for fast lookup."
  (let* ((options (habitica-key-mode-options-for-group for-group))
         (actions (cdr (assoc 'actions options)))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (map (make-sparse-keymap)))
    (suppress-keymap map 'nodigits)
    ;; ret dwim
    (define-key map (kbd "RET") 'habitica-key-mode-exec-at-point)
    ;; tab jumps to the next "button"
    (define-key map (kbd "TAB") 'habitica-key-mode-jump-to-next-exec)

    ;; all maps should `quit' with `C-g' or `q'
    (define-key map (kbd "C-g") `(lambda ()
                                   (interactive)
                                   (habitica-key-mode-command nil)))
    (define-key map (kbd "q")   `(lambda ()
                                   (interactive)
                                   (habitica-key-mode-command nil)))
    ;; run help
    (define-key map (kbd "?") `(lambda ()
                                 (interactive)
                                 (habitica-key-mode-help ',for-group)))

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
        (funcall defkey k `(habitica-key-mode-command ',(nth 2 k))))
      (dolist (k switches)
        (funcall defkey k `(habitica-key-mode-add-option ',for-group ,(nth 2 k))))
      (dolist (k arguments)
        (funcall defkey k `(habitica-key-mode-add-argument
                            ',for-group ,(nth 2 k) ',(nth 3 k)))))

    (push (cons for-group map) habitica-key-mode-key-maps)
    map))

(defvar habitica-key-mode-prefix nil
  "For internal use.  Holds the prefix argument to the command
that brought up the key-mode window, so it can be used by the
command that's eventually invoked.")

(defun habitica-key-mode-command (func)
  (let ((current-prefix-arg (or current-prefix-arg habitica-key-mode-prefix))
        (habitica-custom-options habitica-key-mode-current-options))
    (maphash (lambda (k v)
               (push (concat k v) habitica-custom-options))
             habitica-key-mode-current-args)
    (set-window-configuration habitica-log-mode-window-conf)
    (kill-buffer habitica-key-mode-last-buffer)
    (when func
      (if habitica-custom-options 
	  (funcall func (mapconcat 'identity habitica-custom-options " "))
	(funcall func)))))
		   


(defun habitica-key-mode-add-argument (for-group arg-name input-func)
  (let ((input (funcall input-func (concat arg-name ": "))))
    (puthash arg-name input habitica-key-mode-current-args)
    (habitica-key-mode-redraw for-group)))

(defun habitica-key-mode-add-option (for-group option-name)
  "Toggles the appearance of OPTION-NAME in `habitica-key-mode-current-options'."
  (if (member option-name habitica-key-mode-current-options)
      (setq habitica-key-mode-current-options
            (delete option-name habitica-key-mode-current-options))
    (add-to-list 'habitica-key-mode-current-options option-name))
  (habitica-key-mode-redraw for-group))

(defun habitica-key-mode (for-group &optional original-opts)
  "Mode for habitica key selection.
All commands, switches and options can be toggled/actioned with
the key combination highlighted before the description."
  (interactive)
  ;; save the window config to restore it as was (no need to make this
  ;; buffer local)
  (setq habitica-log-mode-window-conf
        (current-window-configuration))
  ;; setup the mode, draw the buffer
  (let ((buf (get-buffer-create (format habitica-key-mode-buf-name
                                        (symbol-name for-group)))))
    (setq habitica-key-mode-last-buffer buf)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (set (make-local-variable
          'habitica-key-mode-current-options)
         original-opts)
    (set (make-local-variable
          'habitica-key-mode-current-args)
         (make-hash-table))
    (set (make-local-variable 'habitica-key-mode-prefix) current-prefix-arg)
    (habitica-key-mode-redraw for-group))
  (message
   (concat
    "Type a prefix key to toggle it. Run 'actions' with their prefixes. "
    "'?' for more help.")))

(defun habitica-key-mode-get-key-map (for-group)
  "Get or build the keymap for FOR-GROUP."
  (or (cdr (assoc for-group habitica-key-mode-key-maps))
      (habitica-key-mode-build-keymap for-group)))

(defun habitica-key-mode-redraw (for-group)
  "(re)draw the habitica key buffer."
  (let ((buffer-read-only nil)
        (current-exec (get-text-property (point) 'key-group-executor))
        (new-exec-pos)
        (old-point (point))
        (is-first (zerop (buffer-size)))
        (actions-p nil))
    (erase-buffer)
    (make-local-variable 'font-lock-defaults)
    (use-local-map (habitica-key-mode-get-key-map for-group))
    (setq actions-p (habitica-key-mode-draw for-group))
    (delete-trailing-whitespace)
    (setq mode-name "habitica-key-mode" major-mode 'habitica-key-mode)
    (when current-exec
      (setq new-exec-pos (cdr (assoc current-exec (habitica-key-mode-build-exec-point-alist)))))
    (if (and is-first actions-p)
      (progn (goto-char actions-p)
             (habitica-key-mode-jump-to-next-exec))
      (if new-exec-pos
          (progn (goto-char new-exec-pos)
                 (skip-chars-forward " "))
          (goto-char old-point))))
  (setq buffer-read-only t)
  (fit-window-to-buffer))

(defun habitica-key-mode-draw-header (header)
  "Draw a header with the correct face."
  (insert (propertize header 'face 'font-lock-keyword-face) "\n"))

(defvar habitica-key-mode-args-in-cols nil
  "When true, draw arguments in columns as with switches and options.")

(defun habitica-key-mode-draw-args (args)
  "Draw the args part of the menu."
  (habitica-key-mode-draw-buttons
   "Args"
   args
   (lambda (x)
     (format "(%s) %s"
             (nth 2 x)
             (propertize (gethash (nth 2 x) habitica-key-mode-current-args "")
                         'face 'widget-field)))
   (not habitica-key-mode-args-in-cols)))

(defun habitica-key-mode-draw-switches (switches)
  "Draw the switches part of the menu."
  (habitica-key-mode-draw-buttons
   "Switches"
   switches
   (lambda (x)
     (format "(%s)" (let ((s (nth 2 x)))
                      (if (member s habitica-key-mode-current-options)
                        (propertize s 'face 'font-lock-warning-face)
                        s))))))

(defun habitica-key-mode-draw-actions (actions)
  "Draw the actions part of the menu."
  (habitica-key-mode-draw-buttons "Actions" actions nil))

(defun habitica-key-mode-draw-buttons (section xs maker
                                    &optional one-col-each)
  (when xs
    (habitica-key-mode-draw-header section)
    (habitica-key-mode-draw-in-cols
     (mapcar (lambda (x)
               (let* ((head (propertize (car x) 'face 'font-lock-builtin-face))
                      (desc (nth 1 x))
                      (more (and maker (funcall maker x)))
                      (text (format " %s: %s%s%s"
                                    head desc (if more " " "") (or more ""))))
                 (propertize text 'key-group-executor (car x))))
             xs)
     one-col-each)))

(defun habitica-key-mode-draw-in-cols (strings one-col-each)
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

(defun habitica-key-mode-draw (for-group)
  "Draw actions, switches and parameters.
Return the point before the actions part, if any, nil otherwise."
  (let* ((options (habitica-key-mode-options-for-group for-group))
         (switches (cdr (assoc 'switches options)))
         (arguments (cdr (assoc 'arguments options)))
         (actions (cdr (assoc 'actions options)))
         (p nil))
    (habitica-key-mode-draw-switches switches)
    (habitica-key-mode-draw-args arguments)
    (when actions (setq p (point-marker)))
    (habitica-key-mode-draw-actions actions)
    (insert "\n")
    p))

(defun habitica-key-mode-de-generate (group)
  "Unbind the function for GROUP."
  (fmakunbound
   (intern (concat "habitica-key-mode-popup-" (symbol-name group)))))

(defun habitica-key-mode-generate (group)
  "Generate the key-group menu for GROUP."
  (let ((opts (habitica-key-mode-options-for-group group)))
    (eval
     `(defun ,(intern (concat "habitica-key-mode-popup-" (symbol-name group))) nil
        ,(concat "Key menu for " (symbol-name group))
        (interactive)
        (habitica-key-mode (quote ,group))))))

;; create the interactive functions for the key mode popups (which are
;; applied in the top-level key maps)
(mapc (lambda (g)
        (habitica-key-mode-generate (car g)))
      habitica-key-mode-groups)

(provide 'habitrpg-key-mode)
;;; habitrpg-key-mode.el ends here
