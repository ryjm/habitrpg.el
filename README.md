

habitrpg.el
===============

Quick hack to integrate org-mode and habitrpg. Very much a work in progress. Adds a task to habitrpg.com after a TODO state change in org-mode or by calling the function `habitrpg-add`.

Tag (in org-mode) your habits and dailys with `hrpghabit`, `hrpgdaily`, and `hrpgreward` to get them in the corresponding categories.

Installation
------------
Install `request.el` and `deferred.el`, which you can get through el-get or package.el (M-x package-list-packages). I also included them in this repo.

Clone this repo: `git clone https://github.com/ryjm/habitrpg.el`

Add to your .emacs:

    (add-to-list 'load-path "path/to/repo/habitrpg.el")
    (setq habitrpg-api-user "ID-HERE")
    (setq habitrpg-api-token "TOKEN-HERE")

Add this hook if you want a DONE task to be marked as complete and a todo state change to add a task to habitrpg.com

	(add-hook 'org-after-todo-state-change-hook 'habitrpg-add 'append)

Add keybindings.

    (global-set-key (kbd "C-c C-x h") 'habitrpg-add)
	(global-set-key (kbd "<f9> a") 'habitrpg-status)

You can then bring up the habitrpg buffer with `<f9> a`, and do `C-h m` to see the keybindings.

![buffer](http://i.imgur.com/tOgCUD8.png)
![buffer2](http://i.imgur.com/w3XIzL9.gif)

-------------------------------------------------------------------------------

If you want to use the clocking feature:

	(add-hook 'org-clock-in-hook 'habitrpg-clock-in)
	(add-hook 'org-clock-out-hook 'habitrpg-clock-out)

and set the variable `hrpg-tags-list` to the habits you want to associate with the clocked task.

    (add-to-list 'hrpg-tags-list "PROGRAMMING")
    (add-to-list 'hrpg-tags-list "WORK")

Then your habit will get upvoted every two minutes.
