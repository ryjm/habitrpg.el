

habitrpg.el
===============

Quick hack to integrate org-mode and habitrpg. Very much a work in progress. It's a bit slow and triggers after a TODO state change in org-mode (so you need to have an intermediary stage between TODO and DONE, like NEXT, or just give habitrpg-add a keybinding).

Tag your habits and dailys with `hrpghabit`, `hrpgdaily`, and `hrpgreward` to get them in the right category.

Installation
------------

Install pyhabit (https://github.com/xeross/pyhabit) using pip

    pip install git+git://github.com/xeross/pyhabit
    pip install git+git://github.com/xeross/pyhabit-cli

Add this to your .emacs

	(require 'habitrpg)
	(add-to-list 'process-environment "HABIT_USER_ID=putidhere")
	(add-to-list 'process-environment "HABIT_API_KEY=putkeyhere")

Add these hooks if you want it to trigger after a state change, otherwise just add keybindings for them instead.

	(add-hook 'org-after-todo-state-change-hook 'habitrpg-add)
	(add-hook 'org-after-todo-state-change-hook 'habitrpg-done 'append)
