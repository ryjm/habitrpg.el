

habitrpg.el
===============

Quick hack to integrate org-mode and habitrpg. Very much a work in progress. It's a bit slow and triggers after a TODO state change in org-mode (so you need to have an intermediary stage between TODO and DONE, like NEXT, or just give habitrpg-add a keybinding).

Only adds the headlines right now, no body text.

Installation
------------

Install pyhabit using pip

    pip install git+git://github.com/xeross/pyhabit
    pip install git+git://github.com/xeross/pyhabit-cli

Add this to your .emacs

	(require 'habitrpg)
	(add-to-list 'process-environment "HABIT_USER_ID=putidhere")
	(add-to-list 'process-environment "HABIT_API_KEY=putkeyhere")

	(add-hook 'org-after-todo-state-change-hook 'habitrpg-add)
	(add-hook 'org-after-todo-state-change-hook 'habitrpg-done 'append)
