
[![MELPA](http://melpa.org/packages/real-auto-save-badge.svg)](http://melpa.org/#/real-auto-save)

real-auto-save
==============
Automatically save your all your files at regular intervals.

Installation
------------

### package.el

MELPA
-----
You can install a snapshot of real-auto-save from the [MELPA](http://melpa.org/#/real-auto-save) repository. 


Github
------
Download `real-auto-save.el` and put this file in a folder where Emacs can find it.
Add following lines to your .emacs initialization file to enable auto save in all programming modes.

     (require 'real-auto-save)
     (add-hook 'prog-mode-hook 'real-auto-save-mode)


Auto save interval is 10 seconds by default. You can change it:

     (setq real-auto-save-interval 5) ;; in seconds
