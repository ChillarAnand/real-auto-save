
[![MELPA](http://melpa.org/packages/real-auto-save-badge.svg)](http://melpa.org/#/real-auto-save)

# real-auto-save
Automatically save your all your files at regular intervals.

## Installation

### MELPA

You can install a snapshot of real-auto-save from the [MELPA](http://melpa.org/#/real-auto-save) repository. 

### Github

Download `real-auto-save.el` and put this file in a folder where Emacs can find it.
Add following lines to your .emacs initialization file to enable auto save in all programming modes.

     (require 'real-auto-save)
     (add-hook 'prog-mode-hook 'real-auto-save-mode)

## Configuration

Auto save interval is 10 seconds by default. You can change it:

     (setq real-auto-save-interval 5) ;; in seconds

Emacs 26.1 introduced `auto-save-visited-mode`. But that new mode is global, and saves *all* buffers. For a more limited solution, use `real-auto-save` with a file-local variable or mode hook.

For example, to use this mode only for org files, add this to your Emacs config:

    (require 'real-auto-save)
    (add-hook 'org-mode 'real-auto-save)

## Acknowledgments

Thanks to [Steve Purcell](https://github.com/purcell) for reviewing and providing help with development.

Thanks to all contributors!
