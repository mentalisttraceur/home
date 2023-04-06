(menu-bar-mode -1)
(defun init-graphic-frame (frame)
    (when (display-graphic-p frame)
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (select-frame frame)
        (set-frame-font "DejaVu Sans Mono-11")
        (set-background-color "#000000")
        (set-foreground-color "#A0A0A0")
        (set-cursor-color "#FFFFFF")))
(init-graphic-frame (selected-frame))
(add-hook 'after-make-frame-functions 'init-graphic-frame)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(setq init-gc-cons-threshold gc-cons-threshold
      init-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1024 1024 1024)
      file-name-handler-alist nil)

(defun message-time () (interactive)
    (message "%s" (format-time-string "%H:%M:%S")))
(defun message-date () (interactive)
    (message "%s" (format-time-string "%Y-%m-%d")))    
(defun message-battery () (interactive)
    (require 'battery)
    (message "%s" (battery-format "%p%%" (funcall battery-status-function))))

(unless (package-installed-p 'use-package)
    (defmacro use-package (&rest _)))

(use-package undo-tree
    :config
    (global-undo-tree-mode 1)
    (define-key undo-tree-visualizer-mode-map
        "a" 'undo-tree-visualizer-abort)
    (define-key undo-tree-visualizer-mode-map
        "h" 'undo-tree-visualize-switch-branch-left)
    (define-key undo-tree-visualizer-mode-map
        "j" 'undo-tree-visualize-redo)
    (define-key undo-tree-visualizer-mode-map
        "k" 'undo-tree-visualize-undo)
    (define-key undo-tree-visualizer-mode-map
        "l" 'undo-tree-visualize-switch-branch-right))
(use-package vertico
    :config
    (vertico-mode 1))
(use-package eat
    :config
    (eat-eshell-mode 1))
(use-package evil
    :init
    (setq evil-undo-system 'undo-tree)
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1)
    (setq evil-want-minibuffer t)
    (defun fixed-evil-paste-before (arg) (interactive "*P<x>")
        (save-excursion (evil-paste-before arg)))
    (define-key evil-normal-state-map "P" 'fixed-evil-paste-before)
    (define-key evil-normal-state-map "U" 'evil-redo)
    (define-key evil-normal-state-map "v" 'find-file)
    (setq mode-line-front-space '(:eval
        (if (boundp 'evil-mode-line-tag)
            (substring evil-mode-line-tag 2 3)
            " ")))
    (setq evil-mode-line-format nil)
    (setq evil-cross-lines t)
    (define-key evil-motion-state-map " " nil)
    (define-key evil-motion-state-map " u" 'undo-tree-visualize)
    (define-key evil-motion-state-map " b" 'switch-to-buffer)
    (define-key evil-motion-state-map " n" 'universal-argument)
    (define-key evil-motion-state-map " t" 'eat))

(when (display-graphic-p)
    (add-to-list 'command-switch-alist '("-exwm" . (lambda (_option)
        (windmove-default-keybindings 'meta)

        (set-frame-parameter nil 'fullscreen 'fullboth)

        (use-package exwm
            :config
            (add-hook 'exwm-update-title-hook
                (lambda () (rename-buffer exwm-title t)))
            (setq exwm-input-global-keys `(
                ([?\s-t] . message-time)
                ([?\s-d] . message-date)
                ([?\s-b] . message-battery)))
            (exwm-enable))))))

(setq gc-cons-threshold init-gc-cons-threshold
      file-name-handler-alist init-file-name-handler-alist)
