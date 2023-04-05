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

(setq viper-mode t)
(setq viper-want-ctl-h-help t)
(setq viper-inhibit-startup-message t)
(setq viper-expert-level 5)
(require 'viper)
(defun fixed-viper-Put-back (arg) (interactive "P")
    (save-excursion (viper-Put-back arg)))
(define-key viper-vi-global-user-map "P" 'fixed-viper-Put-back)
(define-key viper-vi-global-user-map "u" 'undo)
(setq viper-vi-state-id "V")
(setq viper-emacs-state-id "E")
(setq viper-insert-state-id "I")
(setq viper-replace-state-id "R")
(setq mode-line-front-space
    '(:eval (if (boundp 'viper-mode-string) viper-mode-string " ")))
(delq 'viper-mode-string global-mode-string)

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
        "l" 'undo-tree-visualize-switch-branch-right)
    (define-key viper-vi-global-user-map "U" 'undo-tree-redo)
    (define-key viper-vi-global-user-map " u" 'undo-tree-visualize))
(use-package vertico
    :config
    (vertico-mode 1)
    (define-key viper-minibuffer-map "\C-m" 'vertico-exit)
    (define-key viper-minibuffer-map "\C-j" 'vertico-exit))
(use-package eat
    :config
    (eat-eshell-mode 1))

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
