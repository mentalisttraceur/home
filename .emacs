(setq backup-inhibited t) 
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)


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
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq enable-recursive-minibuffers t)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


(setq init-gc-cons-threshold gc-cons-threshold
      init-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1024 1024 1024)
      file-name-handler-alist nil)


(defmacro save-mutation (&rest body)
    `(let ((buffer-undo-list ()))
        (unwind-protect (progn ,@body)
            (primitive-undo (length buffer-undo-list) buffer-undo-list))))


(unless (and (fboundp 'package-installed-p)
             (package-installed-p 'use-package))
    (defmacro use-package (&rest _)))

(use-package eshell
    :config
    (setq eshell-highlight-prompt nil)
    (setq eshell-prompt-regexp "^[$#] ")
    (setq eshell-prompt-function (lambda ()
        (propertize
            (if (= (user-uid) 0) "# " "$ ")
            'read-only t
            'field 'prompt
            'front-sticky '(read-only)
            'rear-nonsticky t)))
    (setq eshell-hist-ignoredups 'erase)
    (setq eshell-history-size 65536))

(use-package comint
    :config
    (add-hook 'comint-preoutput-filter-functions (lambda (output)
        (propertize output 'read-only t))))

(defun get-command-line-at-point ()
    (let ((start (line-beginning-position))
          (end   (line-end-position)))
        (buffer-substring-no-properties start end)))
(defun delete-command-line-at-point ()
    (let ((start (line-beginning-position))
          (end   (line-end-position)))
        (delete-region start end)))
(defun replace-command-line-at-point (command)
    (delete-command-line-at-point)
    (insert command))

(use-package undo-tree
    :config
    (global-undo-tree-mode 1)
    (setq undo-tree-auto-save-history nil)
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

(use-package consult
    :config
    (defun fixed-consult-history (prefix-argument) (interactive "P")
        (let ((command (when prefix-argument (get-command-line-at-point))))
            (save-excursion (save-mutation
                (end-of-buffer)
                (when prefix-argument
                    (replace-command-line-at-point command))
                (beginning-of-line)
                (consult-history)
                (setq command (get-command-line-at-point))))
            (end-of-buffer)
            (replace-command-line-at-point command)
            command)))

(use-package orderless
    :config
    (add-to-list 'completion-styles 'orderless))

(use-package ace-window
    :config
    (setq aw-dispatch-when-more-than 1)
    (setq aw-scope 'frame)
    (setq aw-make-frame-char nil)
    (setq aw-dispatch-alist '(
        (?o aw-flip-window)
        (?m delete-other-windows "Maximize Window")
        (?x aw-swap-window "Swap Window")
        (?c aw-copy-window "Copy Window")
        (?n aw-switch-to-window "Window")
        (?q ignore)
        (?? aw-show-dispatch-help)))
    (define-key global-map "\C-xo" 'ace-window))

(use-package eat
    :config
    (set-face-foreground 'eat-term-color-0  "#505050")
    (set-face-foreground 'eat-term-color-1  "#C00000")
    (set-face-foreground 'eat-term-color-2  "#00C000")
    (set-face-foreground 'eat-term-color-3  "#C0C000")
    (set-face-foreground 'eat-term-color-4  "#6060C0")
    (set-face-foreground 'eat-term-color-5  "#C000C0")
    (set-face-foreground 'eat-term-color-6  "#00C0C0")
    (set-face-foreground 'eat-term-color-7  "#E0E0E0")
    (set-face-foreground 'eat-term-color-8  "#707070")
    (set-face-foreground 'eat-term-color-9  "#FF0000")
    (set-face-foreground 'eat-term-color-10 "#00FF00")
    (set-face-foreground 'eat-term-color-11 "#FFFF00")
    (set-face-foreground 'eat-term-color-12 "#8080FF")
    (set-face-foreground 'eat-term-color-13 "#FF00FF")
    (set-face-foreground 'eat-term-color-14 "#00FFFF")
    (set-face-foreground 'eat-term-color-15 "#FFFFFF")
    (eat-eshell-mode 1)
    (eat-eshell-visual-command-mode 1))

(use-package evil
    :init
    (setq evil-undo-system 'undo-tree)
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1)
    (defun fixed-evil-paste-before (prefix-argument) (interactive "*P<x>")
        (save-excursion (evil-paste-before prefix-argument)))
    (define-key evil-normal-state-map "P" 'fixed-evil-paste-before)
    (define-key evil-normal-state-map "U" 'evil-redo)
    (setq mode-line-front-space '(:eval
        (if (boundp 'evil-mode-line-tag)
            (substring evil-mode-line-tag 2 3)
            " ")))
    (setq evil-mode-line-format nil)
    (setq evil-want-minibuffer t)
    (defun color-code-vi-state ()
        (if (minibufferp)
            (face-remap-add-relative 'minibuffer-prompt :foreground (cond
                ((evil-normal-state-p)   "#FF0000")
                ((evil-operator-state-p) "#FF8000")
                ((evil-insert-state-p)   "#00FF00")
                ((evil-replace-state-p)  "#FFFF00")
                ((evil-visual-state-p)   "#8080FF")
                ((evil-emacs-state-p)    "#8000FF")
                ((evil-motion-state-p)   "#20E0FF")
                (t                       "#FFFFFF")))
            (face-remap-add-relative 'mode-line :background (cond
                ((evil-normal-state-p)   "#FF4040")
                ((evil-operator-state-p) "#FFA060")
                ((evil-insert-state-p)   "#40FF40")
                ((evil-replace-state-p)  "#FFFF80")
                ((evil-visual-state-p)   "#C0C0FF")
                ((evil-emacs-state-p)    "#C040FF")
                ((evil-motion-state-p)   "#80FFFF")
                (t                       "#FFFFFF")))))
    (add-hook 'evil-normal-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-operator-state-entry-hook 'color-code-vi-state)
    (add-hook 'evil-insert-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-replace-state-entry-hook  'color-code-vi-state)
    (add-hook 'evil-visual-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-emacs-state-entry-hook    'color-code-vi-state)
    (add-hook 'evil-motion-state-entry-hook   'color-code-vi-state)
    (set-face-foreground 'mode-line "#010101")
    (setq evil-cross-lines t)
    (define-key evil-insert-state-map "\C-d" nil)
    (define-key evil-insert-state-map "\C-t" nil)
    (define-key evil-motion-state-map "\C-m" nil)
    (define-key evil-motion-state-map " " nil)
    (define-key evil-motion-state-map " e" 'eval-last-sexp)
    (define-key evil-motion-state-map " g" (lambda () (interactive)
        (setq unread-command-events (listify-key-sequence "\C-g"))))
    (define-key evil-motion-state-map " u" 'undo-tree-visualize)
    (define-key evil-motion-state-map " b" 'switch-to-buffer)
    (define-key evil-motion-state-map " k" 'kill-buffer)
    (define-key evil-motion-state-map " o" 'ace-window)
    (define-key evil-motion-state-map " f" 'find-file)
    (define-key evil-motion-state-map " d" 'dired)
    (use-package dired
        :config
        (define-key dired-mode-map " " nil))
    (define-key evil-motion-state-map " y" 'execute-extended-command)
    (define-key evil-motion-state-map " h" help-map)
    (define-key evil-motion-state-map " p" 'list-packages)
    (add-to-list 'evil-motion-state-modes 'package-menu-mode)
    (define-key evil-motion-state-map " n" 'universal-argument)
    (define-key universal-argument-map " n" 'universal-argument-more)
    (define-key evil-normal-state-map "q" nil)
    (define-key evil-motion-state-map " q" 'evil-record-macro)
    (defun consult-history-execute (prefix-argument) (interactive "P")
        (let* ((consult-history-execute t)
               (command (fixed-consult-history prefix-argument))
               (run-key (listify-key-sequence "\C-m")))
            (evil-repeat-start)
            (add-to-list 'evil-repeat-info `((lambda ()
                (end-of-buffer)
                (replace-command-line-at-point ,command)
                (when ,consult-history-execute
                    (setq unread-command-events '(,@run-key))))))
            (evil-repeat-stop)
            (when consult-history-execute
                (setq unread-command-events run-key))))
    (defun consult-history-execute-quit () (interactive)
        (setq consult-history-execute nil)
        (vertico-exit))
    (add-hook 'minibuffer-setup-hook (lambda ()
        (if (eq this-command 'consult-history-execute)
            (evil-local-set-key 'normal "q" 'consult-history-execute-quit)
            (evil-local-set-key 'normal "q" 'abort-minibuffers))))
    (define-key evil-motion-state-map "gh" 'consult-history-execute)
    (define-key evil-motion-state-map " t" 'eshell)
    (defvar evil-state-before-eshell-command evil-state)
    (add-hook 'eshell-pre-command-hook (lambda ()
        (setq evil-state-before-eshell-command evil-state)
        (evil-insert-state)))
    (add-hook 'eshell-post-command-hook (lambda ()
        (evil-change-state evil-state-before-eshell-command)))
    (evil-declare-not-repeat 'eshell-send-input)
    (evil-declare-not-repeat 'eshell-interrupt-process)
    (define-key evil-motion-state-map " T" 'eat)
    (add-to-list 'evil-emacs-state-modes 'eat-mode)
    (add-hook 'eat-exit-hook (lambda (_process) (evil-normal-state nil)))
    (evil-define-key 'emacs eat-semi-char-mode-map "\C-[" 'eat-self-input)
    (define-key eat-semi-char-mode-map "\C-c\C-z" 'eat-self-input)
    (defun comint-exit-to-evil-normal-state ()
        (let* ((process  (get-buffer-process (current-buffer)))
               (sentinel (process-sentinel process)))
            (set-process-sentinel process `(lambda (process message)
                (unless (process-live-p process)
                    (evil-normal-state nil))
                (,sentinel process message)))))
    (add-hook 'comint-exec-hook 'comint-exit-to-evil-normal-state)
    (evil-set-register ?r (string-join (make-list 8 "1234567890")))
    (evil-set-register ?R "\n")
    (evil-set-register ?r (propertize (evil-get-register ?r)
        'yank-handler '(evil-yank-line-handler nil t)))
    (setq-default display-fill-column-indicator-column 79)
    (defun toggle-show-80+-characters () (interactive)
        (display-fill-column-indicator-mode 'toggle)
        (if display-fill-column-indicator-mode
            (highlight-regexp   ".\\{79\\}\\(.*\\)" 'hi-yellow 1)
            (unhighlight-regexp ".\\{79\\}\\(.*\\)")))
    (evil-declare-not-repeat 'toggle-show-80+-characters)
    (define-key evil-motion-state-map " i" 'toggle-show-80+-characters)
    (define-key evil-motion-state-map " sh" 'evil-window-split)
    (define-key evil-motion-state-map " si" 'evil-window-vsplit)
    (define-key evil-motion-state-map " sk" 'evil-window-delete)
    (defun kill-buffer-window-delete () (interactive)
        (when (kill-buffer nil)
            (evil-window-delete)))
    (define-key evil-motion-state-map " sK" 'kill-buffer-window-delete)
    (add-to-list 'aw-dispatch-alist '(?k (lambda (window)
        (aw-switch-to-window window)
        (evil-window-delete)) "Delete Window"))
    (add-to-list 'aw-dispatch-alist '(?K (lambda (window)
        (aw-switch-to-window window)
        (kill-buffer-window-delete)) "Kill Window"))
    (add-to-list 'evil-motion-state-modes 'Buffer-menu-mode)
    (add-to-list 'evil-motion-state-modes 'completion-list-mode)
    (add-to-list 'evil-motion-state-modes 'debugger-mode)
    (add-to-list 'evil-motion-state-modes 'tar-mode)
    (add-to-list 'evil-normal-state-modes 'eshell-mode))

(use-package with-editor
    :config
    (defun fixed-with-editor-return (with-editor-return &rest arguments)
        (unwind-protect
            (progn
                (when (car arguments)
                    (advice-add 'save-buffer :around 'ignore))
                (advice-add 'delete-file :around 'ignore)
                (apply with-editor-return arguments))
            (advice-remove 'save-buffer 'ignore)
            (advice-remove 'delete-file 'ignore)))
    (advice-add 'with-editor-return :around 'fixed-with-editor-return)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)
    (shell-command-with-editor-mode 1))


(defun message-time () (interactive)
    (message "%s" (format-time-string "%H:%M:%S")))
(defun message-date () (interactive)
    (message "%s" (format-time-string "%Y-%m-%d")))
(defun message-battery () (interactive)
    (require 'battery)
    (message "%s" (battery-format "%p%%" (funcall battery-status-function))))


(when (display-graphic-p)
    (add-to-list 'command-switch-alist '("-exwm" . (lambda (_option)
        (use-package exwm
            :config
            (windmove-default-keybindings 'meta)
            (set-frame-parameter nil 'fullscreen 'fullboth)
            (add-hook 'exwm-update-title-hook
                (lambda () (rename-buffer exwm-title t)))
            (setq exwm-input-global-keys `(
                ([?\s-r] . exwm-reset)
                ([?\s-w] . exwm-workspace-switch)
                ([?\s-&] . (lambda (command)
                    (interactive (list (read-shell-command "$ ")))
                    (start-process-shell-command command nil command)))
                ,@(mapcar
                    (lambda (i) `(,(kbd (format "s-%d" i)) .
                        (lambda () (interactive)
                            (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
                ([?\s-t] . message-time)
                ([?\s-d] . message-date)
                ([?\s-b] . message-battery)))
            (exwm-enable))))))


(setq gc-cons-threshold init-gc-cons-threshold
      file-name-handler-alist init-file-name-handler-alist)
