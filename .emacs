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
(setq scroll-conservatively 101)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


(setq init-gc-cons-threshold gc-cons-threshold
      init-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1024 1024 1024)
      file-name-handler-alist nil)


(cl-defmethod gui-backend-get-selection
        (_selection-type _data-type &context (window-system nil))
    (with-temp-buffer
        (insert-file-contents "~/.clipboard-fix")
        (buffer-string)))
(advice-add 'gui-select-text :before (lambda (text)
    (with-temp-file "~/.clipboard-fix"
        (insert text))))


(define-key help-map "t" 'describe-face)


(defmacro save-mutation (&rest body)
    `(let ((buffer-undo-list ()))
        (unwind-protect (progn ,@body)
            (primitive-undo (length buffer-undo-list) buffer-undo-list))))

(defmacro save-advice (symbol where function &rest body)
    `(unwind-protect
        (progn
            (advice-add ,symbol ,where ,function)
            ,@body)
        (advice-remove ,symbol ,function)))


(defmacro unpack (names list)
    `(let ((--unpack-- ,list))
        (dolist (name ',names)
            (eval (list 'setq name '(car --unpack--)))
            (setq --unpack-- (cdr --unpack--)))))

(defmacro let-unpack-1 (names list &rest body)
    `(let ,names
        (unpack ,names ,list)
        ,@body))

(defmacro let-unpack (unpack-list &rest body)
    (let* ((forms (list 'unused)) (next-form nil) (last-form forms))
        (while unpack-list
            (let-unpack-1 (names list) unpack-list
                (setq next-form `(let-unpack-1 ,names ,list))
                (setcdr (last last-form) (list next-form))
                (setq last-form next-form))
            (setq unpack-list (cddr unpack-list)))
        (setcdr (last last-form) body)
        (cadr forms)))


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
    (setq eshell-history-size 65536)
    (defun hack-ring-empty-p (ring-empty-p ring)
        (if (eq ring eshell-history-ring)
            nil
            (funcall ring-empty-p ring)))
    (defun hack-ring-remove (ring-remove ring &optional index)
        (when index
            (funcall ring-remove ring index)))
    (defun fixed-eshell-add-input-to-history
            (eshell-add-input-to-history &rest arguments)
        (save-advice 'ring-empty-p :around 'hack-ring-empty-p
            (save-advice 'ring-remove :around 'hack-ring-remove
                (apply eshell-add-input-to-history arguments))))
    (advice-add 'eshell-add-input-to-history :around
        'fixed-eshell-add-input-to-history)
    (defun in-eshell-scrollback-p () (interactive)
        (text-property-any (point) (buffer-end 1) 'field 'prompt))
    (defun fixed-eshell-send-input () (interactive)
        (if (in-eshell-scrollback-p)
            (message "not in input")
            (eshell-send-input)))
    (add-to-list 'eshell-modules-list 'eshell-tramp))
(use-package esh-mode
    :config
    (define-key eshell-mode-map "\C-m" 'fixed-eshell-send-input))

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

(use-package dired
    :config
    (setq dired-dwim-target t))

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
    (defvar override-evil-mode-line-tag nil)
    (defmacro save-override-evil-mode-line-tag (tag help-string &rest body)
        `(unwind-protect
            (progn
                (setq override-evil-mode-line-tag (propertize ,tag
                    'help-echo ,help-string
                    'mouse-face 'mode-line-highlight))
                ,@body)
            (setq override-evil-mode-line-tag nil)))
    (setq mode-line-front-space '(:eval
        (if override-evil-mode-line-tag
            override-evil-mode-line-tag
            (if (boundp 'evil-mode-line-tag)
                (substring evil-mode-line-tag 2 3)
                " "))))
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
                ((evil-motion-state-p)   "#A0E0FF")
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
    (define-prefix-command 'space-map)
    (define-key evil-motion-state-map " " 'space-map)
    (define-key evil-motion-state-map "\C-@" 'space-map)
    (define-key evil-motion-state-map [?\C- ] 'space-map)
    (define-key evil-emacs-state-map "\C-@" 'space-map)
    (define-key evil-emacs-state-map [?\C- ] 'space-map)
    (define-key evil-motion-state-map " e" 'eval-last-sexp)
    (define-key evil-motion-state-map " g" (lambda () (interactive)
        (setq unread-command-events (listify-key-sequence "\C-g"))))
    (defun toggle-evil-repeat-move-cursor () (interactive)
        (setq evil-repeat-move-cursor (not evil-repeat-move-cursor))
        (message "evil-repeat-move-cursor: %s" evil-repeat-move-cursor))
    (evil-declare-not-repeat 'toggle-evil-repeat-move-cursor)
    (define-key evil-motion-state-map " ." 'toggle-evil-repeat-move-cursor)
    (define-key evil-motion-state-map " u" 'undo-tree-visualize)
    (add-hook 'undo-tree-visualizer-mode-hook (lambda ()
        (evil-local-set-key 'motion [escape] 'undo-tree-visualizer-quit)))
    (define-key evil-motion-state-map " b" 'switch-to-buffer)
    (define-key evil-motion-state-map " k" 'kill-buffer)
    (define-key evil-motion-state-map " f" 'find-file)
    (define-key evil-motion-state-map " r" 'revert-buffer)
    (define-key evil-motion-state-map " d" 'dired)
    (add-to-list 'evil-motion-state-modes 'dired-mode)
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
    (define-key evil-motion-state-map "gh" 'consult-history-execute)
    (define-key evil-motion-state-map " t" 'eshell)
    (add-to-list 'evil-normal-state-modes 'eshell-mode)
    (defvar evil-eshell-state-for-next-input 'normal)
    (make-variable-buffer-local 'evil-eshell-state-for-next-input)
    (advice-add 'eshell-send-input :before (lambda (&rest _)
        (setq evil-eshell-state-for-next-input evil-state)))
    (defun evil-eshell-force-normal-state () (interactive)
        (evil-force-normal-state)
        (setq evil-eshell-state-for-next-input evil-state))
    (add-hook 'eshell-mode-hook (lambda ()
        (evil-local-set-key 'insert [escape] 'evil-eshell-force-normal-state)))
    (add-hook 'eshell-pre-command-hook (lambda ()
        (setq evil-eshell-state-for-next-input evil-state)
        (evil-insert-state)))
    (add-hook 'eshell-post-command-hook (lambda ()
        (evil-change-state evil-eshell-state-for-next-input)))
    (evil-declare-not-repeat 'fixed-eshell-send-input)
    (evil-declare-not-repeat 'eshell-interrupt-process)
    (define-key evil-motion-state-map " T" 'eat)
    (add-to-list 'evil-emacs-state-modes 'eat-mode)
    (add-hook 'eat-exit-hook (lambda (_process) (evil-normal-state nil)))
    (add-hook 'eat-mode-hook (lambda ()
        (evil-local-set-key 'emacs "\C-[" 'eat-self-input)))
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
            (unhighlight-regexp ".\\{79\\}\\(.*\\)"))
        (message "display-fill-column-indicator-mode: %s"
            display-fill-column-indicator-mode))
    (evil-declare-not-repeat 'toggle-show-80+-characters)
    (define-key evil-motion-state-map " i" 'toggle-show-80+-characters)
    (define-key evil-motion-state-map " sh" 'evil-window-split)
    (define-key evil-motion-state-map " si" 'evil-window-vsplit)
    (define-key evil-motion-state-map " sk" 'evil-window-delete)
    (define-key evil-motion-state-map " so" 'other-window)
    (evil-define-motion evil-vertico-next-line (count)
        (unless count
            (setq count 1))
        (dotimes (_ count)
            (condition-case nil
                (evil-next-line)
                (end-of-buffer (vertico-next)))))
    (evil-define-motion evil-vertico-previous-line (count)
        (unless count
            (setq count 1))
        (dotimes (_ count)
            (condition-case _error
                (evil-previous-line)
                (beginning-of-buffer (vertico-previous)))))
    (add-hook 'minibuffer-setup-hook (lambda ()
        (evil-local-set-key 'normal "j" 'evil-vertico-next-line)
        (evil-local-set-key 'normal "k" 'evil-vertico-previous-line)
        (let ((quit (if (eq this-command 'consult-history-execute)
                         'consult-history-execute-quit
                         'abort-minibuffers)))
            (evil-local-set-key 'normal "q" quit)
            (evil-local-set-key 'normal [escape] quit)
            (evil-local-set-key 'operator "q" 'evil-force-normal-state)
            (evil-local-set-key 'operator [escape] 'evil-force-normal-state))))
    (add-hook 'Info-mode-hook (lambda ()
        (evil-local-set-key 'motion " " 'space-map)))
    (add-to-list 'evil-motion-state-modes 'Buffer-menu-mode)
    (add-to-list 'evil-motion-state-modes 'completion-list-mode)
    (add-to-list 'evil-motion-state-modes 'debugger-mode)
    (add-to-list 'evil-motion-state-modes 'tar-mode))

(use-package ace-window
    :config
    (setq aw-dispatch-when-more-than 1)
    (setq aw-scope 'frame)
    (setq aw-make-frame-char nil)
    (setq aw-leading-char-style 'path)
    (defun aw-window< (window-1 window-2)
        (let ((window-1-edges (window-edges window-1))
              (window-2-edges (window-edges window-2)))
            (let ((window-1-left-edge (car window-1-edges))
                  (window-1-top-edge (cadr window-1-edges))
                  (window-2-left-edge (car window-2-edges))
                  (window-2-top-edge (cadr window-2-edges)))
                (if (= window-1-top-edge window-2-top-edge)
                    (< window-1-left-edge window-2-left-edge)
                    (< window-1-top-edge window-2-top-edge)))))
    (defun hack-aw-window-list (window-list)
        (if (= (length window-list) 1)
            (cons (car window-list) window-list)
            window-list))
    (defun hack-avy-tree (arguments)
        (let ((candidate-list (car arguments)))
            (when (eq (cdar candidate-list) (cdadr candidate-list))
                (advice-remove 'aw-window-list 'hack-aw-window-list)
                (advice-remove 'avy-tree 'hack-avy-tree)
                (setcar arguments (cdr candidate-list))))
        arguments)
    (defun fixed-aw-select (action)
        (save-advice 'aw-window-list :filter-return 'hack-aw-window-list
            (save-advice 'avy-tree :filter-args 'hack-avy-tree
                (aw-select "" action))))
    (defun evil-aw-delete-window (window)
        (aw-switch-to-window window)
        (evil-window-delete))
    (defvar window-state nil)
    (defconst window-state-select
        '("#00FF00" "#606060" "W" "Window state"))
    (defconst window-state-select-latched
        '("#00FF00" "#208020" "W" "Window state"))
    (defconst window-state-swap
        '("#8000FF" "#402060" "S" "Swap window state"))
    (defconst window-state-copy
        '("#FFFF00" "#606000" "C" "Copy window state"))
    (defconst window-state-kill
        '("#FF0000" "#802020" "K" "Kill window state"))
    (defvar window-state--action nil)
    (defvar window-state--loop nil)
    (defvar window-state--loop-default nil)
    (defun window-state--loop ()
        (setq window-state--loop t)
        (setq window-state--loop-default nil)
        (while window-state--loop
            (setq window-state--loop window-state--loop-default)
            (let-unpack ((hint-color tint-color tag help-string) window-state)
                (set-face-foreground 'aw-leading-char-face hint-color)
                (set-face-foreground 'aw-background-face   tint-color)
                (save-override-evil-mode-line-tag tag help-string
                    (fixed-aw-select (lambda (window)
                        (funcall window-state--action window)))))))
    (defun window-state-select ()
        (setq window-state--action 'aw-switch-to-window)
        (setq window-state window-state-select)
        (setq window-state--loop-default nil)
        (setq window-state--loop t))
    (defun window-state-select-latched ()
        (unless (eq window-state window-state-select-latched)
            (setq window-state--action `(lambda (window)
                (aw-switch-to-window window)
                (setq window-state--action ',window-state--action)
                (setq window-state ',window-state)
                (setq window-state--loop-default ,window-state--loop-default)
                (setq window-state--loop t))))
        (setq window-state window-state-select-latched)
        (setq window-state--loop-default nil)
        (setq window-state--loop t))
    (defun window-state-swap (&optional lock)
        (setq window-state--action 'aw-swap-window)
        (setq window-state window-state-swap)
        (setq window-state--loop-default lock)
        (setq window-state--loop t))
    (defun window-state-swap-locked ()
        (window-state-swap t))
    (defun window-state-copy (&optional lock)
        (setq window-state--action 'aw-copy-window)
        (setq window-state window-state-copy)
        (setq window-state--loop-default lock)
        (setq window-state--loop t))
    (defun window-state-copy-locked ()
        (window-state-copy t))
    (defun window-state-kill (&optional lock)
        (setq window-state--action 'evil-aw-delete-window)
        (setq window-state window-state-kill)
        (setq window-state--loop-default lock)
        (setq window-state--loop t))
    (defun window-state-kill-locked ()
        (window-state-kill t))
    (defun window-state-split-horizontal ()
        (evil-window-split)
        (setq window-state--loop t))
    (defun window-state-split-vertical ()
        (evil-window-vsplit)
        (setq window-state--loop t))
    (defun window-state-toggle-auto-balance ()
        (setq evil-auto-balance-windows (not evil-auto-balance-windows))
        (message "evil-auto-balance-windows: %s" evil-auto-balance-windows)
        (setq window-state--loop t))
    (defun window-state () (interactive)
        (window-state-select)
        (window-state--loop))
    (defun window-state-done ()
        (setq window-state--loop nil))
    (setq aw-dispatch-alist '(
        (?o window-state-select)
        (?l window-state-select-latched)
        (?s window-state-swap)
        (?S window-state-swap-locked)
        (?c window-state-copy)
        (?C window-state-copy-locked)
        (?k window-state-kill)
        (?K window-state-kill-locked)
        (?h window-state-split-horizontal)
        (?i window-state-split-vertical)
        (?t window-state-toggle-auto-balance)
        (?q window-state-done)
        (?\C-\[ window-state-done)))
    (setq aw-dispatch-function (lambda (character)
        (if (equal character ?\C-g)
            (aw-dispatch-default ?q)
            (aw-dispatch-default character))))
    (define-key global-map "\C-xo" 'window-state)
    (define-key evil-motion-state-map " o" 'window-state))

(use-package with-editor
    :config
    (defun fixed-with-editor-return (with-editor-return cancel)
        (save-advice 'delete-file :around 'ignore
            (save-advice (if cancel 'save-buffer nil) :around 'ignore
                (funcall with-editor-return cancel))))
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
