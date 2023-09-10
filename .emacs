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
        (set-frame-font "DejaVu Sans Mono-13")
        (set-background-color "#000000")
        (set-foreground-color "#A0A0A0")
        (set-cursor-color "#FFFFFF")))
(init-graphic-frame (selected-frame))
(add-hook 'after-make-frame-functions 'init-graphic-frame)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq enable-recursive-minibuffers t)
(setq scroll-conservatively 101)
(setq hscroll-step 1)
(setq hscroll-margin 1)
(setq-default indent-tabs-mode nil)


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


(define-key minibuffer-inactive-mode-map [mouse-1] nil)


(setq init-gc-cons-threshold gc-cons-threshold
      init-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1024 1024 1024)
      file-name-handler-alist nil)


(when (getenv "TERMUX_VERSION")
    (cl-defmethod gui-backend-get-selection
            (_selection-type _data-type &context (window-system nil))
        (with-temp-buffer
            (insert-file-contents "~/.clipboard-fix")
            (buffer-string)))
    (advice-add 'gui-select-text :before (lambda (text)
        (with-temp-file "~/.clipboard-fix"
            (insert text))))
    (defun fixed-browse-url-xdg-open (url &optional _unused)
        (message "opening %s" url)
        (browse-url-xdg-open url))
    (setq browse-url-browser-function 'fixed-browse-url-xdg-open))


(defalias 'yes-or-no-p 'y-or-n-p)


(blink-cursor-mode -1)


(defun require-final-newline-in-new-files ()
    (unless (file-exists-p buffer-file-name)
        (setq-local require-final-newline t))
    nil)
(add-hook 'find-file-hook 'require-final-newline-in-new-files)


(define-key global-map [\C-tab] 'other-window)
(define-key global-map [\C-iso-lefttab] (lambda () (interactive)
    (other-window -1)))
(add-hook 'minibuffer-setup-hook (lambda ()
    (local-set-key [\C-tab] 'other-window)))


(defun quit-previous-window () (interactive)
    (if (> (count-windows) 1)
        (save-selected-window
            (quit-window nil (get-mru-window nil nil t)))
        (error "Attempt to quit sole window")))


(electric-indent-mode -1)
(setq-default c-basic-offset 4)
(setq lisp-indent-offset 4)


(defmacro save-mutation (&rest body)
    `(let ((buffer-undo-list ()))
        (unwind-protect (progn ,@body)
            (primitive-undo (length buffer-undo-list) buffer-undo-list))))


(defmacro with-temporary-file (name &rest body)
    `(let (,name)
        (unwind-protect
            (progn
                (setq ,name (make-temp-file ""))
                ,@body)
            (when ,name
                (delete-file ,name)))))

(defmacro with-temporary-directory (name &rest body)
    `(let (,name)
        (unwind-protect
            (progn
                (setq ,name (make-temp-file "" t))
                ,@body)
            (when ,name
                (delete-directory ,name t)))))


(defmacro apply-split-nest (callable arguments count body)
    (setq count (1- count))
    (let* ((forms '(unused))
           (last-cons forms))
        (while arguments
            (setcdr last-cons (list (cons callable arguments)))
            (setq last-cons (nthcdr count arguments))
            (setq arguments (cdr last-cons)))
        (setcdr last-cons body)
        (cadr forms)))


(defmacro unpack (names list)
    (let* ((setq-form (list 'setq))
           (last-cons setq-form)
           (unpack-1 '(prog1 (car --unpack--)
                             (setq --unpack-- (cdr --unpack--)))))
        (dolist (name names)
            (setcdr last-cons (list name unpack-1))
            (setq last-cons (cddr last-cons)))
        `(let ((--unpack-- ,list)) ,setq-form nil)))

(defmacro let-unpack-1 (names list &rest body)
    `(let ((--let-unpack-1-- ,list) ,@names)
        (unpack ,names --let-unpack-1--)
        ,@body))

(defmacro let-unpack (unpack-list &rest body)
    `(apply-split-nest let-unpack-1 ,unpack-list 2 ,body))

(defmacro uncons (car-name cdr-name cell)
    `(let ((--uncons-- ,cell))
        (setq ,car-name (car --uncons--)
              ,cdr-name (cdr --uncons--))
        nil))

(defmacro let-uncons-1 (car-name cdr-name cell &rest body)
    `(let ((--let-uncons-1-- ,cell) ,car-name ,cdr-name)
        (uncons ,car-name ,cdr-name --let-uncons-1--)
        ,@body))

(defmacro let-uncons (uncons-list &rest body)
    `(apply-split-nest let-uncons-1 ,uncons-list 3 ,body))


(defmacro with-advice-1 (symbol where function &rest body)
    `(unwind-protect
        (progn
            (advice-add ,symbol ,where ,function)
            ,@body)
        (advice-remove ,symbol ,function)))

(defmacro with-advice (advice-list &rest body)
    `(apply-split-nest with-advice-1 ,advice-list 3 ,body))


(defmacro with-face-attribute-1 (face attribute value &rest body)
    `(let ((--with-face-attribute-1-- (face-attribute ,face ,attribute)))
        (unwind-protect
            (progn
                (set-face-attribute ,face nil ,attribute ,value)
                ,@body)
            (set-face-attribute ,face nil ,attribute
                --with-face-attribute-1--))))

(defmacro with-face-attribute (face-attribute-list &rest body)
    `(apply-split-nest with-face-attribute-1 ,face-attribute-list 3 ,body))


(defun list-interject (list separator)
    (let ((next list))
        (dotimes (_ (length (cdr list)))
            (setcdr next (cons separator (cdr next)))
            (setq next (cddr next)))))


(defun delete-forward-in-line (start count)
    (delete-region start (save-excursion
        (goto-char start)
        (move-to-column (+ (current-column) count))
        (point))))

(defun delete-lines (start count)
    (save-excursion
        (goto-char start)
        (delete-region (pos-bol) (pos-bol (+ count 1)))))


(defun point-to-register-with-scroll (register)
    (set-register register (list
        (point-marker)
        (window-start)
        (window-vscroll nil t)
        (window-hscroll))))

(defun jump-to-register-with-scroll (register)
    (let ((value (get-register register)))
        (if (and (listp value)
                 (= (length value) 4)
                 (markerp (car value)))
            (let-unpack ((marker start vscroll hscroll) value)
                (register-val-jump-to marker nil)
                (let ((window (selected-window)))
                    (set-window-start window start t)
                    (set-window-vscroll window vscroll t)
                    (set-window-hscroll window hscroll)))
            (register-val-jump-to value nil))))


(defconst pop-to-command-buffer nil)
(make-variable-buffer-local 'pop-to-command-buffer)
(defvar pop-to-command-setup-hook nil)
(defun pop-to-command-buffer-name (type command &optional context name)
    (unless name
        (setq name (concat type ": " (string-join command " "))))
    (if context
        (concat "*" name " (" context ")*")
        (concat "*" name "*")))


(unless (and (fboundp 'package-installed-p)
             (package-installed-p 'use-package))
    (defmacro use-package (&rest _)))

(use-package help
    :config
    (setq help-window-select t)
    (define-key help-map "t" 'describe-face)
    (defun fixed-help-view-source ()
        (set-window-start (selected-window) (point)))
    (advice-add 'help-view-source :after 'fixed-help-view-source)
    (define-key help-mode-map "\C-m" 'help-view-source))

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
        (with-advice ('ring-empty-p :around 'hack-ring-empty-p
                      'ring-remove  :around 'hack-ring-remove)
            (apply eshell-add-input-to-history arguments)))
    (advice-add 'eshell-add-input-to-history :around
        'fixed-eshell-add-input-to-history)
    (defun in-eshell-scrollback-p () (interactive)
        (text-property-any (point) (buffer-end 1) 'field 'prompt))
    (defun hack-insert-before-markers-and-inherit (arguments)
        (if (and (equal (length arguments) 1) (equal (car arguments) ?\n))
            (list (propertize "\n" 'field 'end-of-input 'rear-nonsticky t))
            arguments))
    (defun fixed-eshell-send-input () (interactive)
        (if (in-eshell-scrollback-p)
            (message "not in input")
            (with-advice ('insert-before-markers-and-inherit
                    :filter-args 'hack-insert-before-markers-and-inherit)
                (eshell-send-input))))
    (defun eshell/vi (&rest paths)
        (let ((default-directory-when-invoked default-directory))
            (dolist (path (nreverse (flatten-list paths)))
                (let ((default-directory default-directory-when-invoked))
                    (find-file path)))))
    (put 'eshell/vi 'eshell-no-numeric-conversions t)
    (defun eshell/vo (&rest paths)
        (let ((default-directory-when-invoked default-directory))
            (dolist (path (flatten-list paths))
                (let ((default-directory default-directory-when-invoked))
                    (other-window-prefix)
                    (find-file path)))))
    (put 'eshell/vo 'eshell-no-numeric-conversions t)
    (add-to-list 'eshell-modules-list 'eshell-tramp)
    (defun pop-to-command-eshell (command &optional context name)
        (setq name (pop-to-command-buffer-name "eshell" command context name))
        (let ((program   (car command))
              (arguments (cdr command))
              (buffer    (get-buffer name)))
            (if buffer
                (with-current-buffer buffer
                    (when eshell-process-list
                        (eshell-kill-process)
                        (sleep-for 0.04)))
                (setq buffer (get-buffer-create name))
                (set-buffer buffer)
                (let ((eshell-non-interactive-p t)
                      (eshell-history-file-name nil))
                    (eshell-mode)))
            (pop-to-buffer buffer)
            (setq-local pop-to-command-buffer t)
            (end-of-buffer)
            (run-hooks 'pop-to-command-setup-hook)
            (let ((parsed-command (eshell-parse-command program arguments t)))
                (eshell-eval-command parsed-command))
            buffer))
    (add-hook 'eshell-post-command-hook (lambda ()
        (when pop-to-command-buffer
            (insert "\nCommand " (buffer-name) " done.\n")
            (end-of-buffer)))))
(use-package esh-mode
    :config
    (define-key eshell-mode-map "\C-m" 'fixed-eshell-send-input))

(use-package comint
    :config
    (add-hook 'comint-preoutput-filter-functions (lambda (output)
        (propertize output 'read-only t)))
    (defvar comint-exit-hook nil)
    (defun comint-implement-exit-hook ()
        (let* ((process  (get-buffer-process (current-buffer)))
               (sentinel (process-sentinel process)))
            (set-process-sentinel process `(lambda (process message)
                (unless (process-live-p process)
                    (run-hook-with-args 'comint-exit-hook process))
                (,sentinel process message)))))
    (add-hook 'comint-exec-hook 'comint-implement-exit-hook))

(defun get-field-at-point ()
    (buffer-substring-no-properties (field-beginning) (field-end)))
(defun delete-field-at-point ()
    (delete-region (field-beginning) (field-end)))
(defun replace-field-at-point (new-contents)
    (delete-field-at-point)
    (insert new-contents))

(use-package tramp
    :config
    (setq tramp-default-method "sshx"))

(use-package dired
    :config
    (setq dired-dwim-target t)
    (define-key dired-mode-map "I" 'dired-kill-subdir))

(use-package vc
    :config
    (defun fixed-vc-deduce-backend (vc-deduce-backend &rest arguments)
        (if (or (derived-mode-p 'eshell-mode 'eat-mode)
                (bound-and-true-p with-editor-mode))
            (ignore-errors (vc-responsible-backend default-directory))
            (apply vc-deduce-backend arguments)))
    (advice-add 'vc-deduce-backend :around 'fixed-vc-deduce-backend)
    (add-hook 'vc-annotate-mode-hook (lambda ()
        (setq truncate-lines nil))))

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
        "l" 'undo-tree-visualize-switch-branch-right)
    (defun undo-tree-visualize-swing-left (count) (interactive "p")
        (dotimes (_ count)
            (undo-tree-visualize-undo)
            (undo-tree-visualize-switch-branch-left 1)
            (undo-tree-visualize-redo)))
    (defun undo-tree-visualize-swing-right (count) (interactive "p")
        (dotimes (_ count)
            (undo-tree-visualize-undo)
            (undo-tree-visualize-switch-branch-right 1)
            (undo-tree-visualize-redo))))

(use-package vertico
    :config
    (vertico-mode 1)
    (setq vertico-cycle t))

(use-package consult
    :config
    (setq consult-find-args "find .")
    (defun consult-fd (&optional directory initial-query) (interactive "P")
        (let-unpack ((prompt paths directory)
                         (consult--directory-prompt "Fd" directory))
            (let* ((default-directory directory)
                   (file (consult--find
                             prompt 'consult--fd-builder initial-query)))
                (find-file file))))
    (defun consult--fd-builder (query)
        (let-uncons (patterns options (consult--command-split query)
                     patterns highlight-function (funcall
                         consult--regexp-compiler patterns 'extended t))
            (when patterns
                (list-interject patterns "--and")
                (cons
                    `("fd" "--color=never" "--full-path" ,@patterns ,@options)
                    highlight-function))))
    (defun fixed-consult-history (prefix-argument) (interactive "P")
        (let ((command (when prefix-argument (get-field-at-point))))
            (with-advice ('pos-eol :override 'field-end)
                (save-excursion (save-mutation
                    (end-of-buffer)
                    (when prefix-argument
                        (replace-field-at-point command))
                    (goto-char (field-beginning))
                    (consult-history)
                    (setq command (get-field-at-point)))))
            (end-of-buffer)
            (replace-field-at-point command)
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
    (defun pop-to-command-eat (command &optional context name)
        (setq name (pop-to-command-buffer-name "eat" command context name))
        (let ((program   (car command))
              (arguments (cdr command))
              (buffer    (get-buffer name)))
            (if buffer
                (with-current-buffer buffer
                    (eat-kill-process))
                (setq buffer (get-buffer-create name))
                (set-buffer buffer)
                (eat-mode))
            (pop-to-buffer buffer)
            (setq-local pop-to-command-buffer t)
            (run-hooks 'pop-to-command-setup-hook)
            (eat-exec buffer name program nil arguments)
            buffer))
    (eat-eshell-mode 1)
    (eat-eshell-visual-command-mode 1))

(use-package evil
    :init
    (setq evil-undo-system 'undo-tree)
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1)
    (defun fixed-evil-paste-before (evil-paste-before &rest arguments)
        (evil-save-column
            (apply evil-paste-before arguments)))
    (advice-add 'evil-paste-before :around 'fixed-evil-paste-before)
    (defun fixed-evil-yank-line-handler (yank-handler &rest arguments)
        (evil-save-column
            (apply yank-handler arguments)
            (when (eq (car evil-last-paste) 'evil-paste-after)
                (goto-char (nth 4 evil-last-paste)))))
    (advice-add 'evil-yank-line-handler :around 'fixed-evil-yank-line-handler)
    (defun fixed-evil-yank-block-handler (yank-handler &rest arguments)
        (evil-save-column
            (apply yank-handler arguments))
        (when (eq (car evil-last-paste) 'evil-paste-after)
            (move-to-column (+ (current-column) (nth 4 evil-last-paste)))))
    (advice-add 'evil-yank-block-handler :around 'fixed-evil-yank-block-handler)
    (defun fixed-evil-use-register (return-value)
        (setq prefix-arg current-prefix-arg)
        return-value)
    (advice-add 'evil-use-register :filter-return 'fixed-evil-use-register)
    (define-key evil-motion-state-map "\"" 'evil-use-register)
    (define-key evil-normal-state-map "U" 'evil-redo)
    (define-key evil-motion-state-map "Q" 'quit-previous-window)
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
    (setq mode-line (if (facep 'mode-line-active) 'mode-line-active 'mode-line))
    (defun color-code-vi-state ()
        (if (minibufferp)
            (face-remap-add-relative 'minibuffer-prompt :foreground (cond
                ((evil-normal-state-p)   "#FF0000")
                ((evil-operator-state-p) "#FF8000")
                ((evil-insert-state-p)   "#00FF00")
                ((evil-replace-state-p)  "#FFFF00")
                ((evil-visual-state-p)   "#8080FF")
                ((evil-emacs-state-p)    "#A020FF")
                ((evil-motion-state-p)   "#A0E0FF")
                (t                       "#FFFFFF")))
            (face-remap-add-relative mode-line :background (cond
                ((evil-normal-state-p)   "#FF4040")
                ((evil-operator-state-p) "#FFA060")
                ((evil-insert-state-p)   "#40FF40")
                ((evil-replace-state-p)  "#FFFF80")
                ((evil-visual-state-p)   "#C0C0FF")
                ((evil-emacs-state-p)    "#C040FF")
                ((evil-motion-state-p)   "#80FFFF")
                (t                       "#FFFFFF")))
            (face-remap-add-relative 'mode-line-inactive :background (cond
                ((evil-normal-state-p)   "#6C2424")
                ((evil-operator-state-p) "#6C4824")
                ((evil-insert-state-p)   "#246C24")
                ((evil-replace-state-p)  "#6C6C36")
                ((evil-visual-state-p)   "#24246C")
                ((evil-emacs-state-p)    "#48006C")
                ((evil-motion-state-p)   "#366C6C")
                (t                       "#202020")))))
    (add-hook 'evil-normal-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-operator-state-entry-hook 'color-code-vi-state)
    (add-hook 'evil-insert-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-replace-state-entry-hook  'color-code-vi-state)
    (add-hook 'evil-visual-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-emacs-state-entry-hook    'color-code-vi-state)
    (add-hook 'evil-motion-state-entry-hook   'color-code-vi-state)
    (setq evil-insert-state-cursor '(bar . 3))
    (set-face-foreground 'mode-line "#010101")
    (setq evil-cross-lines t)
    (defun fixed-evil-goto-line (evil-goto-count &optional count)
        (if count
            (funcall evil-goto-count count)
            (let* ((end   (point-max))
                   (end-1 (- end 1))
                   (last  (buffer-substring-no-properties end-1 end)))
                (evil-ensure-column
                    (if (equal last "\n")
                        (goto-char end-1)
                        (goto-char end))))))
    (advice-add 'evil-goto-line :around 'fixed-evil-goto-line)
    (setq evil-move-beyond-eol t)
    (defun fixed-evil-end-of-line (evil-end-of-line &rest arguments)
        (let ((evil-move-beyond-eol nil))
            (apply evil-end-of-line arguments)))
    (advice-add 'evil-end-of-line :around 'fixed-evil-end-of-line)
    (setq evil-move-cursor-back nil)
    (defun fixed-evil-paste-after (evil-paste-after &rest arguments)
        (let ((evil-move-cursor-back t))
            (apply evil-paste-after arguments)))
    (advice-add 'evil-paste-after :around 'fixed-evil-paste-after)
    (define-key evil-replace-state-map [escape] 'evil-insert-state)
    (define-key evil-motion-state-map "\C-m" nil)
    (define-key evil-normal-state-map "\C-?" nil)
    (define-key evil-normal-state-map "\C-r" nil)
    (define-key evil-motion-state-map [escape] 'quit-window)
    (define-key evil-operator-state-map [escape] 'evil-force-normal-state)
    (evil-define-command evil-replacing-paste-before (count &optional register)
        (interactive "*P<x>")
        (evil-replacing-paste count register nil))
    (evil-define-command evil-replacing-paste-after (count &optional register)
        (interactive "*P<x>")
        (evil-replacing-paste count register t))
    (defvar hack-evil-last-paste)
    (defun hack-evil-set-marker (character &rest _)
        (when (equal character ?\[)
            (setq hack-evil-last-paste evil-last-paste)))
    (defvar evil-replacing-paste--line-or-block)
    (defun hack-evil-yank-line-handler (&rest _)
        (setq evil-replacing-paste--line-or-block 'line))
    (defun hack-evil-yank-block-handler (&rest _)
        (setq evil-replacing-paste--line-or-block 'block))
    (defun evil-replacing-paste (count register move-point)
        (evil-with-single-undo
            (setq evil-replacing-paste--line-or-block nil)
            (with-advice ('evil-set-marker :before 'hack-evil-set-marker
                          'evil-yank-line-handler
                              :before 'hack-evil-yank-line-handler
                          'evil-yank-block-handler
                              :before 'hack-evil-yank-block-handler)
                (evil-paste-before count register))
            (when (eq evil-replacing-paste--line-or-block nil)
                (let* ((end     (nth 4 hack-evil-last-paste))
                       (columns (- end (nth 3 hack-evil-last-paste))))
                    (delete-forward-in-line end columns)
                    (when move-point
                        (goto-char end))))
            (when (eq evil-replacing-paste--line-or-block 'line)
                (let* ((end   (nth 4 hack-evil-last-paste))
                       (lines (count-lines (nth 3 hack-evil-last-paste) end)))
                    (delete-lines end lines)
                    (when move-point
                        (evil-save-column
                            (goto-char end)))))
            (when (eq evil-replacing-paste--line-or-block 'block)
                (let ((lines   (nth 3 hack-evil-last-paste))
                      (columns (nth 4 hack-evil-last-paste)))
                    (dotimes (line lines)
                        (save-excursion
                            (evil-save-repeat-info
                                (evil-next-line line))
                            (move-to-column (+ (current-column) columns))
                            (delete-forward-in-line (point) columns)))
                    (when move-point
                        (move-to-column (+ (current-column) columns)))))
            (setq evil-last-paste nil)))
    (define-key evil-normal-state-map "gp" 'evil-replacing-paste-after)
    (define-key evil-normal-state-map "gP" 'evil-replacing-paste-before)
    (setq evil-motion-state-modes (append
        evil-motion-state-modes evil-emacs-state-modes))
    (setq evil-emacs-state-modes nil)
    (advice-add 'evil-quit :around (lambda (evil-quit &rest arguments)
        (with-advice ('delete-window :override 'kill-current-buffer)
            (apply evil-quit arguments))))
    (define-prefix-command 'space-map)
    (define-key evil-motion-state-map " " 'space-map)
    (define-key evil-motion-state-map "\C-@" 'space-map)
    (define-key evil-motion-state-map [?\C- ] 'space-map)
    (define-key evil-insert-state-map "\C-@" 'space-map)
    (define-key evil-insert-state-map [?\C- ] 'space-map)
    (define-key evil-replace-state-map "\C-@" 'space-map)
    (define-key evil-replace-state-map [?\C- ] 'space-map)
    (define-key evil-emacs-state-map "\C-@" 'space-map)
    (define-key evil-emacs-state-map [?\C- ] 'space-map)
    (defun fixed-last-s-expression (function &rest arguments)
        (save-excursion
            (condition-case _error
                (forward-char)
                (end-of-buffer nil))
            (apply function arguments)))
    (use-package elisp-mode
        :config
        (advice-remove 'elisp--preceding-sexp 'evil)
        (advice-add 'elisp--preceding-sexp :around 'fixed-last-s-expression))
    (setq evil-highlight-closing-paren-at-point-states '(not))
    (setq evil-show-paren-range 1)
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
        (evil-local-set-key 'motion [escape] 'undo-tree-visualizer-quit)
        (evil-local-set-key 'motion "H" 'undo-tree-visualize-swing-left)
        (evil-local-set-key 'motion "L" 'undo-tree-visualize-swing-right)))
    (define-key evil-motion-state-map " b" 'switch-to-buffer)
    (define-key evil-motion-state-map " B" 'ibuffer)
    (define-key evil-motion-state-map " k" 'kill-buffer)
    (define-key evil-motion-state-map " f" 'find-file)
    (define-key evil-motion-state-map " F" 'find-alternate-file)
    (define-key evil-motion-state-map " r" 'revert-buffer)
    (define-key evil-motion-state-map " d" 'dired)
    (add-to-list 'evil-motion-state-modes 'dired-mode)
    (define-key evil-motion-state-map " y" 'execute-extended-command)
    (define-key evil-motion-state-map " ," 'eval-expression)
    (define-key evil-motion-state-map " h" help-map)
    (define-key help-mode-map "r" 'help-go-back)
    (define-key help-mode-map "R" 'help-go-forward)
    (define-key help-mode-map "K" 'help-go-back)
    (define-key help-mode-map "J" 'help-go-forward)
    (define-key evil-motion-state-map " p" 'list-packages)
    (add-to-list 'evil-motion-state-modes 'package-menu-mode)
    (define-key evil-motion-state-map " n" 'universal-argument)
    (define-key universal-argument-map " n" 'universal-argument-more)
    (define-key universal-argument-map [escape] 'ignore)
    (define-key evil-normal-state-map " q" 'ignore)
    (define-key evil-normal-state-map [?  escape] 'ignore)
    (define-key evil-operator-state-map "q" 'evil-force-normal-state)
    (define-key evil-normal-state-map "q" nil)
    (define-key evil-motion-state-map " M" 'evil-record-macro)
    (define-key evil-motion-state-map " Q" 'delete-frame)
    (define-key evil-motion-state-map " !" 'save-buffers-kill-emacs)
    (define-key evil-motion-state-map "K" nil)
    (define-key evil-motion-state-map " L" 'evil-lookup)
    (define-key evil-motion-state-map " w" 'same-window-prefix)
    (define-key evil-motion-state-map " W" 'other-window-prefix)
    (defun consult-history-execute (prefix-argument) (interactive "P")
        (let* ((consult-history-execute t)
               (command (fixed-consult-history prefix-argument))
               (run-key (listify-key-sequence "\C-m")))
            (evil-repeat-start)
            (add-to-list 'evil-repeat-info `((lambda ()
                (end-of-buffer)
                (replace-field-at-point ,command)
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
        (evil-local-set-key 'normal [escape] 'eshell-interrupt-process)
        (evil-local-set-key 'insert [escape] 'evil-eshell-force-normal-state)
        (evil-local-set-key 'operator [escape] 'evil-force-normal-state)))
    (add-hook 'eshell-pre-command-hook (lambda ()
        (setq evil-eshell-state-for-next-input evil-state)
        (evil-insert-state)))
    (add-hook 'eshell-post-command-hook (lambda ()
        (evil-force-normal-state)
        (evil-change-state evil-eshell-state-for-next-input)))
    (evil-declare-not-repeat 'fixed-eshell-send-input)
    (evil-declare-not-repeat 'eshell-interrupt-process)
    (define-key evil-motion-state-map " T" 'eat)
    (add-to-list 'evil-emacs-state-modes 'eat-mode)
    (add-hook 'eat-exit-hook (lambda (_process) (evil-normal-state nil)))
    (add-hook 'eat-mode-hook (lambda ()
        (evil-local-set-key 'emacs "\C-[" 'eat-self-input)))
    (define-key eat-semi-char-mode-map "\C-c\C-z" 'eat-self-input)
    (add-hook 'comint-exit-hook (lambda (_process) (evil-normal-state nil)))
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
    (define-key evil-motion-state-map " #" 'display-line-numbers-mode)
    (defun cycle-line-wrap () (interactive)
        (if truncate-lines
            (setq truncate-lines nil)
            (visual-line-mode 'toggle)
            (if (not visual-line-mode)
                (setq truncate-lines t)))
        (message "visual-line-mode: %s truncate-lines: %s"
            visual-line-mode truncate-lines))
    (define-key evil-motion-state-map " ;" 'cycle-line-wrap)
    (define-key evil-motion-state-map " l" 'consult-line)
    (define-key evil-motion-state-map " sl" 'consult-ripgrep)
    (define-key evil-motion-state-map " sf" 'consult-fd)
    (define-key evil-motion-state-map " x" 'tramp-cleanup-connection)
    (add-hook 'pop-to-command-setup-hook (lambda ()
        (evil-initialize-state)
        (evil-local-set-key 'normal "q" 'quit-window)
        (evil-local-set-key 'normal [escape] 'quit-window)))
    (defun diff-unsaved-changes () (interactive)
        (if (not buffer-file-name)
            (diff-unsaved-changes--eshell
                "echo" (concat (buffer-name) " is not visiting a file"))
            (with-temporary-directory directory
                (let ((file    (concat directory "/file"))
                    (unsaved (concat directory "/unsaved"))
                    (default-directory "~"))
                    (copy-file buffer-file-name file)
                    (write-region (buffer-end -1) (buffer-end 1) unsaved)
                    (diff-unsaved-changes--eshell
                        "cdexec" directory "git" "diff" "file" "unsaved"))
                (sleep-for 0.2))))
    (defun diff-unsaved-changes--eshell (&rest command)
        (pop-to-command-eshell command (buffer-name) "Diff unsaved"))
    (define-key evil-motion-state-map " c" 'diff-unsaved-changes)
    (defmacro git (&rest arguments)
        (let ((command (cons "git" (mapcar 'symbol-name arguments))))
            `(lambda () (interactive)
                (let ((default-directory (vc-root-dir)))
                    (pop-to-command-eshell ',command default-directory)))))
    (defmacro git-on-current-file (&rest arguments)
        (let ((command (cons "git" (mapcar 'symbol-name arguments))))
            `(lambda () (interactive)
                (let ((default-directory (vc-root-dir))
                      (command           '(,@command)))
                    (setq command (append command (list buffer-file-name)))
                    (pop-to-command-eshell command default-directory)))))
    (define-key evil-motion-state-map " vv" (git status))
    (define-key evil-motion-state-map " vl" (git log))
    (define-key evil-motion-state-map " vL" (git log -p))
    (define-key evil-motion-state-map " vo" (git reflog))
    (define-key evil-motion-state-map " vp" (git stash list -p))
    (define-key evil-motion-state-map " vd" (git diff))
    (define-key evil-motion-state-map " vs" (git diff --staged))
    (define-key evil-motion-state-map " va" (git add -p))
    (define-key evil-motion-state-map " vA" (git-on-current-file add -p))
    (define-key evil-motion-state-map " vq" (git checkout -p))
    (define-key evil-motion-state-map " vQ" (git-on-current-file checkout -p))
    (define-key evil-motion-state-map " vw" (git reset -p))
    (define-key evil-motion-state-map " vW" (git-on-current-file reset -p))
    (define-key evil-motion-state-map " ve" (git stash -p))
    (define-key evil-motion-state-map " vE" (git-on-current-file stash -p))
    (define-key evil-motion-state-map " vu" (git pull))
    (define-key evil-motion-state-map " vy" (git push))
    (define-key evil-motion-state-map " vY" (git push --force))
    (define-key evil-motion-state-map " vt" (git push --tags))
    (define-key evil-motion-state-map " vT" (git push --tags --force))
    (define-key evil-motion-state-map " vc" (git commit))
    (define-key evil-motion-state-map " vC" (git commit --amend))
    (define-key evil-motion-state-map " vb" 'vc-annotate)
    (define-key evil-motion-state-map " zy" (lambda () (interactive)
        (let ((default-directory "~/Downloads"))
            (pop-to-command-eshell
                '("sh" "-c" "yt-dlp -f bestaudio \"`p`\"") nil "yt-dlp"))))
    (define-key evil-motion-state-map " zm" (lambda () (interactive)
        (let ((default-directory "~"))
            (pop-to-command-eshell
                '("termux-media-scan" "/storage/emulated/0")))))
    (evil-define-key 'motion doc-view-mode-map "j"
        'doc-view-next-line-or-next-page)
    (evil-define-key 'motion doc-view-mode-map "k"
        'doc-view-previous-line-or-previous-page)
    (evil-define-key 'motion doc-view-mode-map "l" 'image-forward-hscroll)
    (evil-define-key 'motion doc-view-mode-map "h" 'image-backward-hscroll)
    (evil-define-key 'motion doc-view-mode-map "\C-d"
        'doc-view-scroll-up-or-next-page)
    (evil-define-key 'motion doc-view-mode-map "\C-u"
        'doc-view-scroll-down-or-previous-page)
    (evil-define-key 'motion doc-view-mode-map "gg" 'doc-view-first-page)
    (defun evil-doc-view-goto-page (prefix-argument) (interactive "P")
        (if prefix-argument
            (doc-view-goto-page prefix-argument)
            (doc-view-last-page)))
    (evil-define-key 'motion doc-view-mode-map "G" 'evil-doc-view-goto-page)
    (evil-define-key 'motion doc-view-mode-map "-" 'doc-view-shrink)
    (evil-define-key 'motion doc-view-mode-map [down]
        'doc-view-next-line-or-next-page)
    (evil-define-key 'motion doc-view-mode-map [up]
        'doc-view-previous-line-or-previous-page)
    (evil-define-key 'motion doc-view-mode-map [right] 'image-forward-hscroll)
    (evil-define-key 'motion doc-view-mode-map [left] 'image-backward-hscroll)
    (evil-define-motion evil-vertico-next-line (count)
        (unless count
            (setq count 1))
        (dotimes (_ count)
            (condition-case _error
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
        (evil-local-set-key 'normal "j"    'evil-vertico-next-line)
        (evil-local-set-key 'normal [down] 'evil-vertico-next-line)
        (evil-local-set-key 'normal "k"    'evil-vertico-previous-line)
        (evil-local-set-key 'normal [up]   'evil-vertico-previous-line)
        (let ((quit (if (eq this-command 'consult-history-execute)
                         'consult-history-execute-quit
                         'abort-minibuffers)))
            (evil-local-set-key 'normal   [escape] quit)
            (evil-local-set-key 'normal   "q"      quit)
            (evil-local-set-key 'operator [escape] 'evil-force-normal-state)
            (evil-local-set-key 'operator "q"      'evil-force-normal-state))))
    (add-hook 'isearch-mode-hook (lambda ()
        (define-key overriding-terminal-local-map [escape]
            (lambda () (interactive)
                (isearch-done)))))
    (add-hook 'Info-mode-hook (lambda ()
        (evil-local-set-key 'motion " " 'space-map)))
    (evil-declare-not-repeat 'ignore)
    (add-to-list 'evil-motion-state-modes 'shortdoc-mode)
    (with-current-buffer (messages-buffer)
        (evil-force-normal-state)))

(use-package auto-dim-other-buffers
    :config
    (set-face-background 'auto-dim-other-buffers-face "#202020")
    (defun hack-window-list (window-list &optional frame _minibuffer window)
        (funcall window-list frame t window))
    (defun hack-adob--rescan-windows (adob--rescan-windows &rest arguments)
        (with-advice ('window-list :around 'hack-window-list)
            (apply adob--rescan-windows arguments)))
    (advice-add 'adob--rescan-windows :around 'hack-adob--rescan-windows)
    (defun hack-window-minibuffer-p (&optional _window)
        nil)
    (defun hack-adob--update (adob--update &rest arguments)
        (with-advice ('window-minibuffer-p :override 'hack-window-minibuffer-p)
            (apply adob--update arguments)))
    (advice-add 'adob--update :around 'hack-adob--update)
    (defun hack-adob--focus-out-hook (adob--focus-out-hook &rest arguments)
        (with-advice ('window-minibuffer-p :override 'hack-window-minibuffer-p)
            (apply adob--focus-out-hook arguments)))
    (advice-add 'adob--focus-out-hook :around 'hack-adob--focus-out-hook)
    (with-current-buffer " *Minibuf-0*" (insert ?\t))
    (add-hook 'after-change-functions (lambda (&rest _)
        (with-current-buffer " *Minibuf-0*"
            (when (< (buffer-size) 1)
                (insert ?\t)))))
    (define-key evil-motion-state-map " \\" (lambda () (interactive)
        (with-current-buffer " *Minibuf-0*" (insert ?\t))))
    (get-buffer-create " *Echo Area 0*")
    (get-buffer-create " *Echo Area 1*")
    (auto-dim-other-buffers-mode 1))

(use-package ace-window
    :config
    (setq aw-dispatch-when-more-than 1)
    (setq aw-scope 'frame)
    (setq aw-make-frame-char nil)
    (setq aw-leading-char-style 'path)
    (defun fixed-aw--make-background (window)
        (let ((buffer (window-buffer window)))
            (with-current-buffer buffer
                (let ((overlay (make-overlay 1 (buffer-size) buffer nil t)))
                    (overlay-put overlay 'face 'aw-background-face)
                    overlay))))
    (defun fixed-aw--make-backgrounds (window-list)
        (setq aw-overlays-back
            (mapcar 'fixed-aw--make-background window-list)))
    (advice-add 'aw--make-backgrounds :override 'fixed-aw--make-backgrounds)
    (defun fixed-aw--done (&rest _)
        (setq aw-empty-buffers-list
            (seq-filter 'buffer-live-p aw-empty-buffers-list)))
    (advice-add 'aw--done :before 'fixed-aw--done)
    (defun aw-window< (window-1 window-2)
        (cond
            ((window-minibuffer-p window-1) t)
            ((window-minibuffer-p window-2) nil)
            (t
                (let ((window-1-edges (window-edges window-1))
                      (window-2-edges (window-edges window-2)))
                    (let ((window-1-left-edge (car window-1-edges))
                          (window-1-top-edge (cadr window-1-edges))
                          (window-2-left-edge (car window-2-edges))
                          (window-2-top-edge (cadr window-2-edges)))
                        (if (= window-1-top-edge window-2-top-edge)
                            (< window-1-left-edge window-2-left-edge)
                            (< window-1-top-edge window-2-top-edge)))))))
    (defun hack-aw-window-list (window-list)
        (if (= (length window-list) 1)
            (cons (car window-list) window-list)
            window-list))
    (defun hack-avy-tree (avy-tree candidate-list key-list)
        (advice-remove 'aw-window-list 'hack-aw-window-list)
        (advice-remove 'avy-tree 'hack-avy-tree)
        (let* ((first-candidate (car candidate-list))
               (first-window    (cdr first-candidate)))
            (when (eq first-window (cdadr candidate-list))
                (setq candidate-list (cdr candidate-list)))
            (if (window-minibuffer-p first-window)
               (let ((tree (funcall avy-tree (cdr candidate-list) key-list)))
                    (cons `(?0 leaf . ,first-candidate) tree))
                (funcall avy-tree candidate-list key-list))))
    (defun fixed-aw-select (action)
        (with-advice ('aw-window-list :filter-return 'hack-aw-window-list
                      'avy-tree :around 'hack-avy-tree)
            (aw-select "" action)))
    (defvar window-state nil)
    (defconst window-state-normal
        '("#00FF00" "#606060" "#202020" "W" "Window state"))
    (defconst window-state-target-pending
        '("#00FF00" "#906030" "#302010" "T" "Target-pending window state"))
    (defvar window-state-this-register nil)
    (defvar window-state-last-search-target nil)
    (defvar window-state--action nil)
    (defvar window-state--execute-once nil)
    (defvar window-state--execute-more nil)
    (defun window-state--execute ()
        (evil-echo "-- WINDOW --")
        (setq window-state-this-register evil-this-register)
        (while window-state--execute-once
            (setq window-state--execute-once window-state--execute-more)
            (let-unpack ((hint tint dim tag help-string) window-state)
                (with-face-attribute (
                        'aw-leading-char-face        :foreground hint
                        'aw-background-face          :foreground tint
                        'auto-dim-other-buffers-face :background dim)
                    (save-override-evil-mode-line-tag tag help-string
                        (fixed-aw-select 'window-state--do-action))))))
    (defun window-state--do-action (window)
        (funcall window-state--action window)
        (setq window-state-this-register nil))
    (defun window-state--normal ()
        (setq window-state--action 'aw-switch-to-window)
        (setq window-state window-state-normal))
    (defun window-state-for-one-command () (interactive)
        (window-state--normal)
        (setq window-state--execute-once t)
        (setq window-state--execute-more nil)
        (window-state--execute)
        (setq evil-inhibit-operator t))
    (defun window-state () (interactive)
        (window-state--normal)
        (setq window-state--execute-once t)
        (setq window-state--execute-more t)
        (window-state--execute)
        (setq evil-inhibit-operator t))
    (defun window-state-quit ()
        (if (eq window-state window-state-normal)
            (setq window-state--execute-once nil)
            (window-state--normal)
            (setq window-state--execute-once t)))
    (defmacro window-state-define-motion (name &rest body)
        `(defun ,name ()
            (window-state--do-action
                (condition-case error
                    (save-selected-window
                        ,@body
                        (selected-window))
                (error
                    (message "%s" (error-message-string error))
                    (selected-window))))))
    (window-state-define-motion window-state-move-up
        (windmove-up))
    (window-state-define-motion window-state-move-down
        (windmove-down))
    (window-state-define-motion window-state-move-left
        (windmove-left))
    (window-state-define-motion window-state-move-right
        (windmove-right))
    (window-state-define-motion window-state-split-up
        (evil-window-split))
    (window-state-define-motion window-state-split-down
        (let ((evil-split-window-below t))
            (evil-window-split)))
    (window-state-define-motion window-state-split-left
        (evil-window-vsplit))
    (window-state-define-motion window-state-split-right
        (let ((evil-vsplit-window-right t))
            (evil-window-vsplit)))
    (defun window-state-toggle-auto-balance ()
        (setq evil-auto-balance-windows (not evil-auto-balance-windows))
        (message "evil-auto-balance-windows: %s" evil-auto-balance-windows)
        (setq window-state--execute-once t))
    (defmacro window-state-define-operator (name &rest body)
        (let ((operator-name (intern (concat (symbol-name name) "-operator"))))
            `(cons
                (defun ,name (&optional window)
                    (unless window
                        (setq window (selected-window)))
                    (with-selected-window window
                        ,@body)
                    (window-state--normal))
                (defun ,operator-name ()
                    (cond
                        ((eq window-state--action ',name)
                            (,name))
                        ((eq window-state--action 'aw-switch-to-window)
                            (setq window-state--action ',name)
                            (setq window-state window-state-target-pending)
                            (setq window-state--execute-once t))
                        (t
                            (keyboard-quit)))))))
    (window-state-define-operator window-state-yank
        (point-to-register-with-scroll (or window-state-this-register ?0)))
    (window-state-define-operator window-state-delete
        (point-to-register-with-scroll (or window-state-this-register ?0))
        (evil-window-delete))
    (window-state-define-operator window-state-paste
        (with-advice ('push-mark :override 'ignore)
            (jump-to-register-with-scroll (or window-state-this-register ?0))))
    (defun window-state--swap-buffer (window)
        (save-selected-window
            (aw-swap-window window)))
    (defun window-state-swap ()
        (setq window-state--action 'window-state--swap-buffer)
        (setq window-state window-state-target-pending)
        (setq window-state--execute-once t))
    (defun window-state-swap-move ()
        (setq window-state--action 'aw-swap-window)
        (setq window-state window-state-target-pending)
        (setq window-state--execute-once t))
    (window-state-define-operator window-state-change
        (condition-case _error
            (call-interactively 'switch-to-buffer)
            (quit)))
    (window-state-define-operator window-state-open
        (condition-case _error
            (call-interactively 'find-file)
            (quit)))
    (window-state-define-operator window-state-target-window-prefix
        (display-buffer-override-next-command
            `(lambda (&rest _)
                (cons ,(selected-window) 'reuse))
            nil
            "[target-window]")
        (message "Display next command buffer in target window..."))
    (window-state-define-operator window-state-bury
        (bury-buffer))
    (window-state-define-operator window-state-unbury
        (switch-to-buffer (last-buffer nil t)))
    (window-state-define-operator window-state-kill
        (kill-buffer))
    (window-state-define-operator window-state-ex
        (condition-case _error
            (evil-ex)
            (quit)))
    (window-state-define-operator window-state-vi-search-forward
        (condition-case _error
            (progn
                (evil-search-forward)
                (setq window-state-last-search-target (selected-window)))
            (quit)))
    (window-state-define-operator window-state-vi-search-backward
        (condition-case _error
            (progn
                (evil-search-backward)
                (setq window-state-last-search-target (selected-window)))
            (quit)))
    (defun window-state-vi-search-next (&optional window)
        (unless window
            (setq window window-state-last-search-target))
        (with-selected-window window
            (evil-search-next)))
    (defun window-state-vi-search-previous (&optional window)
        (unless window
            (setq window window-state-last-search-target))
        (with-selected-window window
            (evil-search-previous)))
    (defun window-state-use-register ()
        (condition-case _error
            (setq window-state-this-register
                (call-interactively 'evil-use-register))
            (quit))
        (setq window-state--execute-once t))
    (defun window-state-passthrough (character)
        (list character `(lambda ()
            (setq window-state--execute-once nil)
            (setq unread-command-events '(,character)))))
    (setq aw-dispatch-alist `(
        (?h window-state-move-left)
        (?j window-state-move-down)
        (?k window-state-move-up)
        (?l window-state-move-right)
        (?H window-state-split-left)
        (?J window-state-split-down)
        (?K window-state-split-up)
        (?L window-state-split-right)
        (?a window-state-toggle-auto-balance)
        (?y window-state-yank-operator)
        (?Y window-state-yank)
        (?d window-state-delete-operator)
        (?D window-state-delete)
        (?p window-state-paste-operator)
        (?P window-state-paste)
        (?x window-state-swap)
        (?X window-state-swap-move)
        (?c window-state-change-operator)
        (?C window-state-change)
        (?o window-state-open-operator)
        (?O window-state-open)
        (?w window-state-target-window-prefix-operator)
        (?W window-state-target-window-prefix)
        (?f window-state-bury-operator)
        (?F window-state-bury)
        (?t window-state-unbury-operator)
        (?T window-state-unbury)
        (?r window-state-kill-operator)
        (?R window-state-kill)
        (?: window-state-ex-operator)
        (?/ window-state-vi-search-forward-operator)
        (?? window-state-vi-search-backward-operator)
        (?n window-state-vi-search-next)
        (?N window-state-vi-search-previous)
        (?\" window-state-use-register)
        ,(window-state-passthrough ? )
        (?q window-state-quit)
        (?\C-\[ window-state-quit)))
    (setq aw-dispatch-function (lambda (character)
        (if (and (equal character ??) (not (alist-get ?? aw-dispatch-alist)))
            (message "No such candidate: ?, hit `C-g' to quit.")
            (if (equal character ?\C-g)
                (aw-dispatch-default ?q)
                (aw-dispatch-default character)))))
    (define-key evil-motion-state-map " o" 'window-state-for-one-command)
    (define-key evil-motion-state-map " O" 'window-state)
    (define-key evil-motion-state-map "s" 'window-state-for-one-command)
    (define-key evil-normal-state-map "s" nil)
    (define-key evil-motion-state-map "S" 'window-state)
    (define-key evil-normal-state-map "S" nil))

(use-package with-editor
    :config
    (defun fixed-with-editor-return (with-editor-return cancel)
        (with-advice ('delete-file :override 'ignore
                      (if cancel 'save-buffer nil) :override 'ignore)
            (funcall with-editor-return cancel)))
    (advice-add 'with-editor-return :around 'fixed-with-editor-return)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)
    (shell-command-with-editor-mode 1))

(use-package rainbow-mode
    :config
    (define-key evil-motion-state-map " R" 'rainbow-mode))


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


(setq initial-buffer-choice 'eshell)
