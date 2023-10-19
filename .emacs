(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)


(menu-bar-mode -1)
(defun init-graphic-frame (frame)
    (if (not (display-graphic-p frame))
        (set-face-background 'default "#202020")
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (select-frame frame)
        (set-frame-font "DejaVu Sans Mono-13")
        (set-background-color "#202020")
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
    (defun fixed-browse-url-xdg-open (url &optional _unused)
        (message "opening %s" url)
        (browse-url-xdg-open url))
    (setq browse-url-browser-function 'fixed-browse-url-xdg-open)
    (setq woman-manpath '("~/../usr/share/man")))


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


(defmacro lambda-let (varlist &rest body)
    `(lambda-partial-let ,varlist () ,@body))

(defmacro lambda-partial-let (varlist args &rest body)
    (let (parameters parameter arguments argument)
        (dolist (var varlist)
            (if (listp var)
                (if (length= var 2)
                    (unpack (parameter argument) var)
                    (if (length= var 1)
                        (setq parameter (setq argument (car var)))
                        (eval `(let (,var)))))
                (setq parameter (setq argument var)))
            (setq parameters (cons parameter parameters))
            (setq arguments (cons argument arguments)))
        (dolist (arg args)
            (setq parameters (cons arg parameters)))
        (setq parameters (nreverse parameters))
        (setq arguments (nreverse arguments))
        `(apply-partially
            (lambda ,parameters
                ,@body)
            ,@arguments)))


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


(defmacro with-buffer-modified-p (flag &rest body)
    `(let ((--with-buffer-modified-p-- (buffer-modified-p)))
        (unwind-protect
            (progn
                (set-buffer-modified-p ,flag)
                ,@body)
            (set-buffer-modified-p --with-buffer-modified-p--))))


(defmacro with-visited-file-modtime (time-flag &rest body)
    `(let ((--with-visited-file-modtime-- (visited-file-modtime)))
        (unwind-protect
            (progn
                (set-visited-file-modtime ,time-flag)
                ,@body)
            (set-visited-file-modtime --with-visited-file-modtime--))))


(defun buffer-differs-from-visited-file-p ()
    (with-visited-file-modtime '(0 0)
        (let ((differs nil))
            (with-advice ('ask-user-about-supersession-threat
                             :override (lambda (_) (setq differs t)))
                (with-buffer-modified-p nil
                    (set-buffer-modified-p t)
                    differs)))))


(defun refresh-modified-state (&optional buffer) (interactive)
    (unless buffer
        (setq buffer (current-buffer)))
    (with-current-buffer buffer
        (let ((differs (buffer-differs-from-visited-file-p)))
            (with-advice ('ask-user-about-supersession-threat
                              :override (lambda (_)))
                (set-buffer-modified-p differs)))
        (set-visited-file-modtime)))


(defun dlist-cons (item dlist)
    (let ((entry (cons
                     (cons nil  dlist)
                     (cons item (cdr dlist)))))
        (when dlist
            (setcar (car dlist) entry))
        entry))

(defun dlist (&rest items)
    (let ((dlist nil))
        (dolist (item (reverse items) dlist)
            (setq dlist (dlist-cons item dlist)))))

(defun dlist-car (dlist)
    (cadr dlist))

(defun dlist-cdr (dlist)
    (cdar dlist))

(defun dlist-nthcdr (n dlist)
    (dotimes (_ n dlist)
        (setq dlist (dlist-cdr dlist))))

(defun dlist-nth (n dlist)
    (dlist-car (dlist-nthcdr n dlist)))

(defun dlist-setcar (dlist newcar)
    (setcar (cdr dlist) newcar))

(defun dlist-setcdr (dlist newcdr)
    (setcdr (car dlist) newcdr)
    (setcdr (cdr dlist) (cdr newcdr))
    (when newcdr
        (setcar (car newcdr) dlist))
    newcdr)

(defun dlist-unlink (link)
    (let-uncons (headward-link tailward-link (car link))
        (when tailward-link
            (setcar (car tailward-link) headward-link))
        (when headward-link
            (setcdr (car headward-link) tailward-link)
            (setcdr (cdr headward-link) (cdr tailward-link))))
    nil)


(defun make-ordered-hash-table (&rest arguments)
    (list (apply 'make-hash-table arguments)))

(defun ordered-hash-table-get (table key &optional default)
    (if-let (entry (gethash key (car table)))
        (dlist-car entry)
        default))

(defun ordered-hash-table-pop (table key)
    (let ((hash-table (car table)))
        (when-let (entry (gethash key hash-table))
            (remhash key hash-table)
            (when (eq entry (cdr table))
                (setcdr table (dlist-cdr entry)))
            (dlist-unlink entry)
            (dlist-car entry))))

(defun ordered-hash-table-put (table key value)
    (ordered-hash-table-pop table key)
    (let ((entry (dlist-cons value (cdr table))))
        (puthash key entry (car table))
        (setcdr table entry))
    value)


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


(defun repeat-string (string count)
    (string-join (make-list count string)))


(defun point-marker-with-scroll ()
    (list
        (point-marker)
        (window-start)
        (window-vscroll nil t)
        (window-hscroll)))

(defun jump-to-marker-with-scroll (marker-with-scroll)
    (let-unpack ((marker start vscroll hscroll) marker-with-scroll)
        (register-val-jump-to marker nil)
        (let ((window (selected-window)))
            (set-window-start window start t)
            (set-window-vscroll window vscroll t)
            (set-window-hscroll window hscroll))))


(defun read-key-sequence-in-keymap (keymap &rest arguments)
    (let ((overriding-terminal-local-map nil)
          (overriding-local-map keymap)
          (saved-global-map (current-global-map)))
        (unwind-protect
            (progn
                (use-global-map (make-sparse-keymap))
                (apply 'read-key-sequence arguments))
            (use-global-map saved-global-map))))


(defun identity+ignore (value &rest _)
    value)

(defun ignore+return (value)
    (apply-partially 'identity+ignore value))


(defun field-string-no-properties (&optional position)
    (substring-no-properties (field-string position)))
(defun replace-field (new-contents &optional position)
    (delete-field position)
    (if position
        (save-excursion
            (goto-char position)
            (insert new-contents))
        (insert new-contents)
        (goto-char (+ (point) (length new-contents)))))


(defvar command-at-point-mode-alist nil)
(defun command-string (&optional position)
    (let ((functions (alist-get major-mode command-at-point-mode-alist)))
        (if-let ((function (nth 0 functions)))
            (funcall function position)
            (field-string-no-properties position))))
(defun delete-command (&optional position)
    (let ((functions (alist-get major-mode command-at-point-mode-alist)))
        (if-let ((function (nth 1 functions)))
            (funcall function position)
            (delete-field position))))
(defun replace-command (new-command &optional position)
    (let ((functions (alist-get major-mode command-at-point-mode-alist)))
        (if-let ((function (nth 2 functions)))
            (funcall function new-command position)
            (replace-field new-command position))))


(defun buffer-process-send-string (string)
    (process-send-string (get-buffer-process (current-buffer)) string))


(defconst pop-to-command-buffer nil)
(make-variable-buffer-local 'pop-to-command-buffer)
(defvar pop-to-command-setup-hook nil)
(defvar pop-to-command--callback nil)
(make-variable-buffer-local 'pop-to-command--callback)
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

(defun make-histdir-history ()
    (let ((table (make-ordered-hash-table :test 'eq)))
        (vector table nil nil nil)))
(defun histdir-history-table (history)
    (aref history 0))
(defun histdir-history--last (history)
    (aref history 1))
(defun histdir-history--set-last (history new-last)
    (aset history 1 new-last))
(defun histdir-history-buffers (history)
    (aref history 2))
(defun histdir-history--register-buffer (history buffer)
    (let ((buffers (aref history 2)))
        (unless (memq buffer buffers)
            (aset history 2 (cons buffer buffers)))))
(defun histdir-history-watches (history)
    (aref history 3))
(defun histdir-history--set-watches (history descriptors)
    (aset history 3 descriptors))
(defvar histdir-buffer-local-history-list nil)
(make-variable-buffer-local 'histdir-buffer-local-history-list)
(defun histdir--update-buffer-local-history-pointers (history)
    (let ((strings (cddr (histdir-history-table history)))
          (buffers (histdir-history-buffers history)))
        (dolist (buffer buffers)
            (if (buffer-live-p buffer)
                (with-current-buffer buffer
                    (setq histdir-buffer-local-history-list strings))
                (setq buffers (delq buffer buffers))))
        (aset history 2 buffers)))
(defun histdir-history--add-newest (history key string)
    (let* ((table           (histdir-history-table history))
           (unordered-table (car table))
           (older-duplicate (gethash key unordered-table)))
        (when older-duplicate
            (dlist-setcar older-duplicate nil)
            (when (eq older-duplicate (histdir-history--last history))
                (histdir-history--set-last history (caar older-duplicate))))
        (ordered-hash-table-put table key string))
    (histdir--update-buffer-local-history-pointers history))
(defun histdir-history--add-oldest (history key string)
    (let* ((table           (histdir-history-table history))
           (unordered-table (car table))
           (newer-duplicate (gethash key unordered-table)))
        (unless newer-duplicate
            (let ((new-last (dlist-cons string nil)))
                (if-let (last (histdir-history--last history))
                    (dlist-setcdr last new-last)
                    (setcdr table new-last))
                (puthash key new-last unordered-table)
                (histdir-history--set-last history new-last))
            (when (= (hash-table-count unordered-table) 1)
                (histdir--update-buffer-local-history-pointers history)))))
(defun histdir-history--remove (history key)
    (let* ((table           (histdir-history-table history))
           (unordered-table (car table)))
        (when-let ((first   (cdr table))
                   (removed (gethash key unordered-table)))
            (ordered-hash-table-pop table key)
            (dlist-setcar removed nil)
            (when (eq removed (histdir-history--last history))
                (histdir-history--set-last history (caar removed)))
            (when (eq removed first)
                (histdir--update-buffer-local-history-pointers history)))))
(defun histdir--read-file (path)
    (condition-case _error
        (progn
            (insert-file-contents path)
            (goto-char (point-max))
            (when (equal (char-before) ?\n)
                (delete-char -1))
            (prog1
                (string-replace "\0" "" (buffer-string))
                (erase-buffer)))
        (file-missing)))
(defun histdir--read-call (file)
    (when-let (hash (histdir--read-file file))
        (when (> (length hash) 0)
            (thread-yield)
            (cons
                (intern hash)
                (histdir--read-file (concat
                    (file-name-parent-directory (file-name-directory file))
                    "string/"
                    hash))))))
(defun histdir--read (path history)
    (let ((default-directory (concat path "/v1"))
          (files nil))
        (thread-yield)
        (condition-case _error
            (setq files (directory-files "call" nil "..." t))
            (file-missing))
        (thread-yield)
        (setq files (sort files 'string-greaterp))
        (with-temp-buffer
            (dolist (file files)
                (thread-yield)
                (when-let (entry (histdir--read-call (concat "call/" file)))
                    (let-uncons (hash string entry)
                        (histdir-history--add-oldest history hash string)))))))
(defun histdir--see-add (history watch-event)
    (let-unpack ((_descriptor action file) watch-event
                 (hash string) ())
        (when (eq action 'renamed)
            (setq file (nth 3 watch-event)))
        (when (memq action '(created changed renamed))
            (with-temp-buffer
                (uncons hash string (histdir--read-call file)))
            (when (and string (> (length string) 0))
                (histdir-history--add-newest history hash string)))))
(defun histdir--see-remove (history watch-event)
    (let-unpack ((_descriptor action file) watch-event
                 (hash string) ())
        (when (eq action 'deleted)
            (let ((hash (intern (file-name-base file))))
                (histdir-history--remove history hash)))))
(defun histdir--watch (path history)
    (let ((directory (concat path "/v1")))
        (list
            (file-notify-add-watch (concat directory "/call") '(change)
                (apply-partially 'histdir--see-add history))
            (file-notify-add-watch (concat directory "/string") '(change)
                (apply-partially 'histdir--see-remove history)))))
(defvar histdir)
(make-variable-buffer-local 'histdir)
(defconst histdir--histories (make-hash-table :test 'equal))
(defun histdir--history ()
    (gethash (expand-file-name histdir) histdir--histories))
(defun histdir-watch+read ()
    (require 'filenotify)
    (let* ((path       (expand-file-name histdir))
           (history    (gethash path histdir--histories))
           (first-read (not history)))
        (when first-read
            (setq history (make-histdir-history))
            (puthash path history histdir--histories))
        (histdir-history--register-buffer history (current-buffer))
        (histdir--update-buffer-local-history-pointers history)
        (unless (histdir-history-watches history)
            (let ((descriptors (histdir--watch path history)))
                (histdir-history--set-watches history descriptors)))
        (when first-read
            (make-thread (apply-partially 'histdir--read path history)))))
(defun histdir-add (entry &optional deduplicate)
    (let ((default-directory "~"))
        (setq deduplicate (if deduplicate "--deduplicate" "--"))
        (call-process-region entry nil "histdir" nil 0 nil
            "add" deduplicate (expand-file-name histdir))))
(defun histdir-remove (entry)
    (let ((default-directory "~"))
        (call-process-region entry nil "histdir" nil 0 nil
            "remove" (expand-file-name histdir))))
(defvar histdir-buffer-local-history--position nil)
(make-variable-buffer-local 'histdir-buffer-local-history--position)
(defun histdir-input-add (input &optional deduplicate)
    (setq histdir-buffer-local-history--position nil)
    (unless (equal (string-trim input) "")
        (histdir-add input deduplicate)))
(defun histdir-input--select (position)
    (setq histdir-buffer-local-history--position position)
    (replace-command
        (if (not position)
            ""
            (dlist-car position))))
(defun histdir-input--cycle-older (position)
    (let ((history (histdir--history)))
        (if position
            (let ((older (dlist-cdr position)))
                (while (and older (not (dlist-car older)))
                    (setq older (dlist-cdr older)))
                older)
            (cdr (histdir-history-table history)))))
(defun histdir-input--cycle-newer (position)
    (let ((history (histdir--history)))
        (if position
            (let ((newer (caar position)))
                (while (and newer (not (dlist-car newer)))
                    (setq newer (caar newer)))
                newer)
            (histdir-history--last history))))
(defun histdir-input-cycle-older () (interactive)
    (histdir-input--select
        (histdir-input--cycle-older
            histdir-buffer-local-history--position)))
(defun histdir-input-cycle-newer () (interactive)
    (histdir-input--select
        (histdir-input--cycle-newer
            histdir-buffer-local-history--position)))

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
    (setq eshell-history-size 0)
    (advice-add 'eshell-hist-initialize :before (lambda (&rest _)
        (setq histdir "~/.history/eshell")))
    (advice-add 'eshell-read-history :override (lambda (&rest _)
        (histdir-watch+read)))
    (advice-add 'eshell-add-input-to-history :override (lambda (input)
        (histdir-input-add input t)))
    (advice-add 'eshell-write-history :override (lambda (&rest _)))
    (defun in-eshell-prompt-p ()
        (eq (get-text-property (point) 'field) 'prompt))
    (defun in-eshell-scrollback-p ()
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
    (defun fixed-eshell-up-arrow () (interactive)
        (if (in-eshell-scrollback-p)
            (message "not in input")
            (condition-case _error
                (progn
                    (previous-line)
                    (when (in-eshell-prompt-p)
                        (move-end-of-line 1)
                        (goto-char (field-beginning)))
                    (when (in-eshell-scrollback-p)
                        (goto-char (buffer-end 1))
                        (histdir-input-cycle-older)))
                (beginning-of-buffer
                    (histdir-input-cycle-older)))))
    (defun fixed-eshell-down-arrow () (interactive)
        (if (in-eshell-scrollback-p)
            (message "not in input")
            (condition-case _error
                (next-line)
                (end-of-buffer
                    (histdir-input-cycle-newer)))))
    (advice-add 'eshell-interrupt-process :before (lambda (&rest _)
        (setq histdir-buffer-local-history--position nil)))
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
    (defun pop-to-command-eshell (command &optional context name callback)
        (setq name (pop-to-command-buffer-name "eshell" command context name))
        (let ((program   (car command))
              (arguments (cdr command))
              (directory default-directory)
              (buffer    (get-buffer name)))
            (if buffer
                (with-current-buffer buffer
                    (when eshell-process-list
                        (eshell-kill-process)
                        (sleep-for 0.04)))
                (setq buffer (get-buffer-create name))
                (set-buffer buffer)
                (setq histdir nil)
                (let ((eshell-non-interactive-p t)
                      (eshell-history-file-name nil))
                    (eshell-mode)))
            (pop-to-buffer buffer)
            (setq-local pop-to-command-buffer t)
            (setq-local pop-to-command--callback callback)
            (setq default-directory directory)
            (end-of-buffer)
            (run-hooks 'pop-to-command-setup-hook)
            (let ((parsed-command (eshell-parse-command program arguments t)))
                (eshell-eval-command parsed-command))
            buffer))
    (add-hook 'eshell-post-command-hook (lambda ()
        (when pop-to-command-buffer
            (insert "\nCommand " (buffer-name) " done.\n")
            (end-of-buffer)
            (when pop-to-command--callback
                (funcall pop-to-command--callback))))))
(use-package esh-mode
    :config
    (define-key eshell-mode-map "\C-m" 'fixed-eshell-send-input))
(use-package em-hist
    :config
    (define-key eshell-hist-mode-map [up] 'fixed-eshell-up-arrow)
    (define-key eshell-hist-mode-map [down] 'fixed-eshell-down-arrow))

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
    (setq vertico-cycle t)
    (setq minibuffer-prompt-properties
        (append '(cursor-intangible t) minibuffer-prompt-properties))
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

(use-package consult
    :config
    (add-to-list 'consult-mode-histories
        '(eshell-mode
          histdir-buffer-local-history-list
          nil
          eshell-bol))
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
    (defun hack-consult-preview (preview cell action candidate)
        (funcall preview 'setup nil)
        (if (eq action 'hack-repeat-preview)
            (funcall preview 'preview (car cell))
            (setcar cell candidate)
            (funcall preview action candidate)))
    (defun fixed-consult-history (prefix-argument) (interactive "P")
        (let* ((position (if prefix-argument (point) (buffer-end 1)))
               (command  (command-string position))
               (preview  (apply-partially 'hack-consult-preview
                             (consult--insertion-preview (point) (point))
                             (cons nil nil))))
            (let-unpack ((history index bol) (consult--current-history))
                (delete-command position)
                (with-temp-buffer
                    (insert-before-markers command)
                    (run-with-idle-timer 0.04 nil
                        (lambda-let (preview (window (selected-window)))
                            (with-selected-window window
                                (funcall preview 'hack-repeat-preview nil))))
                    (with-advice ('consult--insertion-preview
                                     :override (ignore+return preview))
                        (consult-history history index bol))
                    (setq command (buffer-string))))
            (end-of-buffer)
            (replace-command command)
            command))
    (defun consult-line-resume (prefix-argument) (interactive "P")
        (when (eq this-command 'consult-line-resume)
            (setq this-command 'consult-line))
        (delete-dups consult--line-history)
        (consult-line
            (cond
                ((not prefix-argument)
                    (nth 0 consult--line-history))
                ((and (integerp prefix-argument) (>= prefix-argument 0))
                    (nth (- prefix-argument 1) consult--line-history))
                (t
                    (completing-read
                        "Resume consult-line: "
                        consult--line-history
                        nil
                        nil
                        nil
                        'consult--line-history)))))
    (defun consult-line-quit () (interactive)
        (let ((query (field-string-no-properties)))
            (if (length> query 0)
                (push query consult--line-history)
                (delete-dups consult--line-history)))
        (abort-minibuffers)))

(use-package orderless
    :config
    (add-to-list 'completion-styles 'orderless))

(use-package eat
    :config
    (defun eat-point ()
        (when eat-terminal
            (marker-position
                (eat-term-display-cursor eat-terminal))))
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
    (defun pop-to-command-eat (command &optional context name callback)
        (setq name (pop-to-command-buffer-name "eat" command context name))
        (let ((program   (car command))
              (arguments (cdr command))
              (directory default-directory)
              (buffer    (get-buffer name)))
            (if buffer
                (with-current-buffer buffer
                    (eat-kill-process))
                (setq buffer (get-buffer-create name))
                (set-buffer buffer)
                (eat-mode))
            (pop-to-buffer buffer)
            (setq-local pop-to-command-buffer t)
            (setq-local pop-to-command--callback callback)
            (setq default-directory directory)
            (run-hooks 'pop-to-command-setup-hook)
            (eat-exec buffer name program nil arguments)
            buffer))
    (add-hook 'eat-exit-hook (lambda (_process)
        (when (and pop-to-command-buffer pop-to-command--callback)
            (funcall pop-to-command--callback))))
    (eat-eshell-mode 1)
    (eat-eshell-visual-command-mode 1))

(use-package evil
    :init
    (setq evil-undo-system 'undo-tree)
    (setq evil-want-C-u-scroll t)
    :config
    (evil-mode 1)
    (evil-declare-not-repeat 'scroll-up-line)
    (evil-declare-not-repeat 'scroll-down-line)
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
    (defun evil-yank-string (string &optional register yank-handler)
        (with-temp-buffer
            (insert string)
            (evil-yank-characters 1 (buffer-end 1) register yank-handler)))
    (defun evil-paste-to-string (count &optional register)
        (repeat-string
            (if register
                (evil-get-register register)
                (current-kill 0))
            (prefix-numeric-value count)))
    (define-key evil-motion-state-map "\"" 'evil-use-register)
    (define-key evil-normal-state-map "U" 'evil-redo)
    (define-key evil-normal-state-map "H" 'evil-join)
    (define-key evil-normal-state-map "J" nil)
    (define-key evil-motion-state-map "J" 'evil-window-bottom)
    (define-key evil-motion-state-map "K" 'evil-window-top)
    (define-key evil-motion-state-map "L" nil)
    (define-key evil-normal-state-map "q" nil)
    (define-key evil-operator-state-map "q" 'evil-force-normal-state)
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
    (evil-select-search-module 'evil-search-module 'evil-search)
    (setq mode-line (if (facep 'mode-line-active) 'mode-line-active 'mode-line))
    (defvar color-code-vi-state--cookie-1 nil)
    (make-variable-buffer-local 'color-code-vi-state--cookie-1)
    (defvar color-code-vi-state--cookie-2 nil)
    (make-variable-buffer-local 'color-code-vi-state--cookie-2)
    (defun color-code-vi-state ()
        (when color-code-vi-state--cookie-1
            (face-remap-remove-relative color-code-vi-state--cookie-1))
        (when color-code-vi-state--cookie-2
            (face-remap-remove-relative color-code-vi-state--cookie-2))
        (if (minibufferp)
            (setq color-code-vi-state--cookie-1
                (face-remap-add-relative 'minibuffer-prompt :foreground (cond
                    ((evil-normal-state-p)   "#FF0000")
                    ((evil-operator-state-p) "#FF8000")
                    ((evil-insert-state-p)   "#00FF00")
                    ((evil-replace-state-p)  "#FFFF00")
                    ((evil-visual-state-p)   "#8080FF")
                    ((evil-emacs-state-p)    "#A020FF")
                    ((evil-motion-state-p)   "#A0E0FF")
                    (t                       "#FFFFFF"))))
            (setq color-code-vi-state--cookie-1
                (face-remap-add-relative mode-line :background (cond
                    ((evil-normal-state-p)   "#FF4040")
                    ((evil-operator-state-p) "#FFA060")
                    ((evil-insert-state-p)   "#40FF40")
                    ((evil-replace-state-p)  "#FFFF80")
                    ((evil-visual-state-p)   "#C0C0FF")
                    ((evil-emacs-state-p)    "#C040FF")
                    ((evil-motion-state-p)   "#80FFFF")
                    (t                       "#FFFFFF"))))
            (setq color-code-vi-state--cookie-2
                (face-remap-add-relative 'mode-line-inactive :background (cond
                    ((evil-normal-state-p)   "#6C2424")
                    ((evil-operator-state-p) "#6C4824")
                    ((evil-insert-state-p)   "#246C24")
                    ((evil-replace-state-p)  "#6C6C36")
                    ((evil-visual-state-p)   "#24246C")
                    ((evil-emacs-state-p)    "#48006C")
                    ((evil-motion-state-p)   "#366C6C")
                    (t                       "#202020"))))))
    (add-hook 'evil-normal-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-operator-state-entry-hook 'color-code-vi-state)
    (add-hook 'evil-insert-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-replace-state-entry-hook  'color-code-vi-state)
    (add-hook 'evil-visual-state-entry-hook   'color-code-vi-state)
    (add-hook 'evil-emacs-state-entry-hook    'color-code-vi-state)
    (add-hook 'evil-motion-state-entry-hook   'color-code-vi-state)
    (set-face-foreground 'mode-line "#010101")
    (setq evil-insert-state-cursor '(bar . 3))
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
    (define-key evil-motion-state-map "gG" 'end-of-buffer)
    (define-prefix-command 'space-map)
    (define-key evil-motion-state-map " " 'space-map)
    (define-key evil-motion-state-map "\C-@" 'evil-execute-in-normal-state)
    (define-key evil-motion-state-map [?\C- ] 'evil-execute-in-normal-state)
    (define-key evil-insert-state-map "\C-@" 'evil-execute-in-normal-state)
    (define-key evil-insert-state-map [?\C- ] 'evil-execute-in-normal-state)
    (define-key evil-replace-state-map "\C-@" 'evil-execute-in-normal-state)
    (define-key evil-replace-state-map [?\C- ] 'evil-execute-in-normal-state)
    (define-key evil-emacs-state-map "\C-@" 'evil-execute-in-normal-state)
    (define-key evil-emacs-state-map [?\C- ] 'evil-execute-in-normal-state)
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
    (define-key evil-motion-state-map " E" 'eval-print-last-sexp)
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
    (define-key evil-motion-state-map " d" 'dired)
    (add-to-list 'evil-motion-state-modes 'dired-mode)
    (define-key evil-motion-state-map " y" 'execute-extended-command)
    (define-key evil-motion-state-map " ," 'eval-expression)
    (define-key evil-motion-state-map " h" help-map)
    (evil-define-key 'motion help-mode-map "H" 'help-go-back)
    (evil-define-key 'motion help-mode-map "L" 'help-go-forward)
    (define-key evil-motion-state-map " p" 'list-packages)
    (add-to-list 'evil-motion-state-modes 'package-menu-mode)
    (define-key evil-motion-state-map " n" 'universal-argument)
    (define-key universal-argument-map " n" 'universal-argument-more)
    (define-key universal-argument-map [escape] 'ignore)
    (define-key evil-motion-state-map " q" 'ignore)
    (define-key evil-motion-state-map [?  escape] 'ignore)
    (define-key evil-motion-state-map " Q" 'delete-frame)
    (define-key evil-motion-state-map " !" 'save-buffers-kill-emacs)
    (defun consult-history-execute (prefix-argument) (interactive "P")
        (let* ((consult-history-execute t)
               (command (fixed-consult-history prefix-argument))
               (run-key (listify-key-sequence "\C-m")))
            (evil-repeat-start)
            (add-to-list 'evil-repeat-info `((lambda ()
                (end-of-buffer)
                (replace-command ,command)
                (when ,consult-history-execute
                    (setq unread-command-events '(,@run-key))))))
            (evil-repeat-stop)
            (when consult-history-execute
                (setq unread-command-events run-key))))
    (defun consult-history-execute-quit () (interactive)
        (setq consult-history-execute nil)
        (vertico-exit))
    (defun consult-history-remove (prefix-argument) (interactive "P")
        (let* ((consult-history-remove t)
               (entry (fixed-consult-history prefix-argument))
               (history (car (consult--current-history))))
            (when consult-history-remove
                (if histdir
                    (histdir-remove entry)
                    (if (ring-p history)
                        (while-let ((index (ring-member history entry)))
                            (ring-remove history index))))
                (evil-end-undo-step)
                (evil-start-undo-step)
                (delete-command))))
    (consult-customize consult-history-remove :prompt "Remove history: ")
    (evil-declare-not-repeat 'consult-history-remove)
    (defun consult-history-remove-quit () (interactive)
        (setq consult-history-remove nil)
        (vertico-exit))
    (define-key evil-motion-state-map "gh" 'consult-history-execute)
    (define-key evil-motion-state-map "gr" 'consult-history-remove)
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
        (evil-local-set-key 'operator [escape] 'evil-force-normal-state)
        (evil-local-set-key 'insert "\C-d" 'eshell-send-eof-to-process)))
    (add-hook 'eshell-pre-command-hook (lambda ()
        (setq evil-eshell-state-for-next-input evil-state)
        (evil-insert-state)))
    (add-hook 'eshell-post-command-hook (lambda ()
        (evil-force-normal-state)
        (when (eq (current-buffer) (window-buffer (selected-window)))
            (evil-change-state evil-eshell-state-for-next-input))))
    (evil-declare-not-repeat 'fixed-eshell-send-input)
    (evil-declare-not-repeat 'eshell-interrupt-process)
    (define-key evil-motion-state-map " T" 'eat)
    (add-to-list 'evil-emacs-state-modes 'eat-mode)
    (add-hook 'eat-exit-hook (lambda (_process) (evil-normal-state nil)))
    (add-hook 'eat-mode-hook (lambda ()
        (evil-local-set-key 'emacs "\C-[" 'eat-self-input)))
    (define-key eat-semi-char-mode-map "\C-c\C-z" 'eat-self-input)
    (evil-set-register ?r (repeat-string "1234567890" 8))
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
    (defun toggle-inhibit-read-only () (interactive)
        (setq inhibit-read-only (not inhibit-read-only))
        (message "inhibit-read-only: %s" inhibit-read-only))
    (define-key evil-motion-state-map " I" 'toggle-inhibit-read-only)
    (define-key evil-motion-state-map " #" 'display-line-numbers-mode)
    (set-face-foreground 'line-number "#808080")
    (defun cycle-line-wrap () (interactive)
        (if truncate-lines
            (setq truncate-lines nil)
            (visual-line-mode 'toggle)
            (if (not visual-line-mode)
                (setq truncate-lines t)))
        (message "visual-line-mode: %s truncate-lines: %s"
            visual-line-mode truncate-lines))
    (evil-declare-not-repeat 'cycle-line-wrap)
    (define-key evil-motion-state-map " ;" 'cycle-line-wrap)
    (define-key evil-motion-state-map " l" 'consult-line)
    (define-key evil-motion-state-map " L" 'consult-line-resume)
    (define-key evil-motion-state-map " sl" 'consult-ripgrep)
    (define-key evil-motion-state-map " sf" 'consult-fd)
    (define-key evil-motion-state-map " x" 'tramp-cleanup-connection)
    (add-hook 'pop-to-command-setup-hook (lambda ()
        (evil-initialize-state)
        (evil-local-set-key 'normal "q" 'quit-window)
        (evil-local-set-key 'normal [escape] 'quit-window)))
    (defun pop-to-command-eshell--not-a-file (name)
        (pop-to-command-eshell
            (list "echo" (concat (buffer-name) " is not visiting a file"))
            (buffer-name)
            name))
    (defun diff-unsaved-changes () (interactive)
        (if (not buffer-file-name)
            (pop-to-command-eshell--not-a-file "Diff unsaved")
            (with-temporary-directory directory
                (let ((file    (concat directory "/file"))
                      (unsaved (concat directory "/unsaved"))
                      (default-directory "~"))
                    (copy-file buffer-file-name file)
                    (write-region (buffer-end -1) (buffer-end 1) unsaved)
                    (pop-to-command-eshell
                        (list "cdexec" directory "gd" "file" "unsaved")
                        (buffer-name)
                        "Diff unsaved"
                        (apply-partially 'diff-unsaved-changes--finish
                            (current-buffer) directory)))
                (setq directory nil))))
    (defun diff-unsaved-changes--finish (buffer directory)
        (unwind-protect
            (refresh-modified-state buffer)
            (delete-directory directory t)))
    (define-key evil-motion-state-map " c" 'diff-unsaved-changes)
    (defun partial-save () (interactive)
        (if (not buffer-file-name)
            (pop-to-command-eshell--not-a-file "Partial save")
            (with-temporary-directory directory
                (let ((file    (concat directory "/file"))
                      (unsaved (concat directory "/unsaved"))
                      (default-directory "~"))
                    (copy-file buffer-file-name file)
                    (write-region (buffer-end -1) (buffer-end 1) unsaved)
                    (pop-to-command-eshell
                        (list "gp" file unsaved)
                        (buffer-name)
                        "Partial save"
                        (apply-partially 'partial-save--finish
                            (current-buffer) directory)))
                (setq directory nil))))
    (defun partial-save--finish (buffer directory)
        (unwind-protect
            (with-current-buffer buffer
                (copy-file (concat directory "/file") buffer-file-name t)
                (refresh-modified-state buffer))
            (delete-directory directory t)))
    (define-key evil-motion-state-map " w" 'partial-save)
    (defun partial-revert () (interactive)
        (if (not buffer-file-name)
            (call-interactively 'revert-buffer)
            (with-temporary-directory directory
                (let ((file    (concat directory "/file"))
                      (unsaved (concat directory "/unsaved"))
                      (default-directory "~"))
                    (copy-file buffer-file-name file)
                    (write-region (buffer-end -1) (buffer-end 1) unsaved)
                    (pop-to-command-eshell
                        (list "gp" unsaved file)
                        (buffer-name)
                        "Partial revert"
                        (apply-partially 'partial-revert--finish
                            (current-buffer) directory)))
                (setq directory nil))))
    (defun partial-revert--finish (buffer directory)
        (unwind-protect
            (with-current-buffer buffer
                (let ((buffer-file-name (concat directory "/unsaved")))
                    (revert-buffer t t t))
                (setq buffer-file-truename
                    (abbreviate-file-name (file-truename buffer-file-name)))
                (refresh-modified-state buffer))
            (delete-directory directory t)))
    (define-key evil-motion-state-map " r" 'partial-revert)
    (define-key evil-motion-state-map " R" 'revert-buffer)
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
        (let ((quit (cond
                        ((eq this-command 'consult-history-execute)
                            'consult-history-execute-quit)
                        ((eq this-command 'consult-history-remove)
                            'consult-history-remove-quit)
                        ((eq this-command 'consult-line)
                            'consult-line-quit)
                        (t
                            'abort-minibuffers))))
            (evil-local-set-key 'normal   [escape] quit)
            (evil-local-set-key 'normal   "q"      quit)
            (evil-local-set-key 'operator [escape] 'evil-force-normal-state)
            (evil-local-set-key 'operator "q"      'evil-force-normal-state))))
    (add-hook 'isearch-mode-hook (lambda ()
        (define-key overriding-terminal-local-map [escape]
            (lambda () (interactive)
                (isearch-done)))))
    (add-hook 'Info-mode-hook (lambda ()
        (evil-local-set-key 'motion "H" 'Info-history-back)
        (evil-local-set-key 'motion "L" 'Info-history-forward)
        (evil-local-set-key 'motion " " 'space-map)))
    (evil-declare-not-repeat 'ignore)
    (add-to-list 'evil-motion-state-modes 'shortdoc-mode)
    (with-current-buffer (messages-buffer)
        (evil-force-normal-state)))

(define-derived-mode histdir-repl-mode eat-mode "HER")
(add-to-list 'consult-mode-histories
    '(histdir-repl-mode
      histdir-buffer-local-history-list
      nil
      beginning-of-line))
(add-to-list 'command-at-point-mode-alist
    '(histdir-repl-mode
      histdir-repl-get-input
      histdir-repl-delete-input
      histdir-repl-replace-input))
(defun histdir-repl (command histdir)
    (let* ((program       (car command))
           (arguments     (cdr command))
           (buffer-locals (list default-directory histdir))
           (name          (concat "*" (string-join command " ") "*"))
           (buffer        (get-buffer name)))
        (unless buffer
            (setq buffer (get-buffer-create name))
            (set-buffer buffer)
            (histdir-repl-mode))
        (pop-to-buffer-same-window buffer)
        (unpack (default-directory histdir) buffer-locals)
        (unless (get-buffer-process (current-buffer))
            (eat-exec buffer name program nil arguments))
        (evil-local-set-key 'normal "q" 'quit-window)
        (evil-local-set-key 'normal "\C-m" 'histdir-repl-send-input)
        (evil-local-set-key 'insert "\C-m" 'histdir-repl-send-input)
        (evil-local-set-key 'normal "\C-c\C-c" 'histdir-repl-interrupt)
        (evil-local-set-key 'insert "\C-c" 'histdir-repl-interrupt)
        (evil-local-set-key 'normal [escape] 'histdir-repl-interrupt)
        (evil-local-set-key 'normal "d" 'histdir-repl-evil-delete-input)
        (evil-local-set-key 'normal "D" 'histdir-repl-evil-delete-input-line)
        (evil-local-set-key 'normal "c" 'histdir-repl-evil-change-input)
        (evil-local-set-key 'normal "C" 'histdir-repl-evil-change-input-line)
        (evil-local-set-key 'normal "i" 'histdir-repl-insert)
        (evil-local-set-key 'normal "I" 'histdir-repl-insert-at-beginning)
        (evil-local-set-key 'normal "a" 'histdir-repl-append)
        (evil-local-set-key 'normal "A" 'histdir-repl-append-at-end)
        (evil-local-set-key 'normal "o" 'histdir-repl-append-at-end)
        (evil-local-set-key 'normal "O" 'histdir-repl-insert-at-beginning)
        (evil-local-set-key 'normal "0" 'histdir-repl-beginning-of-line)
        (evil-local-set-key 'normal "$" 'histdir-repl-end-of-line)
        (dolist (key '("x" [delete] [deletechar]))
            (evil-local-set-key 'normal key 'histdir-repl-delete-char))
        (dolist (key '("X" [backspace] "\C-?"))
            (evil-local-set-key 'normal key 'histdir-repl-backspace-char))
        (evil-local-set-key 'normal "r" 'histdir-repl-evil-replace)
        (evil-local-set-key 'normal "R" 'histdir-repl-evil-replace-state)
        (substitute-key-definition
            'self-insert-command 'histdir-repl-self-input+replace
            evil-replace-state-local-map global-map)
        (dolist (key '([backspace] "\C-?"))
            (evil-local-set-key 'replace key
                'histdir-repl-evil-replace-backspace))
        (dolist (key '([delete] [deletechar]))
            (evil-local-set-key 'replace key
                'histdir-repl-evil-replace-delete))
        (evil-local-set-key 'replace "\C-m"
            'histdir-repl-evil-replace-send-input)
        (evil-local-set-key 'replace "\C-c"
            'histdir-repl-evil-replace-interrupt)
        (evil-local-set-key 'replace "\t" 'histdir-repl-evil-replace-tab)
        (evil-local-set-key 'normal "p" 'histdir-repl-evil-paste-after)
        (evil-local-set-key 'normal "P" 'histdir-repl-evil-paste-before)
        (evil-local-set-key 'normal "gp"
            'histdir-repl-evil-replacing-paste-after)
        (evil-local-set-key 'normal "gP"
            'histdir-repl-evil-replacing-paste-before)
        (evil-local-set-key 'insert "\C-q" 'eat-quoted-input)
        (dolist (key '("\C-a" "\C-d" "\C-e" "\C-k" "\C-l" "\C-u"))
            (evil-local-set-key 'insert key 'eat-self-input))
        (evil-normal-state)
        (histdir-watch+read)
        (evil-local-set-key 'insert [up] 'histdir-input-cycle-older)
        (evil-local-set-key 'insert [down] 'histdir-input-cycle-newer)
        (evil-local-set-key 'replace [up] 'histdir-repl-evil-replace-up)
        (evil-local-set-key 'replace [down] 'histdir-repl-evil-replace-down)
        buffer))
(defun histdir-repl-beginning-of-input () (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-a")
    (sleep-for 0.04)
    (point))
(defun histdir-repl-end-of-input () (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-e")
    (sleep-for 0.04)
    (point))
(defun histdir-repl-get-input (&optional position) (interactive)
    (let ((start (histdir-repl-beginning-of-input))
          (end   (histdir-repl-end-of-input)))
        (substring-no-properties (filter-buffer-substring start end))))
(defun histdir-repl-delete-input (&optional position) (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "x\C-a\C-k"))
(defun histdir-repl-replace-input (new-input &optional position) (interactive)
    (histdir-repl-delete-input)
    (buffer-process-send-string new-input))
(defun histdir-repl-send-input () (interactive)
    (histdir-input-add (histdir-repl-get-input) t)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-m"))
(defun histdir-repl-interrupt () (interactive)
    (setq histdir-buffer-local-history--position nil)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-c"))
(defun histdir-repl-forward-char-in-input (&optional count)
    (buffer-process-send-string (repeat-string "\e[C" (or count 1))))
(defun histdir-repl-backward-char-in-input (&optional count)
    (buffer-process-send-string (repeat-string "\e[D" (or count 1))))
(defun histdir-repl-backspace-char-in-input (&optional count)
    (buffer-process-send-string (repeat-string "\C-?" (or count 1))))
(defun histdir-repl-enter-input (offset)
    (let* ((point-in-buffer   (point))
           (point-in-terminal (goto-char (eat-point)))
           (start             (histdir-repl-beginning-of-input))
           (end               (histdir-repl-end-of-input))
           (desired-point     nil))
        (if (<= start point-in-buffer end)
            (setq desired-point point-in-buffer)
            (setq desired-point point-in-terminal))
        (setq desired-point (+ desired-point offset))
        (setq desired-point (min desired-point end))
        (setq desired-point (max desired-point start))
        (histdir-repl-backward-char-in-input (length
            (filter-buffer-substring desired-point end)))
        (list start desired-point end)))
(defun histdir-repl-insert () (interactive)
    (histdir-repl-enter-input 0)
    (evil-insert-state))
(defun histdir-repl-insert-at-beginning () (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-a")
    (evil-insert-state))
(defun histdir-repl-append () (interactive)
    (histdir-repl-enter-input 1)
    (evil-insert-state))
(defun histdir-repl-append-at-end () (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-e")
    (evil-insert-state))
(defun histdir-repl-enter+delete-input (count)
    (let* ((point-in-buffer   (point))
           (point-in-terminal (goto-char (eat-point)))
           (start             (histdir-repl-beginning-of-input))
           (end               (histdir-repl-end-of-input))
           (desired-point     nil)
           (length            nil))
        (if (<= start point-in-buffer end)
            (setq desired-point point-in-buffer)
            (setq desired-point point-in-terminal))
        (setq length (length (filter-buffer-substring desired-point end)))
        (when (> length count)
            (histdir-repl-backward-char-in-input (- length count)))
        (setq count (min length count))
        (histdir-repl-backspace-char-in-input count)
        count))
(defun histdir-repl-delete-char (&optional count) (interactive "p")
    (histdir-repl-enter+delete-input count))
(defun histdir-repl-backspace-char (&optional count) (interactive "p")
    (let-unpack ((start point) (histdir-repl-enter-input 0))
        (setq count (min count (- point start)))
        (histdir-repl-backspace-char-in-input count)))
(defun histdir-repl-enter+replace-input (count character)
    (let* ((point-in-buffer   (point))
           (point-in-terminal (goto-char (eat-point)))
           (start             (histdir-repl-beginning-of-input))
           (end               (histdir-repl-end-of-input))
           (desired-point     nil)
           (length            nil))
        (if (<= start point-in-buffer end)
            (setq desired-point point-in-buffer)
            (setq desired-point point-in-terminal))
        (setq length (length (filter-buffer-substring desired-point end)))
        (when (> length count)
            (histdir-repl-backward-char-in-input (- length count)))
        (setq count (min length count))
        (histdir-repl-backspace-char-in-input count)
        (buffer-process-send-string (make-string count character))
        (histdir-repl-backward-char-in-input count)
        count))
(defun histdir-repl-evil-replace (count) (interactive "p")
    (let ((character nil))
        (unwind-protect
            (let ((evil-force-cursor 'replace))
                (evil-refresh-cursor)
                (setq character (evil-read-key)))
            (evil-refresh-cursor))
        (histdir-repl-enter+replace-input count character)))
(defvar histdir-repl--evil-replace-edges nil)
(make-variable-buffer-local 'histdir-repl--evil-replace-edges)
(defun histdir-repl-evil-replace-state () (interactive)
    (let-unpack ((start point end) (histdir-repl-enter-input 0))
        (setq histdir-repl--evil-replace-edges (list start point end end)))
    (evil-replace-state))
(defun histdir-repl-self-input+replace () (interactive)
    (let-unpack ((_start-of-input start-of-replace _end-of-replace end-of-input)
                     histdir-repl--evil-replace-edges)
        (let ((point-before-replace (point)))
            (call-interactively 'eat-self-input)
            (setcar (cdr histdir-repl--evil-replace-edges)
                (min point-before-replace start-of-replace))
            (if (< point-before-replace end-of-input)
                (buffer-process-send-string "\C-d")
                (setcar (cdddr histdir-repl--evil-replace-edges)
                    (1+ end-of-input))))))
(defun histdir-repl-evil-replace-backspace () (interactive)
    (let-unpack ((start-of-input start-of-replace end-of-replace end-of-input)
                     histdir-repl--evil-replace-edges)
        (when (< start-of-input (point))
            (if (< start-of-replace (point))
                (if (<= (point) end-of-replace)
                    (if-let (replaced
                                (cdr (assq (1- (point)) evil-replace-alist)))
                        (buffer-process-send-string
                            (concat "\C-?" (char-to-string replaced) "\e[D"))
                        (histdir-repl-backspace-char-in-input)
                        (setcar (cdddr histdir-repl--evil-replace-edges)
                            (1- end-of-input)))
                    (histdir-repl-backspace-char-in-input)
                    (setcar (cdddr histdir-repl--evil-replace-edges)
                        (1- end-of-input)))
                (histdir-repl-backward-char-in-input)
                (setcar (cdr histdir-repl--evil-replace-edges)
                    (1- start-of-replace))))))
(defun histdir-repl-evil-replace-delete () (interactive)
    (let-unpack ((start-of-input start-of-replace end-of-replace end-of-input)
                     histdir-repl--evil-replace-edges)
        (histdir-repl-delete-char-in-input)
        (setq histdir-repl--evil-replace-edges (list
            start-of-input
            (min start-of-replace (point))
            end-of-replace
            (1- end-of-input)))))
(defun histdir-repl-evil-replace-send-input () (interactive)
    (evil-insert-state)
    (histdir-repl-send-input))
(defun histdir-repl-evil-replace-interrupt () (interactive)
    (evil-insert-state)
    (histdir-repl-interrupt))
(defun histdir-repl-evil-replace-tab () (interactive)
    (evil-insert-state)
    (call-interactively 'eat-self-input))
(defun histdir-repl-evil-replace-up () (interactive)
    (evil-insert-state)
    (histdir-input-cycle-older))
(defun histdir-repl-evil-replace-down () (interactive)
    (evil-insert-state)
    (histdir-input-cycle-newer))
(defun histdir-repl-point-in-input-p ()
    (save-excursion
        (let* ((point-in-buffer   (point))
               (point-in-terminal (goto-char (eat-point)))
               (start             (histdir-repl-beginning-of-input))
               (end               (histdir-repl-end-of-input)))
            (prog1
                (<= start point-in-buffer end)
                (histdir-repl-backward-char-in-input (length
                    (filter-buffer-substring point-in-terminal end)))))))
(defun histdir-repl-beginning-of-line () (interactive)
    (if (histdir-repl-point-in-input-p)
        (histdir-repl-beginning-of-input)
        (beginning-of-line)))
(defun histdir-repl-end-of-line () (interactive)
    (if (histdir-repl-point-in-input-p)
        (histdir-repl-end-of-input)
        (end-of-line)))
(defun histdir-repl-enter+delete-input-range (start-of-range end-of-range)
    (goto-char (eat-point))
    (let* ((start-of-input (histdir-repl-beginning-of-input))
           (end-of-input   (histdir-repl-end-of-input))
           (start-of-range (max start-of-range start-of-input))
           (end-of-range   (min end-of-range end-of-input))
           (skip           (length (filter-buffer-substring
                                       end-of-range end-of-input)))
           (deleted        (filter-buffer-substring
                               start-of-range end-of-range)))
        (histdir-repl-backward-char-in-input skip)
        (histdir-repl-backspace-char-in-input (length deleted))
        deleted))
(evil-define-operator histdir-repl-evil-delete-input (start end type register)
    (interactive "<R><x>")
    :move-point nil
    (if (eq type 'line)
        (histdir-repl-evil-delete-input-line register)
        (let ((deleted (histdir-repl-enter+delete-input-range start end))
              (evil-was-yanked-without-register nil))
            (evil-yank-string deleted register))))
(evil-define-operator histdir-repl-evil-delete-input-line (&optional register)
    (interactive "<x>")
    :move-point nil
    (let ((input (histdir-repl-get-input))
          (evil-was-yanked-without-register nil))
        (evil-yank-string input register))
    (histdir-repl-delete-input))
(evil-define-operator histdir-repl-evil-change-input (start end type register)
    (interactive "<R><x>")
    :move-point nil
    (if (eq type 'line)
        (histdir-repl-evil-delete-input-line register)
        (let ((deleted (histdir-repl-enter+delete-input-range start end))
              (evil-was-yanked-without-register nil))
            (evil-yank-string deleted register)))
    (evil-insert-state))
(evil-define-operator histdir-repl-evil-change-input-line (&optional register)
    (interactive "<x>")
    :move-point nil
    (let ((input (histdir-repl-get-input))
          (evil-was-yanked-without-register nil))
        (evil-yank-string input register))
    (histdir-repl-delete-input)
    (evil-insert-state))
(evil-define-command histdir-repl-evil-paste-after (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-replace "\n" "" string)))
        (histdir-repl-enter-input 1)
        (buffer-process-send-string string)))
(evil-define-command histdir-repl-evil-paste-before (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-replace "\n" "" string)))
        (histdir-repl-enter-input 0)
        (buffer-process-send-string string)
        (histdir-repl-backward-char-in-input (length string))))
(evil-define-command histdir-repl-evil-replacing-paste-after
        (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-replace "\n" "" string))
           (length (length string)))
        (histdir-repl-enter+delete-input length)
        (buffer-process-send-string string)))
(evil-define-command histdir-repl-evil-replacing-paste-before
        (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-replace "\n" "" string))
           (length (length string)))
        (histdir-repl-enter+delete-input length)
        (buffer-process-send-string string)
        (histdir-repl-backward-char-in-input length)))
(defun histdir-repl-name (name)
    (cond
        ((string-prefix-p "python" name) "python")
        ((string-prefix-p "pypy"   name) "python")
        ((string-prefix-p "node"   name) "js")
        ((string-prefix-p "bun"    name) "js")
        ((string-prefix-p "irb"    name) "ruby")
        (t name)))
(defun eshell/r (&rest command)
    (let* ((program      (car command))
           (program-name (file-name-base program))
           (repl         (histdir-repl-name program-name))
           (histdir      (concat "~/.history/" repl)))
        (histdir-repl command histdir)))
(put 'eshell/r 'eshell-no-numeric-conversions t)
(defun eshell/ro (&rest command)
    (other-window-prefix)
    (apply 'eshell/r command))
(put 'eshell/ro 'eshell-no-numeric-conversions t)

(defvar face-remap-selected-window--window (selected-window))
(defvar face-remap-selected-window--tagged nil)
(make-variable-buffer-local 'face-remap-selected-window--tagged)
(defun face-remap-selected-window ()
    (set-window-parameter face-remap-selected-window--window
        'face-remap-selected-window nil)
    (setq face-remap-selected-window--window (selected-window))
    (unless face-remap-selected-window--tagged
        (face-remap-add-relative 'default
            '(:filtered (:window face-remap-selected-window t)
                (:background "#010101")))
        (setq face-remap-selected-window--tagged t))
    (set-window-parameter face-remap-selected-window--window
        'face-remap-selected-window t))
(setq after-focus-change-function 'face-remap-selected-window)
(add-hook 'window-configuration-change-hook 'face-remap-selected-window)
(add-hook 'buffer-list-update-hook 'face-remap-selected-window)
(setq redisplay-skip-initial-frame nil)

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
        '("#00FF00" "#808080" "#202020" "W" "Window state"))
    (defconst window-state-target-pending
        '("#00FF00" "#C08040" "#302010" "T" "Target-pending window state"))
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
                        'aw-leading-char-face :foreground hint
                        'aw-background-face   :foreground tint
                        'line-number          :foreground tint
                        'default              :background dim)
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
    (defconst window-state--register-ring (make-ring 9))
    (setcar (cdr window-state--register-ring) 9)
    (defconst window-state--register-bank (make-hash-table :size 27))
    (window-state-define-operator window-state-yank
        (if (or (not window-state-this-register)
                (equal window-state-this-register ?\")
                (<= ?0 window-state-this-register ?9))
            (puthash ?0
                (ring-insert window-state--register-ring
                    (point-marker-with-scroll))
                window-state--register-bank)
            (puthash window-state-this-register
                (point-marker-with-scroll)
                window-state--register-bank)))
    (window-state-define-operator window-state-delete
        (if (or (not window-state-this-register)
                (equal window-state-this-register ?\")
                (<= ?1 window-state-this-register ?9))
            (ring-insert window-state--register-ring
                (point-marker-with-scroll))
            (puthash window-state-this-register
                (point-marker-with-scroll)
                window-state--register-bank))
        (evil-window-delete))
    (window-state-define-operator window-state-paste
        (with-advice ('push-mark :override 'ignore)
            (jump-to-marker-with-scroll
                (if (or (not window-state-this-register)
                        (equal window-state-this-register ?\"))
                    (ring-ref window-state--register-ring 0)
                    (if (<= ?1 window-state-this-register ?9)
                        (ring-ref window-state--register-ring
                            (- window-state-this-register ?1))
                        (gethash window-state-this-register
                            window-state--register-bank))))))
    (defun window-state--swap (window)
        (save-selected-window
            (aw-swap-window window)))
    (defun window-state-swap ()
        (setq window-state--action 'window-state--swap)
        (setq window-state window-state-target-pending)
        (setq window-state--execute-once t))
    (defun window-state-swap-move ()
        (setq window-state--action 'aw-swap-window)
        (setq window-state window-state-target-pending)
        (setq window-state--execute-once t))
    (defun window-state--fast-paste-move (window)
        (let ((marker (point-marker-with-scroll)))
            (select-window window)
            (jump-to-marker-with-scroll marker)))
    (defun window-state--fast-paste (window)
        (save-selected-window
            (window-state--fast-paste-move window)))
    (defun window-state-fast-paste ()
        (setq window-state--action 'window-state--fast-paste)
        (setq window-state window-state-target-pending)
        (setq window-state--execute-once t))
    (defun window-state-fast-paste-move ()
        (setq window-state--action 'window-state--fast-paste-move)
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
            (dotimes (x (prefix-numeric-value current-prefix-arg))
                (evil-search-next))))
    (defun window-state-vi-search-previous (&optional window)
        (unless window
            (setq window window-state-last-search-target))
        (with-selected-window window
            (dotimes (x (prefix-numeric-value current-prefix-arg))
                (evil-search-previous))))
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
        (?f window-state-bury)
        (?F window-state-unbury)
        (?b window-state-bury-operator)
        (?B window-state-unbury-operator)
        (?r window-state-kill-operator)
        (?R window-state-kill)
        (?: window-state-ex-operator)
        (?/ window-state-vi-search-forward-operator)
        (?? window-state-vi-search-backward-operator)
        (?n window-state-vi-search-next)
        (?N window-state-vi-search-previous)
        (?\" window-state-use-register)
        ,(window-state-passthrough ? )
        (?g (lambda ()
                (let* ((key (read-key "g-"))
                       (action (alist-get key '(
                        (?p . window-state-fast-paste)
                        (?P . window-state-fast-paste-move)
                        (?q . window-state-quit)
                        (?\C-\[ . window-state-quit)))))
                    (if action
                        (funcall action)
                        (window-state--bad-candidate key)
                        (setq window-state--execute-once t)))))
        (?q window-state-quit)
        (?\C-\[ window-state-quit)))
    (defun window-state--bad-candidate (key)
        (message "No such candidate: %s, hit `C-g' to quit."
            (char-to-string key)))
    (setq aw-dispatch-function (lambda (character)
        (if (and (equal character ??) (not (alist-get ?? aw-dispatch-alist)))
            (window-state--bad-candidate character)
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
    (define-key evil-motion-state-map " C" 'rainbow-mode))


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
