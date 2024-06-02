(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)


(defconst termux (getenv "TERMUX_VERSION"))


(menu-bar-mode -1)
(defun init-graphic-frame (frame)
    (if (not (display-graphic-p frame))
        (if termux
            (set-face-background 'default "#202020")
            (set-face-background 'default "#141414"))
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (select-frame frame)
        (set-frame-font "DejaVu Sans Mono-13")
        (set-background-color "#141414")
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


(setq init-gc-cons-threshold gc-cons-threshold
      init-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold (* 1024 1024 1024)
      file-name-handler-alist nil)

(when termux
    (define-key minibuffer-inactive-mode-map [mouse-1] nil)
    (define-key mode-line-buffer-identification-keymap
        [mode-line mouse-1] 'ignore)
    (defun fixed-browse-url-xdg-open (url &optional _unused)
        (message "opening %s" url)
        (browse-url-xdg-open url))
    (setq browse-url-browser-function 'fixed-browse-url-xdg-open)
    (setq woman-manpath '("~/../usr/share/man")))


(blink-cursor-mode -1)


(defun require-final-newline-in-new-files ()
    (unless (file-exists-p buffer-file-name)
        (setq-local require-final-newline t))
    nil)
(add-hook 'find-file-hook 'require-final-newline-in-new-files)


(define-key global-map [\C-tab] 'other-window)
(define-key global-map [\C-iso-lefttab]
    (lambda ()
        (interactive)
        (other-window -1)))
(add-hook 'minibuffer-setup-hook
    (lambda ()
        (local-set-key [\C-tab] 'other-window)))


(defun quit-previous-window ()
    (interactive)
    (if (> (count-windows) 1)
        (save-selected-window
            (quit-window nil (get-mru-window nil nil t)))
        (error "Attempt to quit sole window")))


(electric-indent-mode -1)
(setq-default c-basic-offset 4)
(setq lisp-body-indent 4)

(defun double-tab ()
    (interactive)
    (if (eq last-command 'double-tab)
        (let ((lisp-indent-offset 4))
            (indent-for-tab-command))
        (indent-for-tab-command)))

(define-key global-map [remap indent-for-tab-command] 'double-tab)

(setq tab-always-indent 'complete)


(advice-add 'blink-matching-open :override 'ignore)


(setq read-minibuffer-restore-windows nil)
(setq resize-mini-windows t)


(defmacro lambda-let (varlist args &rest body)
    (declare (indent 2))
    (let (bindings parameter argument)
        (dolist (var varlist)
            (if (listp var)
                (if (length= var 2)
                    (setq parameter (car var)
                          argument  (cadr var))
                    (if (length= var 1)
                        (setq parameter (setq argument (car var)))
                        (eval `(let (,var)))))
                (setq parameter (setq argument var)))
            (push `(cons ',parameter ,argument) bindings))
        (if bindings
            (setq bindings (cons 'list (nreverse bindings)))
            (setq bindings t))
        `(eval
             '(lambda ,args
                  ,@body)
             ,bindings)))


(defmacro compose (&rest functions)
    (setq functions (nreverse functions))
    (let ((form `(apply ,(car functions) arguments)))
        (while (setq functions (cdr functions))
            (setq form `(funcall ,(car functions) ,form)))
        `(lambda (&rest arguments) ,form)))


(defmacro apply-split (callable arguments count)
    (setq arguments (copy-sequence arguments))
    (setq count (1- count))
    (let* ((forms (list 'progn))
           (last-cons forms))
        (while arguments
            (setcdr last-cons (list (cons callable arguments)))
            (setq last-cons (cdr last-cons))
            (let ((end (nthcdr count arguments)))
                (setq arguments (cdr end))
                (setcdr end nil)))
        forms))


(defmacro apply-split-nest (callable arguments count body)
    (setq arguments (copy-sequence arguments))
    (setq count (1- count))
    (let* ((forms (list 'unused))
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


(defmacro += (place value)
    `(setf ,place (+ ,place ,value)))


(defmacro -= (place value)
    `(setf ,place (- ,place ,value)))


(defmacro ^= (place value)
    `(setf ,place (logxor ,place ,value)))


(defmacro <<= (place value)
    `(setf ,place (ash ,place ,value)))


(defmacro until (test &rest body)
    `(while (not ,test)
         ,@body))


(defmacro save-point (&rest body)
    `(let ((--save-point-- (point)))
         (unwind-protect
             (progn
                 ,@body)
             (goto-char --save-point--))))


(defmacro save-mutation (&rest body)
    `(let ((buffer-undo-list ()))
         (unwind-protect
             (progn
                 ,@body)
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


(defun advice-where (symbol function)
    (when-let (advice (advice-member-p function symbol))
        (aref (aref advice 2) 2)))

(defun advice-list (symbol)
    (let ((advice-list ()))
        (advice-mapc
            (lambda-let (symbol) (function properties)
                (let ((where (advice-where symbol function))
                      (advice ()))
                    (when properties
                        (push properties advice))
                    (push function advice)
                    (push where advice)
                    (push advice advice-list)))
            symbol)
        advice-list))

(defun advice-remove-all (symbol)
    (dolist (advice (advice-list symbol))
        (let-unpack ((_where function) advice)
            (advice-remove symbol function))))

(defmacro with-advice-1 (advice-add-arguments &rest body)
    `(let ((--with-advice-1-- (list ,@advice-add-arguments)))
         (unwind-protect
             (progn
                 (condition-case error
                     (apply 'advice-add --with-advice-1--)
                     (wrong-number-of-arguments
                         (setq --with-advice-1-- nil)
                         (signal (car error) (cdr error))))
                 ,@body)
             (when --with-advice-1--
                 (setcdr --with-advice-1-- (cddr --with-advice-1--))
                 (apply 'advice-remove --with-advice-1--)))))

(defmacro with-advice (advice-list &rest body)
    `(apply-split-nest with-advice-1 ,advice-list 1 ,body))

(defmacro without-advice-1 (advice-remove-arguments &rest body)
    `(let ((--without-advice-1-- (list ,@advice-remove-arguments)))
         (if-let (where (apply 'advice-where --without-advice-1--))
             (unwind-protect
                 (progn
                     (apply 'advice-remove --without-advice-1--)
                     ,@body)
                 (setcdr --without-advice-1--
                         (cons where (cdr --without-advice-1--)))
                 (apply 'advice-add --without-advice-1--))
             ,@body)))

(defmacro without-advice (advice-list &rest body)
    `(apply-split-nest without-advice-1 ,advice-list 1 ,body))

(defmacro without-advice-all-1 (symbol &rest body)
    `(let ((advice-list (advice-list ,symbol)))
         (unwind-protect
             (progn
                 (advice-remove-all ,symbol)
                 ,@body)
             (dolist (advice advice-list)
                 (apply 'advice-add ,symbol advice)))))

(defmacro without-advice-all (symbol-list &rest body)
    `(apply-split-nest without-advice-all-1 ,symbol-list 1 ,body))


(defmacro with-hook-1 (add-hook-arguments &rest body)
    `(let ((--with-hook-1-- (list ,@add-hook-arguments)))
         (unwind-protect
             (progn
                 (condition-case error
                     (apply 'add-hook --with-hook-1--)
                     (wrong-number-of-arguments
                         (setq --with-hook-1-- nil)
                         (signal (car error) (cdr error))))
                 ,@body)
             (when --with-hook-1--
                 (setcdr (cdr --with-hook-1--) (cdddr --with-hook-1--))
                 (apply 'remove-hook --with-hook-1--)))))

(defmacro with-hook (hook-list &rest body)
    `(apply-split-nest with-hook-1 ,hook-list 1 ,body))


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
            (with-advice (('ask-user-about-supersession-threat
                              :override (lambda (_) (setq differs t))))
                (with-buffer-modified-p nil
                    (set-buffer-modified-p t)
                    differs)))))


(defun refresh-modified-state (&optional buffer)
    (interactive)
    (setq-if-nil buffer (current-buffer))
    (with-current-buffer buffer
        (when buffer-file-name
            (let ((differs (if (file-exists-p buffer-file-name)
                               (buffer-differs-from-visited-file-p)
                               (> (buffer-size) 0))))
                (with-advice (('ask-user-about-supersession-threat
                                  :override 'ignore))
                    (set-buffer-modified-p differs)))
            (set-visited-file-modtime))))


(defun plist-put-default (plist property default-value &optional predicate)
    (if (plist-member plist property predicate)
        plist
        (plist-put plist property default-value predicate)))


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

(defun dlist-last (dlist)
    (let ((next))
        (while (setq next (dlist-cdr dlist))
            (setq dlist next))
        dlist))


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


(defun point-position-with-scroll ()
    (list
        (point)
        (window-start)
        (window-vscroll nil t)
        (window-hscroll)))

(defun jump-to-position-with-scroll (position-with-scroll)
    (let-unpack ((position start vscroll hscroll) position-with-scroll)
        (goto-char position)
        (let ((window (selected-window)))
            (set-window-start window start t)
            (set-window-vscroll window vscroll t)
            (set-window-hscroll window hscroll))))

(defun point-marker-with-scroll ()
    (list
        (point-marker)
        (window-start)
        (window-vscroll nil t)
        (window-hscroll)))

(defun jump-to-marker-with-scroll (marker-with-scroll)
    (let-unpack ((marker start vscroll hscroll) marker-with-scroll)
        (if-let (buffer (marker-buffer marker))
            (switch-to-buffer buffer)
            (user-error "Buffer no longer exists"))
        (goto-char marker)
        (let ((window (selected-window)))
            (set-window-start window start t)
            (set-window-vscroll window vscroll t)
            (set-window-hscroll window hscroll))))


(defun run-pre-command-hook ()
    (run-hook-wrapped 'pre-command-hook 'pre-command-hook--wrap))
(defun pre-command-hook--wrap (function)
    (let ((format (format "Error in pre-command-hook %S: %%S" function)))
        (condition-case error
            (funcall function)
            (error
                (remove-hook 'pre-command-hook function)
                (message format error))))
    nil)
(defun run-post-command-hook ()
    (run-hook-wrapped 'post-command-hook 'post-command-hook--wrap))
(defun post-command-hook--wrap (function)
    (let ((format (format "Error in post-command-hook %S: %%S" function)))
        (condition-case error
            (funcall function)
            (error
                (remove-hook 'post-command-hook function)
                (message format error))))
    nil)


(defun read-key-sequence-in-keymap (keymap prompt &rest arguments)
    (let ((overriding-terminal-local-map nil)
          (overriding-local-map keymap)
          (saved-global-map (current-global-map)))
        (unwind-protect
            (progn
                (use-global-map (make-sparse-keymap))
                (apply 'read-key-sequence prompt arguments))
            (use-global-map saved-global-map))))


(defun command-execute-in-keymap (keymap &optional prefix run-hooks)
    (condition-case _error
        (with-hook (('prefix-command-echo-keystrokes-functions
                        (lambda-let (prefix) ()
                            (when (length> prefix 0)
                                (key-description prefix)))))
            (let* ((keys (read-key-sequence-in-keymap keymap nil))
                   (binding (lookup-key keymap keys t)))
                (when run-hooks
                    (run-post-command-hook)
                    (run-pre-command-hook))
                (setq last-command-event (aref keys (1- (length keys))))
                (if binding
                    (command-execute binding)
                    (with-advice (('this-single-command-keys
                                      :filter-return
                                      (apply-partially 'vconcat prefix)))
                        (undefined)))))
        (quit)))


(defun add-single-use-hook--wrapper (remove-arguments function &rest arguments)
    (unwind-protect
        (apply function arguments)
        (apply 'remove-hook remove-arguments)))

(defun add-single-use-hook (hook function &optional depth local)
    (let* ((remove-arguments (list hook nil local))
           (wrapper          (apply-partially 'add-single-use-hook--wrapper
                                 remove-arguments function)))
        (setcar (cdr remove-arguments) wrapper)
        (add-hook hook wrapper depth local)))


(defun become-command (command)
    (let ((this-command command))
        (call-interactively command)))


(defun identity+ignore (value &rest _)
    value)

(defun ignore+return (value)
    (lambda-let (value) (&rest _) value))


(defun file-size (filename)
    (file-attribute-size (file-attributes filename)))


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


(defun funcall-process (program &rest arguments)
    (with-temp-buffer
        (let* ((result (apply 'call-process program nil t nil arguments))
               (output (string-remove-suffix "\n" (buffer-string))))
            (list result output))))

(defmacro apply-process (&rest command)
    `(apply 'funcall-process ,@command))


(defun buffer-process-send-string (string)
    (process-send-string (get-buffer-process (current-buffer)) string))


(defmacro setq-if-nil-1 (symbol value)
    `(if ,symbol
         ,symbol
         (setq ,symbol ,value)))

(defmacro setq-if-nil (&rest symbol-value-pairs)
    `(apply-split setq-if-nil-1 ,symbol-value-pairs 2))

(defmacro setf-if-nil-1 (place value)
    `(if ,place
         ,place
         (setf ,place ,value)))

(defmacro setf-if-nil (&rest place-value-pairs)
    `(apply-split setf-if-nil-1 ,place-value-pairs 2))


(defun string-to-number-or-nil (string)
    (when (length> string 0)
        (string-to-number string)))

(defun number-to-string-or-nil (number)
    (when number
        (number-to-string number)))


(defun string-prefixes (string &optional shortest longest)
    (if (not shortest)
        (setq shortest 1)
        (when (< shortest 0)
            (setq shortest 0)))
    (when (or (not longest) (length< string longest))
        (setq longest (length string)))
    (let ((prefixes ())
          (size     longest))
        (while (>= size shortest)
            (push (substring string 0 size) prefixes)
            (setq size (1- size)))
        prefixes))


(defun regexp-opt-prefix-group (string &optional shortest longest paren lax)
    (let ((strings (string-prefixes string shortest longest)))
        (regexp-opt-group strings paren lax)))


(defun next-window-other-buffer (&optional window minibuffer all-frames)
    (let* ((window (next-window window minibuffer all-frames))
           (buffer (window-buffer window)))
        (while (and (eq buffer (current-buffer))
                    (not (eq window (selected-window))))
            (setq window (next-window window minibuffer all-frames))
            (setq buffer (window-buffer window)))
        (if (eq window (selected-window))
            nil
            window)))


(defun read-other-buffer (prompt)
    (with-advice (('confirm-nonexistent-file-or-buffer :override 'always))
        (read-buffer-to-switch prompt)))


(defun write-file-no-visit (filename)
    (without-restriction
        (write-region 1 (point-max) filename)))


(defmacro with-nested-command-state (&rest body)
    (declare (indent 0))
    `(let ((this-command      this-command)
           (real-this-command real-this-command)
           (last-command      last-command)
           (real-last-command real-last-command))
         ,@body))

(defun funcall-with-nested-command-state (function &rest arguments)
    (with-nested-command-state
        (apply function arguments)))


(setq use-short-answers t)
(define-key y-or-n-p-map "\C-m" 'act)
(define-key y-or-n-p-map [return] 'act)
(advice-add 'y-or-n-p :around 'funcall-with-nested-command-state)
(defun read-multiple-choice-name (choice)
    (let ((raw   (cdr choice))
          (fancy (cdr (rmc--add-key-description choice))))
        (if raw
            (if (length= raw 0)
                (substring fancy 0 -1)
                fancy)
            (substring fancy 0 -4))))
(defun hack-read-multiple-choice (prompt choices &rest _)
    (let ((names (mapcar 'read-multiple-choice-name choices)))
        (setq prompt (concat prompt " (" (string-join names ", ") "): ")))
    (let ((map (make-sparse-keymap)))
        (dolist (choice choices)
            (let ((key (char-to-string (car choice))))
                (define-key map key
                    (lambda-let (key) ()
                        (interactive)
                        (insert key)
                        (exit-minibuffer)))))
        (define-key map [t]
            (lambda ()
                (interactive)
                (user-error "invalid choice")))
        (let ((choice (with-nested-command-state
                          (read-from-minibuffer prompt nil map))))
                (assoc (string-to-char choice) choices))))
(advice-add 'read-multiple-choice :override 'hack-read-multiple-choice)
(defun confirm-p (prompt)
    (let ((map (make-sparse-keymap)))
        (define-key map "\C-m" 'exit-minibuffer)
        (define-key map [return] 'exit-minibuffer)
        (define-key map [t]
            (lambda ()
                (interactive)
                (user-error
                    (concat
                        (propertize "RET" 'face 'help-key-description)
                        " to confirm"))))
        (if (with-nested-command-state
                (read-from-minibuffer (concat prompt " ") nil map))
            t
            nil)))


(defmacro defer-input (&rest body)
    `(let ((defer-input-map (make-sparse-keymap))
           (defer-input--events (list 'unused)))
         (define-key defer-input-map [t]
             (lambda-let (defer-input--events) ()
                 (interactive)
                 (nconc defer-input--events
                     (listify-key-sequence (this-command-keys)))))
         (let ((overriding-terminal-local-map defer-input-map))
             ,@body)
         (pop defer-input--events)
         (setq unread-command-events
             (nconc defer-input--events unread-command-events))))


(defun hack-save-buffers-kill-emacs (save-buffers-kill-emacs &rest arguments)
    (with-advice (('save-some-buffers :override 'ignore))
        (apply save-buffers-kill-emacs arguments)))
(advice-add 'save-buffers-kill-emacs :around 'hack-save-buffers-kill-emacs)


(defmacro use-packages (&rest packages-:config-body)
    (let ((head (cons 'use-package packages-:config-body))
          (tail packages-:config-body))
        (unless (eq (car tail) :config)
            (while (and tail (not (eq (cadr tail) :config)))
                (let-uncons (package next tail)
                    (when next
                        (setcdr tail (list :config (cons 'use-package next))))
                    (setq tail next))))
        head))

(use-package cl-seq
    :config
    (defun any (seq &rest cl-reduce-keyword-arguments)
        (setq cl-reduce-keyword-arguments
            (plist-put-default cl-reduce-keyword-arguments :initial-value nil))
        (apply 'cl-reduce (lambda (a b) (or a b)) seq
               cl-reduce-keyword-arguments))
    (defun all (seq &rest cl-reduce-keyword-arguments)
        (setq cl-reduce-keyword-arguments
            (plist-put-default cl-reduce-keyword-arguments :initial-value t))
        (apply 'cl-reduce (lambda (a b) (and a b)) seq
               cl-reduce-keyword-arguments)))

(defun variadic-mapcar (function &rest lists)
    (setq lists (nreverse lists))
    (let ((results ()))
        (while (any lists)
            (let ((arguments ())
                  (head lists))
                (while head
                    (push (pop (car head)) arguments)
                    (pop head))
                (push (apply function arguments) results)))
        (nreverse results)))

(use-package help
    :config
    (setq help-window-select t)
    (define-key help-map "t" 'describe-face)
    (define-key help-map "g" nil)
    (define-key help-map "r" nil)
    (defun fixed-help-view-source (&rest _)
        (set-window-start (selected-window) (point)))
    (advice-add 'help-function-def--button-function
        :after 'fixed-help-view-source)
    (define-key help-mode-map "\C-m" 'help-view-source)
    (defun independent-help--name (type formatter arguments)
        (if arguments
            (format "*Help (%s: %s)*" type (apply formatter arguments))
            (format "*Help (%s)*" type)))
    (defun independent-help--advice (type formatter function &rest arguments)
        (let ((name (independent-help--name type formatter arguments)))
            (with-advice (('help-buffer
                              :override
                              (lambda-let (name) ()
                                  (get-buffer-create name)
                                  name)))
                (apply function arguments))))
    (defun independent-help (type formatter)
        (apply-partially 'independent-help--advice type formatter))
    (advice-add 'describe-function
        :around (independent-help "function" 'identity))
    (advice-add 'describe-variable
        :around (independent-help "variable" 'identity+ignore))
    (advice-add 'describe-face
        :around (independent-help "face"
            (lambda (face &rest _)
                (if (listp face)
                    (substring (format "%s" face) 1 -1)
                    face))))
    (advice-add 'describe-package
        :around (independent-help "package"
            (lambda (package)
                (if (package-desc-p package)
                    (package-desc-name package)
                    package)))))

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
(defvar-local histdir-buffer-local-history-list nil)
(defvar-local histdir-buffer-local-history--head nil)
(defun histdir--update-buffer-local-history-pointers (history)
    (let ((shared  (cdr (histdir-history-table history)))
          (buffers (histdir-history-buffers history)))
        (dolist (buffer buffers)
            (if (buffer-live-p buffer)
                (with-current-buffer buffer
                    (unless (cadr histdir-buffer-local-history--head)
                        (setq histdir-buffer-local-history--head
                              (dlist (cadr shared))))
                    (setcdr (car histdir-buffer-local-history--head) shared)
                    (setcdr (cdr histdir-buffer-local-history--head)
                            (cdr shared))
                    (setq histdir-buffer-local-history-list
                          (cdr histdir-buffer-local-history--head)))
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
(defvar-local histdir-buffer-local-history--position nil)
(defun histdir-input-add (input &optional deduplicate)
    (setq histdir-buffer-local-history--position nil)
    (unless (equal (string-trim input) "")
        (setcar (cdr histdir-buffer-local-history--head) input)
        (histdir-add input deduplicate)))
(defun histdir-input--select (position)
    (setq histdir-buffer-local-history--position position)
    (replace-command
        (if (not position)
            ""
            (dlist-car position))))
(defun histdir-input--cycle-older (position)
    (if position
        (let ((older (dlist-cdr position)))
            (while (and older (not (dlist-car older)))
                (setq older (dlist-cdr older)))
            older)
        (let ((latest-local  histdir-buffer-local-history--head)
              (latest-synced (dlist-cdr histdir-buffer-local-history--head)))
            (if (equal (dlist-car latest-local) (dlist-car latest-synced))
                latest-synced
                latest-local))))
(defun histdir-input--older (position)
    (if-let (older (histdir-input--cycle-older position))
        older
        position))
(defun histdir-input--newer (position)
    (let ((newer (caar position)))
        (while (and newer (not (dlist-car newer)))
            (setq newer (caar newer)))
        newer))
(defun histdir-input--cycle-newer (position)
    (let ((history (histdir--history)))
        (if position
            (histdir-input--newer position)
            (histdir-history--last history))))
(defun histdir-input-older ()
    (interactive)
    (histdir-input--select
        (histdir-input--older
            histdir-buffer-local-history--position)))
(defun histdir-input-newer ()
    (interactive)
    (histdir-input--select
        (histdir-input--newer
            histdir-buffer-local-history--position)))
(defun histdir-input-cycle-older ()
    (interactive)
    (histdir-input--select
        (histdir-input--cycle-older
            histdir-buffer-local-history--position)))
(defun histdir-input-cycle-newer ()
    (interactive)
    (histdir-input--select
        (histdir-input--cycle-newer
            histdir-buffer-local-history--position)))

(use-package eshell
    :config
    (setq eshell-highlight-prompt nil)
    (setq eshell-prompt-regexp "^[$#] ")
    (setq eshell-prompt-function
        (lambda ()
            (propertize
                (if (= (user-uid) 0) "# " "$ ")
                'read-only t
                'field 'prompt
                'front-sticky '(read-only)
                'rear-nonsticky t)))
    (setq eshell-banner-message "")
    (setq eshell-history-size 0)
    (advice-add 'eshell-hist-initialize
        :before
        (lambda (&rest _)
            (setq histdir "~/.history/eshell")))
    (advice-add 'eshell-read-history
        :override
        (lambda (&rest _)
            (histdir-watch+read)))
    (advice-add 'eshell-add-input-to-history
        :override
        (lambda (input)
            (histdir-input-add input t)))
    (advice-add 'eshell-write-history :override (lambda (&rest _)))
    (defun in-eshell-prompt-p ()
        (eq (get-text-property (point) 'field) 'prompt))
    (defun in-eshell-scrollback-p ()
        (text-property-any (point) (point-max) 'field 'prompt))
    (defun hack-insert-before-markers-and-inherit (arguments)
        (if (and (equal (length arguments) 1) (equal (car arguments) ?\n))
            (list (propertize "\n" 'field 'end-of-input 'rear-nonsticky t))
            arguments))
    (defun fixed-eshell-send-input ()
        (interactive)
        (if (in-eshell-scrollback-p)
            (message "not in input")
            (with-advice (('insert-before-markers-and-inherit
                              :filter-args
                              'hack-insert-before-markers-and-inherit))
                (eshell-send-input))))
    (defun fixed-eshell-up-arrow ()
        (interactive)
        (when (in-eshell-scrollback-p)
            (goto-char (point-max)))
        (condition-case _error
            (progn
                (previous-line)
                (when (in-eshell-prompt-p)
                    (move-end-of-line 1)
                    (goto-char (field-beginning)))
                (when (in-eshell-scrollback-p)
                    (goto-char (point-max))
                    (histdir-input-older)))
            (beginning-of-buffer
                (histdir-input-older))))
    (defun fixed-eshell-down-arrow ()
        (interactive)
        (when (in-eshell-scrollback-p)
            (goto-char (point-max)))
        (condition-case _error
            (next-line)
            (end-of-buffer
                (histdir-input-newer))))
    (advice-add 'eshell-interrupt-process
        :before
        (lambda (&rest _)
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
    (add-to-list 'eshell-modules-list 'eshell-tramp))
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
    (define-key dired-mode-map "I" 'dired-kill-subdir)
    (when termux
        (define-key dired-mode-map [mouse-2] 'mouse-set-point))
    (define-key dired-mode-map "a" 'dired-hide-details-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    (define-key dired-mode-map "r" 'dired-do-rename)
    (define-key dired-mode-map "R" nil)
    (define-key dired-mode-map "c" 'dired-do-copy)
    (define-key dired-mode-map "C" 'dired-do-compress-to)
    (defun dired-goto-first-file ()
        (interactive)
        (goto-char 1)
        (let ((last-point (point)))
            (until (or (dired-file-name-at-point)
                       (= last-point (point)))
                (setq last-point (point))
                (dired-next-line 1)))
        (dired-move-to-filename))
    (defun dired-goto-last-file ()
        (interactive)
        (goto-char (point-max))
        (redisplay)
        (dired-previous-line 1))
    (defun dired-filename-search-forward (bound)
        (let ((start-of-search (point)))
            (while (and (not (eobp))
                        (or (eolp)
                            (not (dired-file-name-at-point))))
                (dired-next-line 1))
            (dired-move-to-filename)
            (if (and (not (eobp))
                     (>= (point) start-of-search))
                (let ((start-of-match (point-marker)))
                    (end-of-line)
                    (if (<= (point) bound)
                        (let ((end-of-match (point-marker)))
                            (set-match-data (list start-of-match end-of-match))
                            (point))
                        (goto-char start-of-search)
                        nil))
                (goto-char start-of-search)
                nil)))
    (defun independent-dired (directory &rest arguments)
        (let* ((directory (expand-file-name directory))
               (directory (file-name-as-directory directory))
               (existing (assoc directory dired-buffers))
               (dired-buffers (remq existing dired-buffers)))
            (apply 'dired directory arguments)))
    (defun reuse-independent-dired (name directory &rest arguments)
        (let ((buffer (get-buffer name)))
            (if buffer
                (pop-to-buffer-same-window buffer)
                (setq buffer (independent-dired directory))
                (rename-buffer name))
            buffer)))

(when termux
    (use-package image-mode
        :config
        (defun hack-image-mode ()
            (add-single-use-hook 'post-command-hook 'kill-buffer)
            (browse-url-xdg-open (buffer-file-name)))
        (advice-add 'image-mode :override 'hack-image-mode)))

(defun git-repo-root ()
    (let ((result (funcall-process "git" "rev-parse" "--absolute-git-dir")))
        (let-unpack ((status output) result)
            (if (equal status 0)
                (abbreviate-file-name (file-name-directory output))
                (unless (string-prefix-p "fatal: not a git repository" output)
                    (let ((inhibit-message t))
                        (message
                            "git rev-parse in %S exit=%S output=%S"
                            default-directory result output)))
                nil))))

(use-package vc
    :config
    (defun fixed-vc-deduce-backend (vc-deduce-backend &rest arguments)
        (if (or (derived-mode-p 'eshell-mode 'eat-mode)
                (bound-and-true-p with-editor-mode))
            (ignore-errors (vc-responsible-backend default-directory))
            (apply vc-deduce-backend arguments)))
    (advice-add 'vc-deduce-backend :around 'fixed-vc-deduce-backend)
    (defun fixed-vc-git-root (file)
        (let ((file (expand-file-name file)))
            (unless (file-directory-p file)
                (setq file (file-name-directory file)))
            (let ((default-directory file))
                (git-repo-root))))
    (advice-add 'vc-git-root :override 'fixed-vc-git-root)
    (add-hook 'vc-annotate-mode-hook
        (lambda ()
            (setq truncate-lines nil)))
    (defun hack-vc-dir (&rest _)
        (error "vc-dir called"))
    (advice-add 'vc-dir :override 'hack-vc-dir))

(use-package diff-mode
    :config
    (set-face-background 'diff-added "#005000")
    (set-face-background 'diff-removed "#500000")
    (set-face-background 'diff-refine-added "#008000")
    (set-face-background 'diff-refine-removed "#800000"))

(use-package smerge-mode
    :config
    (set-face-background 'smerge-lower "#005000")
    (set-face-background 'smerge-upper "#500000")
    (set-face-background 'smerge-base "#505000")
    (set-face-background 'smerge-refined-added "#008000")
    (set-face-background 'smerge-refined-removed "#800000"))

(use-package display-fill-column-indicator
    :config
    (setq-default display-fill-column-indicator-column 79)
    (defun toggle-show-80+-characters ()
        (interactive)
        (when (eq last-command 'toggle-show-80+-characters)
            (display-fill-column-indicator-mode 'toggle)
            (if display-fill-column-indicator-mode
                (highlight-regexp   ".\\{79\\}\\(.*\\)" 'hi-yellow 1)
                (unhighlight-regexp ".\\{79\\}\\(.*\\)")))
        (message "display-fill-column-indicator-mode: %s"
            display-fill-column-indicator-mode)))

(use-package gnutls
    :config
    (setq gnutls-verify-error t))

(use-package package
    :config
    (defun fixed-package--with-response-buffer-1
            (package--with-response-buffer-1 url body &rest keywords)
        (setq body
            (lambda-let (body) ()
                (funcall body)
                (package-menu--post-refresh)))
        (apply package--with-response-buffer-1 url body :async t keywords))
    (advice-add 'package--with-response-buffer-1
        :around 'fixed-package--with-response-buffer-1))

(use-package hexl
    :config
    (defun fixed-hexl-insert-char-1 (hexl-insert-char character)
        (let ((start (point)))
            (funcall hexl-insert-char character 1)
            (when (<= (point) start)
                (if (>= (current-column) (hexl-ascii-start-column))
                    (forward-char 1)
                    (forward-char 2)))))
    (defun fixed-hexl-insert-char (hexl-insert-char character count)
        (dotimes (_ count)
            (fixed-hexl-insert-char-1 hexl-insert-char character)))
    (advice-add 'hexl-insert-char :around 'fixed-hexl-insert-char)
    (defun hexl-nibble-insert-1 (nibble)
        (setq nibble (hexl-hex-char-to-integer nibble))
        (let ((start (point)))
            (hexl-goto-address (hexl-current-address))
            (let ((byte (hexl-char-after-point))
                  (on-right-nibble (< (point) start)))
                (if on-right-nibble
                    (setq byte (logand byte #xF0))
                    (setq byte (logand byte #x0F))
                    (setq nibble (ash nibble 4)))
                (setq byte (logior byte nibble))
                (if on-right-nibble
                    (hexl-insert-char byte 1)
                    (save-excursion
                        (hexl-insert-char byte 1))
                    (forward-char)))))
    (defun hexl-nibble-insert (nibble &optional count)
        (unless count
            (setq count 1))
        (dotimes (_ count)
            (hexl-nibble-insert-1 nibble)))
    (defun fixed-hexl-self-insert-command (count &optional character)
        (interactive "p")
        (unless character
            (setq character last-command-event))
        (if (>= (current-column) (hexl-ascii-start-column))
            (hexl-insert-multibyte-char character count)
            (hexl-nibble-insert character count)))
    (define-key hexl-mode-map [remap self-insert-command]
        'fixed-hexl-self-insert-command)
    (defun hexl-switch-column ()
        (interactive)
        (let ((ascii-position (overlay-start hexl-ascii-overlay)))
            (if (equal (point) ascii-position)
                (hexl-goto-address (hexl-current-address))
                (goto-char ascii-position))))
    (defun fixed-hexl-save-buffer ()
        (let ((scratch (get-buffer-create (concat " hexl " (buffer-name)) t))
              (buffer  (current-buffer))
              (path    buffer-file-name))
            (unwind-protect
                (with-current-buffer scratch
                    (insert-buffer-substring buffer)
                    (dehexlify-buffer)
                    (write-file-no-visit path))
                (kill-buffer scratch))
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
        t)
    (advice-add 'hexl-save-buffer :override 'fixed-hexl-save-buffer))

(use-package man
    :config
    (setq Man-notify-method 'pushy))

(use-package calendar
    :config
    (defvar calendar-months-before-current (if termux 0 1))
    (defvar calendar-months-after-current 1)
    (defun calendar-generate (month year)
        (setq displayed-month month)
        (setq displayed-year year)
        (erase-buffer)
        (calendar-increment-month month year (- calendar-months-before-current))
        (dotimes (index (+ calendar-months-before-current
                           1
                           calendar-months-after-current))
            (calendar-generate-month month year
                (+ calendar-left-margin
                    (* calendar-month-width index)))
            (calendar-increment-month month year 1)))
    (defun calendar-date-is-visible-p (date)
        (and (calendar-date-is-valid-p date)
             (let* ((year  (calendar-extract-year  date))
                    (month (calendar-extract-month date))
                    (delta (calendar-interval displayed-month displayed-year
                                              month           year)))
                 (<= (- calendar-months-before-current)
                     delta
                     calendar-months-after-current))))
    (defun hack-calendar-cursor-to-visible-date (arguments)
        (advice-remove 'calendar-cursor-to-visible-date
            'hack-calendar-cursor-to-visible-date)
        (setcar arguments (list displayed-month 1 displayed-year))
        arguments)
    (defun hack-calendar-scroll-left
            (calendar-scroll-left &rest arguments)
        (with-advice (('calendar-cursor-to-visible-date
                          :filter-args
                          'hack-calendar-cursor-to-visible-date))
            (apply calendar-scroll-left arguments)))
    (advice-add 'calendar-scroll-left :around 'hack-calendar-scroll-left)
    (defun hack-calendar-displayed-date (function &rest arguments)
        (let ((displayed-month displayed-month)
              (displayed-year  displayed-year))
            (calendar-increment-month displayed-month displayed-year
                (- 1 calendar-months-before-current))
            (apply function arguments)))
    (advice-add 'calendar-cursor-to-date
        :around 'hack-calendar-displayed-date)
    (advice-add 'calendar-cursor-to-visible-date
        :around 'hack-calendar-displayed-date))

(define-error 'datetime-bad "Bad datetime" 'user-error)
(define-error 'datetime-bad-month "Bad month" 'datetime-bad)
(define-error 'datetime-bad-day "Bad day" 'datetime-bad)
(define-error 'datetime-bad-ordinal-day "Bad ordinal day" 'datetime-bad)
(define-error 'datetime-bad-hour "Bad hour" 'datetime-bad)
(define-error 'datetime-bad-minute "Bad minute" 'datetime-bad)
(define-error 'datetime-bad-second "Bad second" 'datetime-bad)

(defun datetime--valid-second-p (second)
    (when (<= 0 second 59)
        second))

(defun datetime--validate-second (second)
    (or (datetime--valid-second-p second)
        (signal 'datetime-bad-second (list second))))

(defun datetime--valid-minute-p (minute)
    (when (<= 0 minute 59)
        minute))

(defun datetime--validate-minute (minute)
    (or (datetime--valid-minute-p minute)
        (signal 'datetime-bad-minute (list minute))))

(defun datetime--valid-hour-p (hour)
    (when (<= 0 hour 23)
        hour))

(defun datetime--validate-hour (hour)
    (or (datetime--valid-hour-p hour)
        (signal 'datetime-bad-hour (list hour))))

(defun datetime--valid-month-p (month)
    (when (<= 1 month 12)
        month))

(defun datetime--validate-month (month)
    (or (datetime--valid-month-p month)
        (signal 'datetime-bad-month (list month))))

(defun datetime-is-leap (year)
    (and (= (mod year 4) 0)
         (or (> (mod year 100) 0)
             (= (% year 400) 0))))

(defconst datetime--days-in-month
    ;; 0 Ja Fe Mr Ap My Jn Jl Au Se Oc No De
    [  0 31 28 31 30 31 30 31 31 30 31 30 31])

(defun datetime--days-in-month (year month)
    (if (and (= month 2)
             (datetime-is-leap year))
        (1+ (aref datetime--days-in-month month))
        (aref datetime--days-in-month month)))

(defun datetime-days-in-month (year month)
    (datetime--validate-month month)
    (datetime--days-in-month year month))

(defun datetime--valid-day-p (year month day)
    (when (and (datetime--valid-month-p month)
               (<= 1 day (datetime--days-in-month year month)))
        day))

(defun datetime--validate-day (year month day)
    (or (datetime--valid-day-p year month day)
        (signal 'datetime-bad-day (list day))))

(defun datetime--valid-month+day-p (year month day)
    (when (datetime--valid-day-p year month day)
        t))

(defun datetime--validate-month+day (year month day)
    (datetime--validate-month month)
    (datetime--validate-day year month day)
    t)

(defun datetime-days-in-year (year)
    (if (datetime-is-leap year)
        366
        365))

(defconst datetime--days-until-month--nonleap
    ;; 0 Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    [  0   0  31  59  90 120 151 181 212 243 273 304 334])

(defconst datetime--days-until-month--leap
    ;; 0 Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
    [  0   0  31  60  91 121 152 182 213 244 274 305 335])

(defun datetime-days-until-month (year month)
    (datetime--validate-month month)
    (if (datetime-is-leap year)
        (aref datetime--days-until-month--leap month)
        (aref datetime--days-until-month--nonleap month)))

(defun datetime-ordinal-day (year month day)
    (datetime--validate-month+day year month day)
    (+ (datetime-days-until-month year month) day))

(defconst datetime--month-of-ordinal-day--nonleap
    (vconcat
        [0]
        (make-vector 31 1)
        (make-vector 28 2)
        (make-vector 31 3)
        (make-vector 30 4)
        (make-vector 31 5)
        (make-vector 30 6)
        (make-vector 31 7)
        (make-vector 31 8)
        (make-vector 30 9)
        (make-vector 31 10)
        (make-vector 30 11)
        (make-vector 31 12)))

(defconst datetime--month-of-ordinal-day--leap
    (vconcat
        [0]
        (make-vector 31 1)
        (make-vector 29 2)
        (make-vector 31 3)
        (make-vector 30 4)
        (make-vector 31 5)
        (make-vector 30 6)
        (make-vector 31 7)
        (make-vector 31 8)
        (make-vector 30 9)
        (make-vector 31 10)
        (make-vector 30 11)
        (make-vector 31 12)))

(defun datetime-month-of-ordinal-day (year day)
    (let ((table (if (datetime-is-leap year)
                     datetime--month-of-ordinal-day--leap
                     datetime--month-of-ordinal-day--nonleap)))
        (unless (< 0 day (length table))
            (signal 'datetime-bad-ordinal-day (list day)))
        (aref table day)))

(defmacro datetime--year+ ()
    `(unless (= year+ 0)
         (setq year (+ year year+))))

(defmacro datetime--month+ ()
    `(unless (= month+ 0)
         (let ((days-in-month (datetime-days-in-month year month)))
             (setq month (+ month month+))
             (if (> month 0)
                 (setq year+ (/ (- month 1) 12))
                 (setq month (- month 12))
                 (setq year+ (/ month 12)))
             (setq month (1+ (mod (1- month) 12)))
             (datetime--year+)
             (when day
                 (setq day (min day (datetime--days-in-month year month)))))))

(defmacro datetime--day+ ()
    `(unless (= day+ 0)
         (setq day (datetime-ordinal-day year month day))
         (setq day (+ day day+))
         (if (> day 0)
             (while-let ((days-in-year (datetime-days-in-year year))
                         (_            (> day days-in-year)))
                 (setq day (- day days-in-year))
                 (setq year (1+ year)))
             (while-let ((_            (setq year (1- year)))
                         (days-in-year (datetime-days-in-year year))
                         (_            (setq day (+ days-in-year day)))
                         (_            (< day 1)))))
         (setq month (datetime-month-of-ordinal-day year day))
         (setq day (- day (datetime-days-until-month year month)))))

(defmacro datetime--hour+ ()
    `(unless (= hour+ 0)
         (datetime--validate-hour hour)
         (setq hour (+ hour hour+))
         (if (>= hour 0)
             (setq day+ (/ hour 24))
             (setq hour (- hour 24))
             (setq day+ (/ hour 24)))
         (setq hour (mod hour 24))
         (datetime--day+)))

(defmacro datetime--minute+ ()
    `(unless (= minute+ 0)
         (datetime--validate-minute minute)
         (setq minute (+ minute minute+))
         (if (>= minute 0)
             (setq hour+ (/ minute 60))
             (setq minute (- minute 60))
             (setq hour+ (/ minute 60)))
         (setq minute (mod minute 60))
         (datetime--hour+)))

(defmacro datetime--second+ ()
    `(unless (= second+ 0)
         (datetime--validate-second second)
         (setq second (+ second second+))
         (if (>= second 0)
             (setq minute+ (/ second 60))
             (setq second (- second 60))
             (setq minute+ (/ second 60)))
         (setq second (mod second 60))
         (datetime--minute+)))

(defun datetime-day-of-week (year month day)
    (datetime--validate-month+day year month day)
    (when (< year 1)
        (setq year (1- year)))
    (calendar-day-of-week (list month day year)))

(defun fixed-decoded-time-add (time delta)
    (let-unpack ((second  minute  hour  day  month  year dst zone) time
                 (second+ minute+ hour+ day+ month+ year+) delta)
        (setq-if-nil year+   0)
        (setq-if-nil month+  0)
        (setq-if-nil day+    0)
        (setq-if-nil hour+   0)
        (setq-if-nil minute+ 0)
        (setq-if-nil second+ 0)
        (datetime--year+)
        (datetime--month+)
        (when (and (not (= year+ month+ 0))
                   (equal month 2)
                   (equal day 29)
                   (not (datetime-is-leap year)))
            (setq day 28))
        (datetime--day+)
        (datetime--hour+)
        (datetime--minute+)
        (datetime--second+)
        (list second minute hour day month year
            (when (and year month day)
                (condition-case nil
                    (datetime-day-of-week year month day)
                    (datetime-bad)))
            dst zone)))

(defun decoded-time-negate (delta)
    (list
        (when-let (second (decoded-time-second delta))
            (- second))
        (when-let (minute (decoded-time-minute delta))
            (- minute))
        (when-let (hour   (decoded-time-hour   delta))
            (- hour))
        (when-let (day    (decoded-time-day    delta))
            (- day))
        (when-let (month  (decoded-time-month  delta))
            (- month))
        (when-let (year   (decoded-time-year   delta))
            (- year))))

(defun decoded-time-iterate (time delta count)
    (when (< count 0)
        (setq count (- count))
        (setq delta (decoded-time-negate delta)))
    (dotimes (_ count time)
        (setq time (fixed-decoded-time-add time delta))))

(defun decoded-time-days-until-weekday (time weekday)
    (let ((year  (decoded-time-year  time))
          (month (decoded-time-month time))
          (day   (decoded-time-day   time)))
        (let* ((start (datetime-day-of-week year month day)))
            (% (- (+ weekday 7) start) 7))))

(defvar datetime-parse-two-digit-year-base 2000)

(defface datetime-read-preview-year-face
    '((t :foreground "#80FFFF" :weight bold)) "")
(defface datetime-read-preview-month-face
    '((t :foreground "#C040FF" :weight bold)) "")
(defface datetime-read-preview-day-face
    '((t :foreground "#40FF40" :weight bold)) "")
(defface datetime-read-preview-hour-face
    '((t :foreground "#FFA060" :weight bold)) "")
(defface datetime-read-preview-minute-face
    '((t :foreground "#FFFF80" :weight bold)) "")
(defface datetime-read-preview-second-face
    '((t :foreground "#FF4040" :weight bold)) "")

(defun datetime-parse (string &optional short now)
    (let-unpack ((parsed _) (datetime-parse--loop string short now))
        (let-unpack ((second minute hour day month year) parsed)
            (concat
                (when year
                    (format "%04d" year))
                (when (and year month)
                    (format "%02d" month))
                (when (and year month day)
                    (format "%02d" day))
                (when (and year month day hour)
                    (format "T%02d" hour))
                (when (and year month day hour minute)
                    (format "%02d" minute))
                (when (and year month day hour minute second)
                    (format "%02d" second))))))

(defun datetime-parse--loop (string &optional short now)
    (setq-if-nil now (decode-time (current-time)))
    (let ((parsed (make-decoded-time))
          (bindings (make-decoded-time))
          (offsets (list nil nil nil nil nil nil))
          (previous-parsed nil)
          (previous-bindings nil)
          (integers ())
          (integer nil)
          (integers-in-span 0)
          (integers-before-span 0)
          (words (split-string string))
          word)
        (setq word words)
        (while word
            (unpack (previous-parsed _ previous-bindings)
                    (datetime-parse--bind nil parsed integers))
            (setq previous-parsed
                  (datetime-parse--future-bias nil previous-parsed now))
            (setq integer nil)
            (datetime-parse--1)
            (if integer
                (setq integers-in-span (1+ integers-in-span))
                (setq integers-before-span
                      (+ integers-before-span integers-in-span))
                (setq integers-in-span 0))
            (pop word))
        (unpack (parsed _ bindings)
                (datetime-parse--bind nil parsed integers bindings))
        (unless (and short (not (any (take 6 parsed))))
            (setq parsed (datetime-parse--future-bias nil parsed now)))
        (when short
            (datetime-parse--plug parsed))
        (setq parsed (datetime-parse--floor nil parsed))
        (when short
            (datetime-parse--unplug parsed))
        (setq parsed (fixed-decoded-time-add parsed nil))
        (if (string-suffix-p " " string)
            (setq string (concat (string-join words " ") " "))
            (setq string (string-join words " ")))
        (if (or (and (< (- (length words) integers-before-span) 2)
                     (not (any offsets)))
                (string-suffix-p " " string))
            (list parsed (list nil bindings string))
            (setq previous-parsed
                  (variadic-mapcar
                      (lambda (value binding offset)
                          (if (and binding (not offset))
                              nil
                              value))
                      previous-parsed
                      previous-bindings
                      offsets))
            (list parsed (list previous-parsed bindings string)))))

(defmacro datetime-parse--1 ()
    `(cond
         ((string-match-p "^[0-9]\\{3,\\}$" (car word))
             (setf (decoded-time-year parsed) (string-to-number (car word))))
         ((string-match-p "^[0-9]+$" (car word))
             (setq integer t)
             (setq integers (append integers (list word))))
         ((string-match-p "^[0-9]\\{3,\\}y" (car word))
             (setf (decoded-time-year parsed) (string-to-number (car word))))
         ((string-match-p "^[0-9]+y" (car word))
             (setf (decoded-time-year parsed)
                   (+ (string-to-number (car word))
                      datetime-parse-two-digit-year-base)))
         ((string-match-p "^'[0-9]\\{1,3\\}$" (car word))
             (setf (decoded-time-year parsed)
                   (+ (string-to-number (substring (car word) 1))
                      datetime-parse-two-digit-year-base)))
         ((string-match-p "^[0-9]+mo" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) (string-to-number (car word))))
         ((string-match-p "^[0-9]+\\(d\\|st\\|nd\\|rd\\|th\\)" (car word))
             (setf (decoded-time-day bindings) nil)
             (setf (decoded-time-day parsed) (string-to-number (car word))))
         ((string-match-p "^[0-9]\\{3,\\}-[0-9][0-9]-[0-9][0-9]$" (car word))
             (let-unpack ((year month day) (string-split (car word) "-"))
                 (setf (decoded-time-month bindings) nil)
                 (setf (decoded-time-day   bindings) nil)
                 (setf (decoded-time-year  parsed) (string-to-number year))
                 (setf (decoded-time-month parsed) (string-to-number month))
                 (setf (decoded-time-day   parsed) (string-to-number day))))
         ((string-match-p "^[0-9]\\{3,\\}-[0-9][0-9]$" (car word))
             (let-unpack ((year month day) (string-split (car word) "-"))
                 (setf (decoded-time-month bindings) nil)
                 (setf (decoded-time-year  parsed) (string-to-number year))
                 (setf (decoded-time-month parsed) (string-to-number month))))
         ((string-match-p "^[0-9]+h" (car word))
             (setf (decoded-time-hour bindings) nil)
             (setf (decoded-time-hour parsed) (string-to-number (car word))))
         ((string-match-p "^[0-9]+am" (car word))
             (setf (decoded-time-hour bindings) nil)
             (let ((number (string-to-number (car word))))
                 (when (= number 12)
                     (setq number 0))
                 (setf (decoded-time-hour parsed) number)))
         ((string-match-p "^[0-9]+pm" (car word))
             (setf (decoded-time-hour bindings) nil)
             (let ((number (string-to-number (car word))))
                 (unless (= number 12)
                     (setq number (+ number 12)))
                 (setf (decoded-time-hour parsed) number)))
         ((string-match-p "^[0-9]+m" (car word))
             (setf (decoded-time-minute bindings) nil)
             (setf (decoded-time-minute parsed) (string-to-number (car word))))
         ((string-match-p "^[0-9]+s" (car word))
             (setf (decoded-time-second bindings) nil)
             (setf (decoded-time-second parsed) (string-to-number (car word))))
         ((string-match-p "^[0-9][0-9]?:[0-9][0-9]:[0-9][0-9]$" (car word))
             (let-unpack ((hour minute second) (string-split (car word) ":"))
                 (setf (decoded-time-hour   bindings) nil)
                 (setf (decoded-time-minute bindings) nil)
                 (setf (decoded-time-second bindings) nil)
                 (setf (decoded-time-hour   parsed) (string-to-number hour))
                 (setf (decoded-time-minute parsed) (string-to-number minute))
                 (setf (decoded-time-second parsed) (string-to-number second))))
         ((string-match-p "^[0-9][0-9]?:[0-9][0-9]$" (car word))
             (let-unpack ((hour minute second) (string-split (car word) ":"))
                 (setf (decoded-time-hour   bindings) nil)
                 (setf (decoded-time-minute bindings) nil)
                 (setf (decoded-time-hour   parsed) (string-to-number hour))
                 (setf (decoded-time-minute parsed) (string-to-number minute))))
         ((string-match-p "^[-+][0-9]+y" (car word))
             (let* ((years (string-to-number (car word)))
                    (delta (make-decoded-time :year years)))
                 (setf (decoded-time-year offsets) t)
                 (setq parsed (datetime-parse--future-bias nil parsed now))
                 (setq parsed (fixed-decoded-time-add parsed delta))))
         ((string-match-p "^[-+][0-9]+mo" (car word))
             (let* ((months (string-to-number (car word)))
                    (delta  (make-decoded-time :month months)))
                 (datetime-parse--offset month)))
         ((string-match-p "^[-+][0-9]+w" (car word))
             (let* ((days  (* 7 (string-to-number (car word))))
                    (delta (make-decoded-time :day days)))
                 (datetime-parse--offset day)))
         ((string-match-p "^[-+][0-9]+d" (car word))
             (let* ((days  (string-to-number (car word)))
                    (delta (make-decoded-time :day days)))
                 (datetime-parse--offset day)))
         ((string-match-p "^[-+][0-9]+h" (car word))
             (let* ((hours (string-to-number (car word)))
                    (delta (make-decoded-time :hour hours)))
                 (datetime-parse--offset hour)))
         ((string-match-p "^[-+][0-9]+m" (car word))
             (let* ((minutes (string-to-number (car word)))
                    (delta   (make-decoded-time :minute minutes)))
                 (datetime-parse--offset minute)))
         ((string-match-p "^[-+][0-9]+s\\($\\|[^t]\\)" (car word))
             (let* ((seconds (string-to-number (car word)))
                    (delta   (make-decoded-time :second seconds)))
                 (datetime-parse--offset second)))
         ((string-match-p "^su" (car word))
             (setf (decoded-time-day bindings) nil)
             (setq parsed (datetime-parse--to-day-of-week parsed 0 now)))
         ((string-match-p "^mo" (car word))
             (setf (decoded-time-day bindings) nil)
             (setq parsed (datetime-parse--to-day-of-week parsed 1 now)))
         ((string-match-p "^tu" (car word))
             (setf (decoded-time-day bindings) nil)
             (setq parsed (datetime-parse--to-day-of-week parsed 2 now)))
         ((string-match-p "^we" (car word))
             (setf (decoded-time-day bindings) nil)
             (setq parsed (datetime-parse--to-day-of-week parsed 3 now)))
         ((string-match-p "^th" (car word))
             (setf (decoded-time-day bindings) nil)
             (setq parsed (datetime-parse--to-day-of-week parsed 4 now)))
         ((string-match-p "^fr" (car word))
             (setf (decoded-time-day bindings) nil)
             (setq parsed (datetime-parse--to-day-of-week parsed 5 now)))
         ((string-match-p "^sa" (car word))
             (setf (decoded-time-day bindings) nil)
             (setq parsed (datetime-parse--to-day-of-week parsed 6 now)))
         ((string-match-p "^ja" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 01))
         ((string-match-p "^fe" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 02))
         ((string-match-p "^\\(mr\\|mar\\)" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 03))
         ((string-match-p "^ap" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 04))
         ((string-match-p "^\\(my\\|may\\)" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 05))
         ((string-match-p "^\\(jn\\|jun\\)" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 06))
         ((string-match-p "^\\(jl\\|jul\\)" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 07))
         ((string-match-p "^au" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 08))
         ((string-match-p "^se" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 09))
         ((string-match-p "^oc" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 10))
         ((string-match-p "^no" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 11))
         ((string-match-p "^de" (car word))
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-month parsed) 12))
         ((string-match-p "^\\(td\\|tod\\)" (car word))
             (setf (decoded-time-year  bindings) nil)
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-day   bindings) nil)
             (setf (decoded-time-year  parsed)
                   (decoded-time-year  now))
             (setf (decoded-time-month parsed)
                   (decoded-time-month now))
             (setf (decoded-time-day   parsed)
                   (decoded-time-day   now)))
         ((string-match-p "^\\(tm\\|tom\\)" (car word))
             (setf (decoded-time-year  bindings) nil)
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-day   bindings) nil)
             (setf (decoded-time-year  parsed)
                   (decoded-time-year  now))
             (setf (decoded-time-month parsed)
                   (decoded-time-month now))
             (setf (decoded-time-day   parsed)
                   (decoded-time-day   now))
             (let ((one-day (make-decoded-time :day 1)))
                 (setq parsed (fixed-decoded-time-add parsed one-day))))
         ((string-match-p "^ye" (car word))
             (setf (decoded-time-year  bindings) nil)
             (setf (decoded-time-month bindings) nil)
             (setf (decoded-time-day   bindings) nil)
             (setf (decoded-time-year  parsed)
                   (decoded-time-year  now))
             (setf (decoded-time-month parsed)
                   (decoded-time-month now))
             (setf (decoded-time-day   parsed)
                   (decoded-time-day   now))
             (let ((one-day (make-decoded-time :day -1)))
                 (setq parsed (fixed-decoded-time-add parsed one-day))))
         (t
             (user-error "Bad word: %S" (car word)))))

(defmacro datetime-parse--bind-1 (slot &rest body)
    (let* ((name (symbol-name slot))
           (decoded-time-slot (intern (concat "decoded-time-" name)))
           (face (intern (concat "datetime-read-preview-" name "-face"))))
        `(unless (eq slot ',slot)
             (unless (,decoded-time-slot parsed)
                 (when-let (cell (pop integers))
                     (let* ((word  (car cell))
                            (,slot (string-to-number word)))
                         (setcar cell (propertize word 'face ',face))
                         (setf (,decoded-time-slot bindings) ,slot)
                         (setf (,decoded-time-slot parsed) ,slot))))
             ,@body)))

(defun datetime-parse--bind (slot parsed integers &optional bindings)
    (setq-if-nil bindings (make-decoded-time))
    (setq parsed (copy-sequence parsed))
    (datetime-parse--bind-1 month
        (datetime-parse--bind-1 day
            (datetime-parse--bind-1 hour
                (datetime-parse--bind-1 minute
                    (datetime-parse--bind-1 second)))))
    (list parsed integers bindings))

(defun datetime-parse--future-bias (slot parsed now)
    (setq parsed (copy-sequence parsed))
    (let ((delta nil))
        (unless (or (eq slot 'year)
                    (decoded-time-year parsed))
            (setf (decoded-time-year parsed)
                  (decoded-time-year now))
            (unless (or (eq slot 'month)
                        (decoded-time-month parsed))
                (setf (decoded-time-month parsed)
                      (decoded-time-month now))
                (unless (or (eq slot 'day)
                            (decoded-time-day parsed))
                    (setf (decoded-time-day parsed)
                          (decoded-time-day now))
                    (unless (or (eq slot 'hour)
                                (decoded-time-hour parsed))
                        (setf (decoded-time-hour parsed)
                              (decoded-time-hour now))
                        (unless (or (eq slot 'minute)
                                    (decoded-time-minute parsed))
                            (setf (decoded-time-minute parsed)
                                  (decoded-time-minute now))
                            (unless (eq slot 'second)
                                (setf-if-nil (decoded-time-second parsed)
                                             (decoded-time-second now)))
                            (when (datetime-parse--lessp parsed now)
                                (setq delta (make-decoded-time :minute 1))))
                        (when (and (not delta)
                                   (datetime-parse--lessp parsed now))
                            (setq delta (make-decoded-time :hour 1))))
                    (when (and (not delta)
                               (datetime-parse--lessp parsed now))
                        (setq delta (make-decoded-time :day 1))))
                (when (and (not delta)
                           (datetime-parse--lessp parsed now))
                    (setq delta (make-decoded-time :month 1))))
            (when (and (not delta)
                       (datetime-parse--lessp parsed now))
                (setq delta (make-decoded-time :year 1))))
        (fixed-decoded-time-add parsed delta)))

(defun datetime-parse--lessp (parsed now)
    (< (datetime-parse--decoded-time-to-integer parsed)
       (datetime-parse--decoded-time-to-integer now)))

(defun datetime-parse--decoded-time-to-integer (decoded-time)
    (logxor
        (ash             (decoded-time-year   decoded-time)             26)
        (ash (logand (or (decoded-time-month  decoded-time) #x0F) #x0F) 22)
        (ash (logand (or (decoded-time-day    decoded-time) #x1F) #x1F) 17)
        (ash (logand (or (decoded-time-hour   decoded-time) #x1F) #x1F) 12)
        (ash (logand (or (decoded-time-minute decoded-time) #x3F) #x3F) 06)
        (ash (logand (or (decoded-time-second decoded-time) #x3F) #x3F) 00)))

(defun datetime-parse--floor (slot parsed)
    (setq parsed (copy-sequence parsed))
    (unless (eq slot 'month)
        (setf-if-nil (decoded-time-month parsed) 1)
        (unless (eq slot 'day)
            (setf-if-nil (decoded-time-day parsed) 1)
            (unless (eq slot 'hour)
                (setf-if-nil (decoded-time-hour parsed) 0)
                (unless (eq slot 'minute)
                    (setf-if-nil (decoded-time-minute parsed) 0)
                    (unless (eq slot 'second)
                        (setf-if-nil (decoded-time-second parsed) 0))))))
    parsed)

(defmacro datetime-parse--offset (slot)
    (let* ((name (symbol-name slot))
           (decoded-time-slot (intern (concat "decoded-time-" name)))
           (slots '(year month day hour minute second))
           (index (cl-position slot slots))
           (next-slot (nth (1+ index) slots)))
        `(progn
             (setf (,decoded-time-slot offsets) t)
             (unpack (parsed integers bindings)
                     (datetime-parse--bind
                         ',next-slot parsed integers bindings))
             (when (,decoded-time-slot bindings)
                 (if (eq (,decoded-time-slot bindings) t)
                     (setf (,decoded-time-slot bindings) nil)
                     (setf (,decoded-time-slot bindings) t)))
             (setq parsed (datetime-parse--future-bias nil parsed now))
             (setq parsed (datetime-parse--floor ',next-slot parsed))
             (setq previous-parsed
                   (datetime-parse--floor ',next-slot previous-parsed))
             (setq previous-parsed
                   (fixed-decoded-time-add previous-parsed nil))
             (setq parsed (fixed-decoded-time-add parsed delta)))))

(defun datetime-parse--to-day-of-week (parsed day-of-week now)
    (setq parsed (datetime-parse--future-bias 'hour parsed now))
    (setq parsed (datetime-parse--floor 'hour parsed))
    (let* ((day-of-week+ (decoded-time-days-until-weekday
                             parsed day-of-week))
           (delta        (make-decoded-time :day day-of-week+)))
        (fixed-decoded-time-add parsed delta)))

(defun datetime-parse--plug (parsed)
    (unless (decoded-time-second parsed)
        (setf (decoded-time-second parsed) t)
        (unless (decoded-time-minute parsed)
            (setf (decoded-time-minute parsed) t)
            (unless (decoded-time-hour parsed)
                (setf (decoded-time-hour parsed) t)
                (unless (decoded-time-day parsed)
                    (setf (decoded-time-day parsed) t)
                    (unless (decoded-time-month parsed)
                        (setf (decoded-time-month parsed) t)
                        (unless (decoded-time-year parsed)
                            (setf (decoded-time-year parsed) t))))))))

(defun datetime-parse--unplug (parsed)
    (when (eq (decoded-time-second parsed) t)
        (setf (decoded-time-second parsed) nil)
        (when (eq (decoded-time-minute parsed) t)
            (setf (decoded-time-minute parsed) nil)
            (when (eq (decoded-time-hour parsed) t)
                (setf (decoded-time-hour parsed) nil)
                (when (eq (decoded-time-day parsed) t)
                    (setf (decoded-time-day parsed) nil)
                    (when (eq (decoded-time-month parsed) t)
                        (setf (decoded-time-month parsed) nil)
                        (when (eq (decoded-time-year parsed) t)
                            (setf (decoded-time-year parsed) nil))))))))

(defvar datetime-read-popup-calendar nil)

(defun datetime-read--preview (short now cell)
    (when-let ((overlay (car cell))
               (_       (eq (current-buffer) (overlay-buffer overlay))))
        (let ((input (substring-no-properties (minibuffer-contents))))
            (condition-case error
                (let-unpack ((parsed info) (datetime-parse--loop
                                               input short now)
                             (_ _ _ day month year) parsed)
                    (datetime-read--preview-calendar year month day cell)
                    (save-point
                        (delete-minibuffer-contents)
                        (insert (nth 2 info)))
                    (datetime-read--preview-show overlay
                        (datetime-read--preview-format parsed info)))
                (error
                    (datetime-read--preview-show overlay
                        (error-message-string error)))))))

(defun datetime-read--preview-show (overlay string)
    (move-overlay overlay (point-max) (point-max))
    (overlay-put overlay 'before-string
        (when (length> string 0)
            (concat (propertize " " 'cursor t) "[" string "]"))))

(defun datetime-read--preview-calendar (year month day cell)
    (unwind-protect
        (if (not datetime-read-popup-calendar)
            (progn
                (setcdr cell nil)
                (calendar-exit))
            (if-let ((_      (cdr cell))
                     (buffer (get-buffer "*Calendar*")))
                (switch-to-buffer buffer)
                (setcdr cell t)
                (calendar))
            (calendar-unmark)
            (when year
                (let (date)
                    (let* ((year  (if (< year 1)
                                      (1- year)
                                      year))
                           (month (or month 1))
                           (day   (or day 1)))
                        (unless (datetime--valid-month month)
                            (setq month 1))
                        (unless (datetime--valid-day year month day)
                            (setq day 1))
                        (setq date (list month day year)))
                    (calendar-goto-date date)
                    (when (and month day
                               (datetime--valid-month+day-p year month day))
                        (calendar-mark-visible-date
                            date '(:foreground "red")))))
            (setq cursor-type nil))
        (select-window (minibuffer-window))))

(defun datetime-read--preview-format (parsed preview-info)
    (let-unpack ((prior bound _) preview-info)
        (concat
            (datetime-read--preview-format-1 'year  parsed prior nil)
            (datetime-read--preview-format-1 'month parsed prior bound)
            (datetime-read--preview-format-1 'day   parsed prior bound)
            (when-let (final-value (decoded-time-weekday parsed))
                (let* ((names '(Sun Mon Tue Wed Thu Fri Sat))
                       (name  (nth final-value names)))
                    (if-let ((prior-value (decoded-time-weekday prior))
                             (_           (not (= prior-value final-value))))
                        (format " (%s->%s)" (nth prior-value names) name)
                        (format " (%s)" name))))
            (datetime-read--preview-format-1 'hour   parsed prior bound)
            (datetime-read--preview-format-1 'minute parsed prior bound)
            (datetime-read--preview-format-1 'second parsed prior bound))))

(defun datetime-read--preview-format-1 (slot parsed prior bound)
    (let* ((format-string (if (eq slot 'year) "%04d" "%02d"))
           (name (symbol-name slot))
           (getter (intern (concat "decoded-time-" name)))
           (face (intern (concat "datetime-read-preview-" name "-face")))
           (final-value (funcall getter parsed))
           (prior-value (funcall getter prior))
           (bound-value (funcall getter bound))
           (prefix      (cond ((memq slot '(second minute)) ":")
                              ((eq   slot 'hour)            " ")
                              ((memq slot '(day month))     "-")
                              ((eq   slot 'year)            ""))))
        (when final-value
            (if (and prior-value (not (= prior-value final-value)))
                (format (concat prefix
                                "{"
                                (if bound-value
                                    (propertize format-string 'face face)
                                    format-string)
                                "->"
                                format-string
                                "}")
                        prior-value final-value)
                (when (eq bound-value final-value)
                    (setq format-string (propertize format-string 'face face)))
                (concat prefix (format format-string final-value))))))

(defun datetime-read (&optional prompt initial-input short)
    (setq-if-nil prompt "Date+time: ")
    (when initial-input
        (setq initial-input (datetime-collapse initial-input))
        (setq initial-input (datetime-expand initial-input t))
        (setq initial-input (concat initial-input " ")))
    (let ((cell (cons nil nil))
          (now  (decode-time (current-time))))
        (add-single-use-hook 'minibuffer-setup-hook
            (lambda-let (cell) ()
                (setcar cell (make-overlay (point-max) (point-max)))))
        (with-hook (('post-command-hook
                        (lambda-let (short now cell) ()
                            (datetime-read--preview short now cell))))
            (let ((datetime (read-string prompt initial-input)))
                (datetime-parse datetime short now)))))

(defun datetime-expand (datetime &optional space-instead-of-t)
    (concat (substring datetime 0 4)
            (when (length> datetime 4)
                "-")
            (when (length> datetime 4)
                (substring datetime 4 6))
            (when (length> datetime 6)
                "-")
            (when (length> datetime 6)
                (substring datetime 6 8))
            (when (length> datetime 8)
                (if space-instead-of-t
                    " "
                    "T"))
            (when (length> datetime 8)
                (substring datetime 9 11))
            (when (length> datetime 11)
                ":")
            (when (length> datetime 11)
                (substring datetime 11 13))
            (when (length> datetime 13)
                ":")
            (when (length> datetime 13)
                (substring datetime 13))))

(defun datetime-collapse (datetime)
    (setq datetime (string-replace "-" "" datetime))
    (setq datetime (string-replace ":" "" datetime))
    (setq datetime (replace-regexp-in-string "[T \n]+" "" datetime))
    (when (string-match-p "^[0-9]\\{9,\\}" datetime)
        (setq datetime (concat (substring datetime 0 8)
                               "T"
                               (substring datetime 8))))
    datetime)

(defun datetime-floor (datetime)
    (concat datetime (substring "00010101T000000" (length datetime))))


(use-package undo-tree
    :config
    (setq undo-tree-auto-save-history nil)
    (add-to-list 'undo-tree-incompatible-major-modes
        'undo-tree-visualizer-mode)
    (add-to-list 'undo-tree-visualizer-mode-hook
        (lambda ()
            (set-window-dedicated-p (selected-window) nil)
            (setq mode-name "Undo")))
    (defun undo-tree-visualizer-quit ()
        (interactive)
        (undo-tree-clear-visualizer-data buffer-undo-tree)
        (if-let (parent undo-tree-visualizer-parent-buffer)
            (with-current-buffer parent
	        (remove-hook 'before-change-functions 'undo-tree-kill-visualizer t)))
        (when undo-tree-visualizer-diff
            (undo-tree-visualizer-hide-diff))
        (quit-window t))
    (define-key undo-tree-visualizer-mode-map
        "\C-m" 'undo-tree-visualizer-quit)
    (define-key undo-tree-visualizer-mode-map
        "q" 'undo-tree-visualizer-abort)
    (defun undo-tree-visualize-jump-branch-left (count)
        (interactive "p")
        (undo-tree-visualizer-selection-mode 1)
        (dotimes (_ count)
            (undo-tree-visualizer-select-left 1))
        (undo-tree-visualizer-set)
        (undo-tree-visualizer-selection-mode -1))
    (defun undo-tree-visualize-jump-branch-right (count)
        (interactive "p")
        (undo-tree-visualizer-selection-mode 1)
        (dotimes (_ count)
            (undo-tree-visualizer-select-right 1))
        (undo-tree-visualizer-set)
        (undo-tree-visualizer-selection-mode -1)))

(use-package vertico
    :config
    (vertico-mode 1)
    (setq vertico-cycle t)
    (defvar multi-vertico--results nil)
    (defun multi-vertico--exit ()
        (setq multi-vertico--results vertico--candidates))
    (defun multi-vertico (function &rest arguments)
        (with-hook (('minibuffer-exit-hook 'multi-vertico--exit))
            (apply function arguments))
        multi-vertico--results)
    (setq minibuffer-prompt-properties
        (append '(cursor-intangible t) minibuffer-prompt-properties))
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))

(use-packages crm vertico
    :config
    (defun vertico-crm-comma ()
        (interactive)
        (vertico-insert)
        (insert ","))
    (defun hack-completing-read-multiple (&rest _)
        (add-single-use-hook 'minibuffer-setup-hook
            (lambda ()
                (local-set-key "," 'vertico-crm-comma)
                (local-set-key "\M-,"
                    (lambda ()
                        (interactive)
                        (insert ","))))))
    (advice-add 'completing-read-multiple
        :before 'hack-completing-read-multiple))

(use-packages window vertico
    :config
    (defvar switch-to-buffer-last-search--tentative nil)
    (defvar switch-to-buffer-last-search nil)
    (defun switch-to-buffer-resume ()
        (interactive)
        (when (eq this-command 'switch-to-buffer-resume)
            (setq this-command 'switch-to-buffer))
        (setq unread-command-events
              (append (listify-key-sequence switch-to-buffer-last-search)
                      unread-command-events))
        (call-interactively 'switch-to-buffer))
    (defun switch-to-buffer--quit ()
        (when (eq current-minibuffer-command 'switch-to-buffer)
            (if switch-to-buffer-last-search--tentative
                (setq switch-to-buffer-last-search
                      switch-to-buffer-last-search--tentative
                      switch-to-buffer-last-search--tentative nil)
                (let ((query (field-string-no-properties)))
                    (when (length> query 0)
                        (setq switch-to-buffer-last-search query))))))
    (add-hook 'minibuffer-exit-hook 'switch-to-buffer--quit)
    (defvar switch-to-buffer-next--candidates nil)
    (defvar switch-to-buffer-next--sorted-candidates nil)
    (defun switch-to-buffer-next--update-candidates ()
        (when vertico--candidates
            (let* ((new        (copy-sequence vertico--candidates))
                   (new-sorted (sort (copy-sequence new) 'string-lessp)))
                (setq new (apply 'dlist new))
                (dlist-setcdr (dlist-last new) new)
                (setq switch-to-buffer-next--candidates
                      (dlist-nthcdr vertico--index new))
                (setq switch-to-buffer-next--sorted-candidates new-sorted))))
    (defun switch-to-buffer--exit (&rest _)
        (when (eq current-minibuffer-command 'switch-to-buffer)
            (switch-to-buffer-next--update-candidates)
            (let ((query (field-string-no-properties)))
                (when (length> query 0)
                    (setq switch-to-buffer-last-search--tentative query)))))
    (advice-add 'vertico-exit :before 'switch-to-buffer--exit)
    (defun switch-to-buffer-next--1 (stepper)
        (let* ((start  (funcall stepper switch-to-buffer-next--candidates))
               (next   (funcall stepper start))
               (name   (dlist-car start))
               (buffer (get-buffer name)))
            (setq switch-to-buffer-next--candidates start)
            (until (or buffer (eq next start))
                (setq switch-to-buffer-next--candidates next)
                (setq name (dlist-car next))
                (setq next (funcall stepper next))
                (setq buffer (get-buffer name)))
            buffer))
    (defun switch-to-buffer-next (count)
        (interactive "p")
        (unless switch-to-buffer-last-search
            (user-error "No search to resume"))
        (let ((buffer nil)
              (stepper 'dlist-cdr))
            (when (< count 0)
                (setq count (- count))
                (setq stepper 'caar))
            (dotimes (_ count)
                (setq buffer (switch-to-buffer-next--1 stepper)))
            (unless (= count 0)
                (unless buffer
                    (user-error "Search failed: %S"
                                switch-to-buffer-last-search))
                (switch-to-buffer buffer))))
    (defun switch-to-buffer-previous (count)
        (interactive "p")
        (switch-to-buffer-next (- count))))

(use-package consult
    :config
    (setq completion-in-region-function 'consult-completion-in-region)
    (defun hack-consult-completion-in-region
            (consult-completion-in-region &rest arguments)
        (let ((completion-reference-buffer (current-buffer)))
            (apply consult-completion-in-region arguments)))
    (advice-add 'consult-completion-in-region
        :around 'hack-consult-completion-in-region)
    (add-to-list 'consult-mode-histories
        '(eshell-mode
          histdir-buffer-local-history-list
          nil
          eshell-bol))
    (setq consult-find-args "find .")
    (setq consult-project-function (lambda (_may-prompt) (git-repo-root)))
    (defun fixed-consult--insertion-preview (preview)
        (when preview
            (lambda-let ((window (selected-window)) preview) (&rest arguments)
                (with-selected-window window
                    (apply preview arguments)))))
    (advice-add 'consult--insertion-preview
        :filter-return 'fixed-consult--insertion-preview)
    (defun hack-consult-preview (preview cell action candidate)
        (funcall preview 'setup nil)
        (if (eq action 'hack-repeat-preview)
            (funcall preview 'preview (car cell))
            (setcar cell candidate)
            (funcall preview action candidate)))
    (defun fixed-consult-history (prefix-argument)
        (interactive "P")
        (if (minibufferp)
            (progn
                (consult-history)
                (command-string))
            (let* ((input   (point-max))
                   (source  (if prefix-argument (point) input))
                   (command (command-string source))
                   (preview (apply-partially 'hack-consult-preview
                                 (consult--insertion-preview input input)
                                 (cons nil nil))))
                (let-unpack ((history index bol) (consult--current-history))
                    (delete-command input)
                    (with-temp-buffer
                        (insert-before-markers command)
                        (run-with-idle-timer 0.04 nil
                            preview 'hack-repeat-preview nil)
                        (with-advice (('consult--insertion-preview
                                          :override (ignore+return preview)))
                            (consult-history history index bol))
                        (setq command (buffer-string))))
                (end-of-buffer)
                (replace-command command)
                command)))
    (defun consult-line-resume (prefix-argument)
        (interactive "P")
        (when (eq this-command 'consult-line-resume)
            (setq this-command 'consult-line))
        (consult-line
            (cond
                ((not prefix-argument)
                    (nth 0 consult--line-history))
                ((and (integerp prefix-argument) (>= prefix-argument 0))
                    (nth (- prefix-argument 1)
                        (seq-uniq consult--line-history)))
                (t
                    (completing-read
                        "Resume consult-line: "
                        consult--line-history
                        nil
                        nil
                        nil
                        'consult--line-history)))))
    (defun consult-line-next (count)
        (interactive "p")
        (let ((consult-after-jump-hook nil))
            (add-single-use-hook 'post-command-hook
                (lambda ()
                    (vertico--update)
                    (when (< vertico--index 0)
                        (push 'fail unread-command-events))))
            (add-single-use-hook 'consult-after-jump-hook
                (lambda-let ((count)
                             (start (line-number-at-pos))
                             (direction 'previous))
                        ()
                    (if (< count 0)
                        (setq count (- count))
                        (setq direction 'next)
                        (unless (equal (line-number-at-pos) start)
                            (setq count (1- count))))
                    (push 'done unread-command-events)
                    (dotimes (_ count)
                        (push direction unread-command-events))))
            (let ((vertico-count 0))
                (defer-input
                    (define-key defer-input-map "\C-g" 'abort-minibuffers)
                    (define-key defer-input-map [next] 'vertico-next)
                    (define-key defer-input-map [previous] 'vertico-previous)
                    (define-key defer-input-map [done] 'vertico-exit)
                    (define-key defer-input-map [fail]
                        (lambda ()
                            (interactive)
                            (throw 'exit
                                (lambda-let ((query (minibuffer-contents))) ()
                                    (user-error "Search failed: %S" query)))))
                    (with-nested-command-state
                        (consult-line-resume nil))))
        (pulse-momentary-highlight-one-line)))
    (defun consult-line-previous (count)
        (interactive "p")
        (consult-line-next (- count)))
    (defun consult-line-quit ()
        (interactive)
        (let ((query (field-string-no-properties)))
            (when (length> query 0)
                (push query consult--line-history)))
        (abort-minibuffers)))

(use-package orderless
    :config
    (add-to-list 'completion-styles 'orderless)
    (setq orderless-matching-styles '(orderless-regexp))
    (defun fixed-orderless-not (orderless-not predicate regexp)
        (let ((matcher     (funcall orderless-not predicate regexp))
              (ignore-case (orderless--ignore-case-p (list regexp))))
            (lambda-let (matcher ignore-case) (string)
                (let ((case-fold-search ignore-case))
                    (funcall matcher string)))))
    (advice-add 'orderless-not :around 'fixed-orderless-not)
    (setq orderless-style-dispatchers nil)
    (defun !-suffix-dispatcher (string index count)
        (when (string-match-p "[^\\]\\([\\][\\]\\)*!$" string)
            (setq string (string-remove-suffix "!" string))
            (cons 'orderless-not string)))
    (add-to-list 'orderless-style-dispatchers '!-suffix-dispatcher))

(use-packages consult orderless
    :config
    (defconst min-consult-id consult--tofu-char)
    (defconst max-consult-id (+ consult--tofu-char consult--tofu-range -1))
    (defconst $-for-consult (format "[%c-%c]*$" min-consult-id max-consult-id))
    (defun $-suffix-dispatcher (string index count)
        (when (string-match-p "[^\\]\\([\\][\\]\\)*$$" string)
            (setq string (concat (substring string 0 -1) $-for-consult))
            (cons 'orderless-regexp string)))
    (add-to-list 'orderless-style-dispatchers '$-suffix-dispatcher))

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
    (eat-eshell-mode 1)
    (eat-eshell-visual-command-mode 1))

(defvar-local pop-to-command-buffer nil)
(defvar pop-to-command-setup-hook nil)
(defvar-local pop-to-command--callback nil)
(defun pop-to-command-buffer-name (type command &optional context name)
    (setq-if-nil name (concat type ": " (string-join command " ")))
    (if context
        (concat "*" name " (" context ")*")
        (concat "*" name "*")))
(defun pop-to-command-eshell (command &optional context name callback)
    (require 'eshell)
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
        (use-local-map (make-sparse-keymap))
        (set-keymap-parent (current-local-map) eshell-mode-map)
        (local-set-key "\C-m" 'ignore)
        (setq-local pop-to-command-buffer t)
        (setq-local pop-to-command--callback callback)
        (setq default-directory directory)
        (end-of-buffer)
        (add-hook 'eshell-post-command-hook 'pop-to-command--done-eshell nil t)
        (run-hooks 'pop-to-command-setup-hook)
        (let ((parsed-command (eshell-parse-command program arguments t)))
            (eshell-eval-command parsed-command))
        buffer))
(defun pop-to-command--done-eshell ()
    (insert "\nCommand " (buffer-name) " done.\n")
    (end-of-buffer)
    (when pop-to-command--callback
        (funcall pop-to-command--callback)))
(defun pop-to-command-eat (command &optional context name callback)
    (require 'eshell)
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
        (add-hook 'eat-exit-hook 'pop-to-command--done-eat nil t)
        (run-hooks 'pop-to-command-setup-hook)
        (eat-exec buffer name program nil arguments)
        buffer))
(defun pop-to-command--done-eat (_process)
    (when pop-to-command--callback
        (funcall pop-to-command--callback)))
(provide 'pop-to-command)

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
            (evil-yank-characters 1 (point-max) register yank-handler)))
    (defun evil-paste-to-string (count &optional register)
        (repeat-string
            (if register
                (evil-get-register register)
                (current-kill 0))
            (prefix-numeric-value count)))
    (define-key evil-motion-state-map "\"" 'evil-use-register)
    (define-key evil-normal-state-map "U" 'evil-redo)
    (define-key evil-normal-state-map "H" 'evil-join)
    (define-key evil-motion-state-map "H" nil)
    (define-key evil-normal-state-map "J" nil)
    (define-key evil-motion-state-map "J" 'evil-window-bottom)
    (define-key evil-motion-state-map "K" 'evil-window-top)
    (define-key evil-motion-state-map "L" nil)
    (define-key evil-normal-state-map "q" nil)
    (define-key evil-visual-state-map "q" 'evil-exit-visual-state)
    (define-key evil-operator-state-map "q" 'evil-force-normal-state)
    (define-key evil-motion-state-map "Q" 'quit-previous-window)
    (defvar override-evil-mode-line-tag nil)
    (defmacro with-override-evil-mode-line-tag (tag help-string &rest body)
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
    (defvar-local color-code-vi-state--cookie-1 nil)
    (defvar-local color-code-vi-state--cookie-2 nil)
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
    (setq evil-shift-round nil)
    (define-key evil-replace-state-map [escape] 'evil-insert-state)
    (define-key evil-motion-state-map "\C-m" nil)
    (define-key evil-motion-state-map "\t" nil)
    (define-key evil-normal-state-map "\C-?" 'evil-delete-backward-char)
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
            (with-advice (('evil-set-marker :before 'hack-evil-set-marker)
                          ('evil-yank-line-handler
                              :before 'hack-evil-yank-line-handler)
                          ('evil-yank-block-handler
                              :before 'hack-evil-yank-block-handler))
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
    (advice-add 'evil-quit
        :around
        (lambda (evil-quit &rest arguments)
            (with-advice (('delete-window :override 'kill-current-buffer))
                (apply evil-quit arguments))))
    (defun lookup-evil-key (state keymap key &optional accept-default)
        (when state
            (setq keymap (evil-get-auxiliary-keymap keymap state)))
        (lookup-key keymap key accept-default))
    (define-key evil-motion-state-map "gG"
        (lambda ()
            (interactive)
            (goto-char (point-max))))
    (defun evil-ex-nosearch ()
        (interactive)
        (setq evil-ex-search-pattern nil)
        (setq evil-ex-search-direction nil)
        (evil-ex-nohighlight))
    (add-to-list 'evil-ex-commands '("nosearch" . evil-ex-nosearch))
    (add-to-list 'evil-ex-commands '("nos" . "nosearch"))
    (define-prefix-command 'space-map)
    (define-key evil-motion-state-map " " 'space-map)
    (dolist (map (list evil-motion-state-map evil-insert-state-map
                       evil-replace-state-map evil-emacs-state-map))
        (dolist (key '([?\C- ] "\C-@"))
            (define-key map key 'evil-execute-in-normal-state)))
    (define-key space-map [escape] 'ignore)
    (define-key space-map "q" 'ignore)
    (define-key space-map "Q" 'delete-frame)
    (define-key space-map "!" 'save-buffers-kill-emacs)
    (define-key universal-argument-map [escape] 'ignore)
    (defmacro define-universal-argument-space-keys (map prefix)
        (let* ((suffix (concat "-argument-in-" (symbol-name map)))
               (universal (intern (concat "universal" suffix)))
               (digit     (intern (concat "digit" suffix)))
               (negative  (intern (concat "negative" suffix))))
            `(progn
                 (defun ,universal (prefix-argument)
                     (interactive "P")
                     (if prefix-argument
                         (universal-argument-more prefix-argument)
                         (universal-argument))
                     (command-execute-in-keymap ,map ,prefix t))
                 (defun ,digit (prefix-argument)
                     (interactive "P")
                     (digit-argument prefix-argument)
                     (command-execute-in-keymap ,map ,prefix t))
                 (defun ,negative (prefix-argument)
                     (interactive "P")
                     (negative-argument prefix-argument)
                     (command-execute-in-keymap ,map ,prefix t))
                 (define-key ,map " " ',universal)
                 (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
                     (define-key ,map key ',digit))
                 (define-key ,map "-" ',negative))))
    (define-universal-argument-space-keys space-map " ")
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
    (define-key space-map "e" 'eval-last-sexp)
    (define-key space-map "E" 'eval-print-last-sexp)
    (defmacro toggle (variable &optional skip-preview)
        (let* ((name          (symbol-name variable))
               (function      (intern (concat "toggle-" name)))
               (format-string (concat name ": %s")))
            `(prog1
                 (defun ,function ()
                     (interactive)
                     ,(if skip-preview
                          `(setq ,variable (not ,variable))
                          `(when (eq last-command ',function)
                               (setq ,variable (not ,variable))))
                     (message ,format-string ,variable))
                 (evil-declare-not-repeat ',function))))
    (define-key space-map "." (toggle evil-repeat-move-cursor))
    (define-key space-map "b"
        (lambda (prefix-argument)
            (interactive "P")
            (if prefix-argument
                (become-command 'switch-to-buffer-resume)
                (become-command 'switch-to-buffer))))
    (define-key space-map "B" 'ibuffer)
    (defun smoother-kill-buffer (prefix-argument)
        (interactive "P")
        (if prefix-argument
            (let ((names (multi-vertico 'read-buffer "Kill buffers: "))
                  (count 0))
                (dolist (name names)
                    (when (kill-buffer name)
                        (+= count 1)))
                (if (equal count 1)
                    (message "Killed 1 buffer" count)
                    (message "Killed %d buffers" count)))
            (when (confirm-p (format "Kill %s?" (buffer-name)))
                (add-hook
                    'kill-buffer-hook
                    (lambda ()
                        (let ((buffer (current-buffer)))
                            (quit-window)
                            (set-buffer buffer)))
                    nil t)
                (kill-buffer (current-buffer)))))
    (define-key space-map "k" 'smoother-kill-buffer)
    (defun kill-other-buffer ()
        (interactive)
        (kill-buffer (read-buffer-to-switch "Kill buffer")))
    (define-key space-map "K" 'kill-other-buffer)
    (define-key space-map "f" 'find-file)
    (define-key space-map "F" 'find-alternate-file)
    (define-key space-map "d" 'dired)
    (add-to-list 'evil-motion-state-modes 'dired-mode)
    (defun delete-buffer-file ()
        (interactive)
        (if buffer-file-name
            (when (y-or-n-p (format "Delete %s?" buffer-file-name))
                (delete-file buffer-file-name)
                (if (y-or-n-p (format "Kill %s?" (buffer-name)))
                    (with-buffer-modified-p nil
                        (kill-buffer))
                    (set-buffer-modified-p t)))
            (user-error "%s is not visiting a file" (buffer-name))))
    (define-key space-map "D" 'delete-buffer-file)
    (define-key space-map "y" 'execute-extended-command)
    (define-key space-map "," 'eval-expression)
    (define-key space-map "h" help-map)
    (evil-define-key 'motion help-mode-map "H" 'help-go-back)
    (evil-define-key 'motion help-mode-map "L" 'help-go-forward)
    (define-key space-map "P" 'list-packages)
    (add-to-list 'evil-motion-state-modes 'package-menu-mode)
    (defconst history-quit nil)
    (defun history--require-match-p (_)
        history-quit)
    (defun history--require-match (arguments)
        (if (length= arguments 2)
            (setcdr (last arguments) (list nil 'history--require-match-p))
            (if (length= arguments 3)
                (setcdr (last arguments) (list 'history--require-match-p))
                (setcar (nthcdr 3 arguments) 'history--require-match-p)))
        arguments)
    (defun history--match (prefix-argument)
        (with-advice (('completing-read :filter-args 'history--require-match))
            (fixed-consult-history prefix-argument)))
    (defun history-execute (prefix-argument)
        (interactive "P")
        (let* ((history-quit nil)
               (command (fixed-consult-history prefix-argument))
               (run (key-binding "\C-m" t)))
            (evil-repeat-start)
            (add-to-list 'evil-repeat-info
                `((lambda ()
                      (end-of-buffer)
                      (replace-command ,command)
                      (unless ,history-quit
                          (,run)))))
            (evil-repeat-stop)
            (unless history-quit
                (funcall run))))
    (defun history-remove (prefix-argument)
        (interactive "P")
        (let* ((history-quit nil)
               (entry (history--match prefix-argument))
               (history (car (consult--current-history))))
            (unless history-quit
                (if histdir
                    (histdir-remove entry)
                    (if (ring-p history)
                        (while-let ((index (ring-member history entry)))
                            (ring-remove history index))))
                (evil-end-undo-step)
                (evil-start-undo-step)
                (delete-command))))
    (consult-customize history-remove :prompt "Remove history: ")
    (evil-declare-not-repeat 'history-remove)
    (defun history-change (prefix-argument)
        (interactive "P")
        (let* ((history-quit nil)
               (old-entry (history--match prefix-argument))
               (new-entry (unless history-quit
                              (setq this-command 'history-change)
                              (read-string "Change history to: " old-entry)))
               (history (car (consult--current-history))))
            (unless history-quit
                (if histdir
                    (progn
                        (histdir-remove old-entry)
                        (histdir-add new-entry t))
                    (when (ring-p history)
                        (while-let ((index (ring-member history old-entry)))
                            (ring-remove history index))
                        (ring-insert history new-entry))))
            (when new-entry
                (evil-end-undo-step)
                (evil-start-undo-step)
                (replace-command new-entry))))
    (consult-customize history-change :prompt "Change history from: ")
    (evil-declare-not-repeat 'history-change)
    (defun history-quit ()
        (interactive)
        (setq history-quit t)
        (vertico-exit))
    (evil-declare-not-repeat 'history-quit)
    (defconst history-commands
        '(history-execute history-remove history-change))
    (define-key space-map "t" 'eshell)
    (add-to-list 'evil-normal-state-modes 'eshell-mode)
    (defvar-local evil-eshell-state-for-next-input 'normal)
    (advice-add 'eshell-send-input
        :before
        (lambda (&rest _)
            (setq evil-eshell-state-for-next-input evil-state)))
    (defun evil-eshell-force-normal-state ()
        (interactive)
        (evil-force-normal-state)
        (setq evil-eshell-state-for-next-input evil-state))
    (add-hook 'eshell-mode-hook
        (lambda ()
            (evil-local-set-key 'normal [escape] 'eshell-interrupt-process)
            (evil-local-set-key 'insert [escape]
                'evil-eshell-force-normal-state)
            (evil-local-set-key 'operator [escape] 'evil-force-normal-state)
            (evil-local-set-key 'insert "\C-d" 'eshell-send-eof-to-process)))
    (add-hook 'eshell-pre-command-hook
        (lambda ()
            (setq evil-eshell-state-for-next-input evil-state)
            (evil-insert-state)))
    (add-hook 'eshell-post-command-hook
        (lambda ()
            (evil-force-normal-state)
            (when (eq (current-buffer) (window-buffer (selected-window)))
                (evil-change-state evil-eshell-state-for-next-input))))
    (evil-declare-not-repeat 'fixed-eshell-send-input)
    (evil-declare-not-repeat 'eshell-interrupt-process)
    (define-key space-map "T" 'eat)
    (add-to-list 'evil-emacs-state-modes 'eat-mode)
    (add-hook 'eat-exit-hook (lambda (_process) (evil-normal-state nil)))
    (add-hook 'eat-mode-hook
        (lambda ()
            (evil-local-set-key 'emacs "\C-[" 'eat-self-input)))
    (define-key eat-semi-char-mode-map "\C-c\C-z" 'eat-self-input)
    (evil-set-register ?r (repeat-string "1234567890" 8))
    (evil-set-register ?R "\n")
    (evil-set-register ?r (propertize (evil-get-register ?r)
        'yank-handler '(evil-yank-line-handler nil t)))
    (define-key space-map "i" 'insert-char)
    (define-key space-map "I" (toggle inhibit-read-only))
    (define-key space-map "#" 'display-line-numbers-mode)
    (set-face-foreground 'line-number "#808080")
    (defun cycle-line-wrap ()
        (interactive)
        (when (eq last-command 'cycle-line-wrap)
            (if truncate-lines
                (setq truncate-lines nil)
                (visual-line-mode 'toggle)
                (when (not visual-line-mode)
                    (setq truncate-lines t))))
        (message "visual-line-mode: %s truncate-lines: %s"
            visual-line-mode truncate-lines))
    (evil-declare-not-repeat 'cycle-line-wrap)
    (define-key space-map ";" 'cycle-line-wrap)
    (define-key space-map "]" (toggle select-enable-clipboard))
    (define-key space-map "l" 'consult-line)
    (define-key space-map "L" 'consult-line-resume)
    (defvar evil-consult--consult-is-last-search nil)
    (defun evil-consult--evil-start (&rest _)
        (setq evil-consult--consult-is-last-search nil))
    (advice-add 'evil-ex-start-search :after 'evil-consult--evil-start)
    (advice-add 'evil-ex-start-word-search :after 'evil-consult--evil-start)
    (defun evil-consult--consult-start (&rest _)
        (setq evil-consult--consult-is-last-search t)
        (evil-ex-nohighlight))
    (advice-add 'consult-line :after 'evil-consult--consult-start)
    (defun evil-consult--clear (&rest _)
        (setq evil-consult--consult-is-last-search nil))
    (advice-add 'evil-ex-nosearch :after 'evil-consult--clear)
    (defun evil-ex-search-or-consult-line-next (count)
        (interactive "p")
        (if evil-consult--consult-is-last-search
            (consult-line-next count)
            (if evil-ex-search-pattern
                (evil-ex-search-next count)
                (user-error "No search to resume"))))
    (evil-declare-not-repeat 'evil-ex-search-or-consult-line-next)
    (defun evil-ex-search-or-consult-line-previous (count)
        (interactive "p")
        (if evil-consult--consult-is-last-search
            (consult-line-previous count)
            (if evil-ex-search-pattern
                (evil-ex-search-previous count)
                (user-error "No search to resume"))))
    (evil-declare-not-repeat 'evil-ex-search-or-consult-line-previous)
    (define-key evil-motion-state-map "n"
        'evil-ex-search-or-consult-line-next)
    (define-key evil-motion-state-map "N"
        'evil-ex-search-or-consult-line-previous)
    (define-prefix-command 'space-search-map)
    (define-key space-map "s" 'space-search-map)
    (define-key space-search-map "l" 'consult-ripgrep)
    (define-key space-search-map "f" 'consult-fd)
    (define-key space-search-map "t"
        (lambda ()
            (interactive)
            (consult-fd "~/storage/shared" "\\.trashed -- -H")))
    (define-key space-map "x" 'tramp-cleanup-connection)
    (add-hook 'pop-to-command-setup-hook
        (lambda ()
            (evil-initialize-state)
            (dolist (key '([escape] "q" "й"))
                (evil-local-set-key 'normal key 'quit-window))))
    (defun pop-to-command-eshell--not-a-file (name)
        (pop-to-command-eshell
            (list "echo" (concat (buffer-name) " is not visiting a file"))
            (buffer-name)
            name))
    (defun diff-unsaved-changes ()
        (interactive)
        (if (not buffer-file-name)
            (pop-to-command-eshell--not-a-file "Diff unsaved")
            (with-temporary-directory directory
                (let* ((file-name    (file-name-nondirectory buffer-file-name))
                       (file         (concat directory "/" file-name))
                       (unsaved-name (concat "unsaved " file-name))
                       (unsaved      (concat directory "/" unsaved-name))
                       (default-directory "~"))
                    (if (file-exists-p buffer-file-name)
                        (copy-file buffer-file-name file)
                        (write-region 1 1 file))
                    (write-file-no-visit unsaved)
                    (pop-to-command-eshell
                        (list "cdexec" directory "gd" file-name unsaved-name)
                        (buffer-name)
                        "Diff unsaved"
                        (apply-partially 'diff-unsaved-changes--finish
                            (current-buffer) directory)))
                (setq directory nil))))
    (defun diff-unsaved-changes--finish (buffer directory)
        (unwind-protect
            (refresh-modified-state buffer)
            (delete-directory directory t)))
    (defun diff-buffer--from (action)
        (if-let (window (next-window-other-buffer nil 'never))
            (window-buffer window)
            (read-other-buffer (concat action " from buffer: "))))
    (defun diff-buffer (buffer-1 buffer-2)
        (interactive (list (diff-buffer--from "Diff") (current-buffer)))
        (setq buffer-1 (get-buffer buffer-1))
        (setq buffer-2 (get-buffer buffer-2))
        (with-temporary-directory directory
            (let* ((name-1 (file-name-nondirectory (buffer-name buffer-1)))
                   (file-1 (concat directory "/" name-1))
                   (name-2 (file-name-nondirectory (buffer-name buffer-2)))
                   (file-2 (concat directory "/" name-2))
                   (default-directory "~"))
                (when (equal name-1 name-2)
                    (setq name-2 (concat "(2) " name-2))
                    (setq file-2 (concat directory "/" name-2)))
                (let ((jka-compr-inhibit t))
                    (with-current-buffer buffer-1
                        (write-file-no-visit file-1))
                    (with-current-buffer buffer-2
                        (write-file-no-visit file-2)))
                (pop-to-command-eshell
                    (list "cdexec" directory "gd" name-1 name-2)
                    (concat name-1 " -> " name-2)
                    "Diff buffer"
                    (apply-partially 'delete-directory directory t)))
            (setq directory nil)))
    (define-key space-map "c"
        (lambda (prefix-argument)
            (interactive "P")
            (if prefix-argument
                (become-command 'diff-buffer)
                (become-command 'diff-unsaved-changes))))
    (defun partial-save ()
        (interactive)
        (if (not buffer-file-name)
            (pop-to-command-eshell--not-a-file "Partial save")
            (with-temporary-directory directory
                (let* ((file-name (file-name-nondirectory buffer-file-name))
                       (file      (concat directory "/" file-name))
                       (unsaved   (concat directory "/unsaved " file-name))
                       (default-directory "~"))
                    (if (file-exists-p buffer-file-name)
                        (copy-file buffer-file-name file)
                        (write-region 1 1 file))
                    (write-file-no-visit unsaved)
                    (pop-to-command-eshell
                        (list "gp" file unsaved)
                        (buffer-name)
                        "Partial save"
                        (apply-partially 'partial-save--finish
                            (current-buffer) directory file)))
                (setq directory nil))))
    (defun partial-save--finish (buffer directory file)
        (unwind-protect
            (with-current-buffer buffer
                (when (or (file-exists-p buffer-file-name)
                          (> (file-size file) 0))
                    (copy-file file buffer-file-name t))
                (refresh-modified-state buffer)))
            (delete-directory directory t))
    (define-key space-map "w" 'partial-save)
    (define-key space-map "W" 'save-buffer)
    (defun partial-revert ()
        (interactive)
        (if (not buffer-file-name)
            (call-interactively 'revert-buffer)
            (with-temporary-directory directory
                (let* ((file-name (file-name-nondirectory buffer-file-name))
                       (file      (concat directory "/" file-name))
                       (unsaved   (concat directory "/unsaved " file-name))
                       (default-directory "~"))
                    (if (file-exists-p buffer-file-name)
                        (copy-file buffer-file-name file)
                        (write-region 1 1 file))
                    (write-file-no-visit unsaved)
                    (pop-to-command-eshell
                        (list "gp" unsaved file)
                        (buffer-name)
                        "Partial revert"
                        (apply-partially 'partial-revert--finish
                            (current-buffer) directory unsaved)))
                (setq directory nil))))
    (defun partial-revert--finish (buffer directory unsaved)
        (unwind-protect
            (with-current-buffer buffer
                (let ((buffer-file-name unsaved))
                    (revert-buffer t t t))
                (setq buffer-file-truename
                    (abbreviate-file-name (file-truename buffer-file-name)))
                (refresh-modified-state buffer))
            (delete-directory directory t)))
    (defun partial-copy (buffer-1 buffer-2)
        (interactive (list (diff-buffer--from "Copy") (current-buffer)))
        (setq buffer-1 (get-buffer buffer-1))
        (setq buffer-2 (get-buffer buffer-2))
        (if (not (or (derived-mode-p 'text-mode 'prog-mode)
                     (eq major-mode 'fundamental-mode)))
            (pop-to-command-eshell
                (list "echo" (concat (buffer-name buffer-2) " is special"))
                (buffer-name)
                "Partial copy")
            (with-temporary-directory directory
                (let* ((name-1 (file-name-nondirectory (buffer-name buffer-1)))
                       (file-1 (concat directory "/" name-1))
                       (name-2 (file-name-nondirectory (buffer-name buffer-2)))
                       (file-2 (concat directory "/" name-2))
                       (default-directory "~"))
                    (when (equal name-1 name-2)
                        (setq name-2 (concat "(2) " name-2))
                        (setq file-2 (concat directory "/" name-2)))
                    (let ((jka-compr-inhibit t))
                        (with-current-buffer buffer-1
                            (write-file-no-visit file-1)))
                    (with-current-buffer buffer-2
                        (write-file-no-visit file-2))
                    (pop-to-command-eshell
                        (list "gp" file-2 file-1)
                        (concat name-1 " -> " name-2)
                        "Partial copy"
                        (apply-partially 'partial-copy--finish
                            buffer-2 directory file-2)))
                (setq directory nil))))
    (defun partial-copy--finish (buffer-2 directory file-2)
        (unwind-protect
            (with-current-buffer buffer-2
                (let ((buffer-file-name file-2))
                    (revert-buffer t t t))
                (when buffer-file-name
                    (setq buffer-file-truename
                        (abbreviate-file-name (file-truename buffer-file-name)))
                    (refresh-modified-state buffer-2)))
            (delete-directory directory t)))
    (define-key space-map "r"
        (lambda (prefix-argument)
            (interactive "P")
            (if prefix-argument
                (become-command 'partial-copy)
                (become-command 'partial-revert))))
    (define-key space-map "R" 'revert-buffer)
    (defun pop-to-command-eshell--not-in-a-git-repository (name)
        (pop-to-command-eshell
            (list "echo" (concat (buffer-name) " is not in a git repository"))
            nil
            name))
    (defun git-pop-to-command (command)
        (if-let (root (vc-root-dir))
            (let ((default-directory root))
                (pop-to-command-eshell command default-directory))
            (pop-to-command-eshell--not-in-a-git-repository
                (string-join (cons "eshell:" command) " "))))
    (defmacro git (&rest arguments)
        (let ((command (cons "git" (mapcar 'symbol-name arguments))))
            `(lambda (prefix-argument)
                 (interactive "P")
                 (let ((command (list ,@command)))
                     (when prefix-argument
                         (nconc command (list
                             (or buffer-file-name
                                 (expand-file-name default-directory)))))
                     (git-pop-to-command command)))))
    (defun git--commit-ish (prefix-argument prompt)
        (if prefix-argument
            (if (integerp prefix-argument)
                (if (>= prefix-argument 0)
                    (format "HEAD~%d" prefix-argument)
                    (format "HEAD@{%d}" (- prefix-argument)))
                (read-string prompt))
            "HEAD"))
    (define-prefix-command 'git-map)
    (define-key space-map "v" 'git-map)
    (define-key git-map "v" (git status))
    (define-key git-map "l" (git log))
    (define-key git-map "L" (git log -p))
    (define-key git-map "o" (git reflog))
    (define-key git-map "p" (git stash list -p))
    (define-key git-map "d" (git diff))
    (define-key git-map "s" (git diff --staged))
    (define-key git-map "a" (git add -p))
    (define-key git-map "q" (git checkout -p))
    (define-key git-map "w" (git reset -p))
    (define-key git-map "e" (git stash -p))
    (define-key git-map "u" (git pull))
    (defun git-p (prefix-argument &optional force)
        (interactive "P")
        (let* ((commit-ish (git--commit-ish prefix-argument "git push head: "))
               (command (list "git")))
            (if (equal commit-ish "HEAD")
                (push "push" command)
                (push "p" command)
                (push commit-ish command))
            (when force
                (push "--force" command))
            (setq command (nreverse command))
            (git-pop-to-command command)))
    (define-key git-map "y" 'git-p)
    (define-key git-map "Y"
        (lambda (prefix-argument)
            (interactive "P")
            (git-p prefix-argument t)))
    (define-key git-map "t" (git push --tags))
    (define-key git-map "T" (git push --tags --force))
    (define-key git-map "c" (git commit))
    (defun git-amend--commit-or-rebase (prefix-argument)
        (if (and prefix-argument
                 (not (equal prefix-argument 0)))
            (list "git" "rebase" "--interactive"
                (git--commit-ish prefix-argument "git rebase from: "))
            (list "git" "commit" "--amend")))
    (defun git-amend (prefix-argument)
        (interactive "P")
        (let ((command (git-amend--commit-or-rebase prefix-argument)))
            (git-pop-to-command command)))
    (define-key git-map "C" 'git-amend)
    (defun git-reset (prefix-argument)
        (interactive "P")
        (let* ((commit-ish (git--commit-ish prefix-argument "git reset to: "))
               (command (list "git" "reset" commit-ish)))
            (git-pop-to-command command)))
    (define-key git-map "r" 'git-reset)
    (define-key git-map "b"
        (lambda ()
            (interactive)
            (if (vc-root-dir)
                (call-interactively 'vc-annotate)
                (pop-to-command-eshell--not-in-a-git-repository "Annotate"))))
    (define-universal-argument-space-keys git-map " v")
    (define-key git-map [escape] 'ignore)
    (define-prefix-command 'space-misc-map)
    (define-key space-map "z" 'space-misc-map)
    (define-key space-misc-map "y"
        (lambda ()
            (interactive)
            (let ((default-directory "~/Downloads"))
                (pop-to-command-eshell
                    '("sh" "-c" "yt-dlp -f bestaudio \"`p`\"") nil "yt-dlp"))))
    (define-key space-misc-map "m"
        (lambda ()
            (interactive)
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
    (defun evil-doc-view-goto-page (prefix-argument)
        (interactive "P")
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
        (setq-if-nil count 1)
        (dotimes (_ count)
            (condition-case _error
                (evil-next-line)
                (end-of-buffer (vertico-next)))))
    (evil-define-motion evil-vertico-previous-line (count)
        (setq-if-nil count 1)
        (dotimes (_ count)
            (condition-case _error
                (evil-previous-line)
                (beginning-of-buffer (vertico-previous)))))
    (defun misc-minibuffer-setup ()
        (dolist (key '([down] "j" "о"))
            (evil-local-set-key 'normal key 'evil-vertico-next-line))
        (dolist (key '([up] "k" "л"))
            (evil-local-set-key 'normal key 'evil-vertico-previous-line))
        (let ((quit (cond
                        ((memq this-command history-commands)
                            'history-quit)
                        ((eq this-command 'consult-line)
                            'consult-line-quit)
                        ((eq this-command 'consult-completion-in-region)
                            'vertico-exit)
                        (t
                            'abort-minibuffers))))
            (dolist (key '([escape] "q" "й"))
                (evil-local-set-key 'normal   key quit)
                (evil-local-set-key 'operator key quit))))
    (add-hook 'minibuffer-setup-hook 'misc-minibuffer-setup)
    (defun wrap-evil-undo-step (function &rest arguments)
        (evil-end-undo-step)
        (evil-start-undo-step)
        (apply function arguments)
        (evil-end-undo-step)
        (evil-start-undo-step))
    (advice-add 'completion-at-point :around 'wrap-evil-undo-step)
    (define-key evil-ex-completion-map [up]
        (lambda ()
            (interactive)
            (beginning-of-line)
            (previous-complete-history-element 1)
            (end-of-line)))
    (define-key evil-ex-completion-map [down]
        (lambda ()
            (interactive)
            (beginning-of-line)
            (next-complete-history-element 1)
            (end-of-line)))
    (add-hook 'isearch-mode-hook
        (lambda ()
            (define-key overriding-terminal-local-map [escape]
                (lambda ()
                    (interactive)
                    (isearch-done)))))
    (add-hook 'Info-mode-hook
        (lambda ()
            (dolist (key '("H" "Р"))
                (evil-local-set-key 'motion key 'Info-history-back))
            (dolist (key '("L" "Д"))
                (evil-local-set-key 'motion key 'Info-history-forward))
            (evil-local-set-key 'motion " " 'space-map)))
    (evil-declare-not-repeat 'ignore)
    (add-to-list 'evil-motion-state-modes 'shortdoc-mode)
    (with-current-buffer (messages-buffer)
        (evil-motion-state)))

(use-packages calendar evil
    :config
    (evil-define-key 'motion calendar-mode-map "H" 'calendar-scroll-right)
    (evil-define-key 'motion calendar-mode-map "L" 'calendar-scroll-left)
    (define-key space-map "p" (toggle datetime-read-popup-calendar t)))

(use-packages display-fill-column-indicator evil
    :config
    (evil-declare-not-repeat 'toggle-show-80+-characters)
    (define-key space-map "|" 'toggle-show-80+-characters))

(use-packages eshell eat evil
    :config
    (add-hook 'eat-eshell-exec-hook
        (lambda ()
            (evil-local-set-key 'insert "\C-v" 'eat-self-input)
            (evil-local-set-key 'insert (kbd "C-м")
                (lambda (count)
                    (interactive "p")
                    (eat-self-input count ?\C-v)))
            (dolist (key `("\C-q" ,(kbd "C-й")))
                (evil-local-set-key 'insert key 'eat-quoted-input))))
    (add-hook 'eat-eshell-exit-hook
        (lambda ()
            (dolist (key `("\C-v" ,(kbd "C-м") "\C-q" ,(kbd "C-й")))
                (evil-local-set-key 'insert key nil)))))

(use-packages hexl evil
    :config
    (defun evil-hexl-replace (count)
        (interactive "p")
        (let ((character nil))
            (unwind-protect
                (let ((evil-force-cursor 'replace))
                    (evil-refresh-cursor)
                    (setq character (evil-read-key)))
                (evil-refresh-cursor))
            (when (> count 1)
                (fixed-hexl-self-insert-command (1- count) character))
            (save-point
                (fixed-hexl-self-insert-command 1 character))))
    (evil-define-key 'normal hexl-mode-map "r" 'evil-hexl-replace)
    (define-key space-map "X" 'hexl-mode))

(use-packages evil undo-tree
    :config
    (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
    (defun evil-undo-tree-replace-state ()
        (interactive)
        (evil-replace-state)
        (overwrite-mode -1)
        (remove-hook 'pre-command-hook 'evil-replace-pre-command t)
        (add-hook 'post-command-hook (lambda () (overwrite-mode -1)) nil t)
        (undo-tree-visualizer-selection-mode -1))
    (defun evil-undo-tree-motion-state ()
        (interactive)
        (evil-motion-state)
        (undo-tree-visualizer-selection-mode 1))
    (advice-add 'undo-tree-visualize :after 'evil-undo-tree-replace-state)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        [escape] 'undo-tree-visualizer-abort)
    (dolist (key '(" " "g" "z"))
        (evil-define-key 'replace undo-tree-visualizer-mode-map
            key (lookup-key evil-motion-state-map key)))
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        "i" 'evil-undo-tree-motion-state)
    (evil-define-key 'motion undo-tree-visualizer-mode-map
        [escape] 'evil-undo-tree-replace-state)
    (define-key undo-tree-visualizer-selection-mode-map
        "q" 'evil-undo-tree-replace-state)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        [left] 'undo-tree-visualize-jump-branch-left)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        [right] 'undo-tree-visualize-jump-branch-right)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        "h" 'undo-tree-visualize-jump-branch-left)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        "j" 'undo-tree-visualize-redo)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        "k" 'undo-tree-visualize-undo)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        "l" 'undo-tree-visualize-jump-branch-right)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        [\S-left] 'undo-tree-visualize-switch-branch-left)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        [\S-right] 'undo-tree-visualize-switch-branch-right)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        "H" 'undo-tree-visualize-switch-branch-left)
    (evil-define-key 'replace undo-tree-visualizer-mode-map
        "L" 'undo-tree-visualize-switch-branch-right)
    (define-key space-map "u" 'undo-tree-visualize))

(use-packages evil pop-to-command undo-tree
    :config
    (defun hack-undo-tree-diff (&optional node)
        (with-temporary-directory directory
            (let* ((name   (file-name-nondirectory (buffer-name)))
                   (name-1 (concat "(1) " name))
                   (name-2 (concat "(2) " name))
                   (file-1 (concat directory "/" name-1))
                   (file-2 (concat directory "/" name-2))
                   (node-2 (undo-tree-current buffer-undo-tree))
                   (node-1 (or node
                               (undo-tree-node-previous node-2)
                               node-2))
                   (default-directory "~"))
                (let ((undo-tree-inhibit-kill-visualizer t))
                    (undo-tree-set node-1 'preserve-timestamps)
                    (write-file-no-visit file-1)
                    (undo-tree-set node-2 'preserve-timestamps)
                    (write-file-no-visit file-2))
                (pop-to-command-eshell
                    (list "*env" "PAGER=cat"
                        "cdexec" directory "gd" name-1 name-2)
                    nil
                    "undo-tree Diff"
                    (apply-partially 'delete-directory directory t))
                (select-window
                    (get-buffer-window undo-tree-visualizer-buffer-name))
                (setq directory nil))))
    (defun hack-undo-tree-visualizer-show-diff (&optional node)
        (setq undo-tree-visualizer-diff t)
        (with-current-buffer undo-tree-visualizer-parent-buffer
	    (hack-undo-tree-diff node)))
    (advice-add 'undo-tree-visualizer-show-diff
        :override 'hack-undo-tree-visualizer-show-diff)
    (defun hack-undo-tree-visualizer-update-diff (&optional node)
        (with-current-buffer undo-tree-visualizer-parent-buffer
	    (hack-undo-tree-diff node)))
    (advice-add 'undo-tree-visualizer-update-diff
        :override 'hack-undo-tree-visualizer-update-diff)
    (defun hack-undo-tree-visualizer-hide-diff ()
        (setq undo-tree-visualizer-diff nil)
        (if-let (window (get-buffer-window undo-tree-diff-buffer-name))
            (quit-window nil window)))
    (advice-add 'undo-tree-visualizer-hide-diff
        :override 'hack-undo-tree-visualizer-hide-diff))

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
        (dolist (key '("\C-a" "\C-d" "\C-e" "\C-k" "\C-l" "\C-u"
                       [delete] [deletechar]))
            (evil-local-set-key 'insert key 'eat-self-input))
        (dolist (key '([\C-left] [\C-right]))
            (evil-local-set-key 'insert key 'eat-self-input)
            (evil-local-set-key 'replace key 'eat-self-input))
        (evil-normal-state)
        (histdir-watch+read)
        (evil-local-set-key 'insert [up] 'histdir-input-older)
        (evil-local-set-key 'insert [down] 'histdir-input-newer)
        (evil-local-set-key 'replace [up] 'histdir-repl-evil-replace-up)
        (evil-local-set-key 'replace [down] 'histdir-repl-evil-replace-down)
        (evil-local-set-key 'insert [mouse-1] 'histdir-repl-mouse-set-point)
        (evil-local-set-key 'replace [down-mouse-1]
            'histdir-repl-mouse-set-point)
        buffer))
(defun histdir-repl-beginning-of-input ()
    (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-a")
    (sleep-for 0.04)
    (point))
(defun histdir-repl-end-of-input ()
    (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-e")
    (sleep-for 0.04)
    (point))
(defun histdir-repl-get-input (&optional position)
    (interactive)
    (let ((start (histdir-repl-beginning-of-input))
          (end   (histdir-repl-end-of-input)))
        (substring-no-properties (filter-buffer-substring start end))))
(defun histdir-repl-delete-input (&optional position)
    (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "x\C-a\C-k"))
(defun histdir-repl-replace-input (new-input &optional position)
    (interactive)
    (histdir-repl-delete-input)
    (buffer-process-send-string new-input))
(defun histdir-repl-send-input ()
    (interactive)
    (histdir-input-add (histdir-repl-get-input) t)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-m"))
(defun histdir-repl-interrupt ()
    (interactive)
    (setq histdir-buffer-local-history--position nil)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-c"))
(defun histdir-repl-forward-char-in-input (&optional count)
    (buffer-process-send-string (repeat-string "\e[C" (or count 1))))
(defun histdir-repl-backward-char-in-input (&optional count)
    (buffer-process-send-string (repeat-string "\e[D" (or count 1))))
(defun histdir-repl-backspace-char-in-input (&optional count)
    (buffer-process-send-string (repeat-string "\C-?" (or count 1))))
(defun histdir-repl-delete-char-in-input (&optional count)
    (buffer-process-send-string (repeat-string "\e[3~" (or count 1))))
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
(defun histdir-repl-insert ()
    (interactive)
    (histdir-repl-enter-input 0)
    (evil-insert-state))
(defun histdir-repl-insert-at-beginning ()
    (interactive)
    (goto-char (eat-point))
    (buffer-process-send-string "\C-a")
    (evil-insert-state))
(defun histdir-repl-append ()
    (interactive)
    (histdir-repl-enter-input 1)
    (evil-insert-state))
(defun histdir-repl-append-at-end ()
    (interactive)
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
(defun histdir-repl-delete-char (&optional count)
    (interactive "p")
    (if (and (= count 1) (= (point) (eat-point)))
        (histdir-repl-delete-char-in-input 1)
        (histdir-repl-enter+delete-input count)))
(defun histdir-repl-backspace-char (&optional count)
    (interactive "p")
    (if (and (= count 1) (= (point) (eat-point)))
        (histdir-repl-backspace-char-in-input 1)
        (let-unpack ((start point) (histdir-repl-enter-input 0))
            (setq count (min count (- point start)))
            (histdir-repl-backspace-char-in-input count))))
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
(defun histdir-repl-evil-replace (count)
    (interactive "p")
    (let ((character nil))
        (unwind-protect
            (let ((evil-force-cursor 'replace))
                (evil-refresh-cursor)
                (setq character (evil-read-key)))
            (evil-refresh-cursor))
        (histdir-repl-enter+replace-input count character)))
(defvar-local histdir-repl--evil-replace-edges nil)
(defun histdir-repl-evil-replace-state ()
    (interactive)
    (let-unpack ((start point end) (histdir-repl-enter-input 0))
        (setq histdir-repl--evil-replace-edges (list start point end end)))
    (evil-replace-state))
(defun histdir-repl-self-input+replace ()
    (interactive)
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
(defun histdir-repl-evil-replace-backspace ()
    (interactive)
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
(defun histdir-repl-evil-replace-delete ()
    (interactive)
    (let-unpack ((start-of-input start-of-replace end-of-replace end-of-input)
                     histdir-repl--evil-replace-edges)
        (call-interactively 'eat-self-input)
        (setq histdir-repl--evil-replace-edges (list
            start-of-input
            (min start-of-replace (point))
            end-of-replace
            (1- end-of-input)))))
(defun histdir-repl-evil-replace-send-input ()
    (interactive)
    (evil-insert-state)
    (histdir-repl-send-input))
(defun histdir-repl-evil-replace-interrupt ()
    (interactive)
    (evil-insert-state)
    (histdir-repl-interrupt))
(defun histdir-repl-evil-replace-tab ()
    (interactive)
    (evil-insert-state)
    (call-interactively 'eat-self-input))
(defun histdir-repl-evil-replace-up ()
    (interactive)
    (evil-insert-state)
    (histdir-input-older))
(defun histdir-repl-evil-replace-down ()
    (interactive)
    (evil-insert-state)
    (histdir-input-newer))
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
(defun histdir-repl-beginning-of-line ()
    (interactive)
    (if (histdir-repl-point-in-input-p)
        (histdir-repl-beginning-of-input)
        (beginning-of-line)))
(defun histdir-repl-end-of-line ()
    (interactive)
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
(defun histdir-repl-mouse-set-point (event &optional promote-to-region)
    (interactive "e\np")
    (mouse-set-point event promote-to-region)
    (histdir-repl-enter-input 0))
(defun histdir-repl-name (name)
    (cond
        ((string-prefix-p "zsh"    name) "sh")
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
(defvar-local face-remap-selected-window--tagged nil)
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
        (with-advice (('aw-window-list :filter-return 'hack-aw-window-list)
                      ('avy-tree :around 'hack-avy-tree))
            (aw-select "" action)))
    (defvar window-state nil)
    (defconst window-state-normal
        '("#00FF00" "#808080" "#141414" "W" "Window state"))
    (defconst window-state-target-pending
        '("#00FF00" "#C08040" "#1E140A" "T" "Target-pending window state"))
    (when termux
        (setcar (nthcdr 2 window-state-normal) "#202020")
        (setcar (nthcdr 2 window-state-target-pending) "#302010"))
    (defvar window-state-this-register nil)
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
                    (with-override-evil-mode-line-tag tag help-string
                        (fixed-aw-select 'window-state--do-action))))))
    (defun window-state--do-action (window)
        (funcall window-state--action window)
        (setq window-state-this-register nil))
    (defun window-state--normal ()
        (setq window-state--action 'aw-switch-to-window)
        (setq window-state window-state-normal))
    (defun window-state--enter (repeat)
        (setq window-state--execute-once t)
        (if window-state
            (window-state--normal)
            (let ((window-state nil))
                (window-state--normal)
                (setq window-state--execute-more repeat)
                (window-state--execute)
                (setq evil-inhibit-operator t))))
    (defun window-state-for-one-command ()
        (interactive)
        (window-state--enter nil))
    (defun window-state ()
        (interactive)
        (window-state--enter t))
    (defun window-state-quit ()
        (interactive)
        (if (eq window-state window-state-normal)
            (setq window-state--execute-once nil)
            (window-state--normal)
            (setq window-state--execute-once t)))
    (defmacro window-state-define-motion (name &rest body)
        `(defun ,name ()
             (interactive)
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
    (defun window-state--target+split ()
        (when (eq window-state--action 'window-state-target-window-prefix)
            (let ((window (selected-window)))
                (set-window-parameter window 'window-state--target+split t))))
    (window-state-define-motion window-state-split-up
        (evil-window-split)
        (window-state--target+split))
    (window-state-define-motion window-state-split-down
        (let ((evil-split-window-below t))
            (evil-window-split))
        (window-state--target+split))
    (window-state-define-motion window-state-split-left
        (evil-window-vsplit)
        (window-state--target+split))
    (window-state-define-motion window-state-split-right
        (let ((evil-vsplit-window-right t))
            (evil-window-vsplit))
        (window-state--target+split))
    (defun window-state-toggle-auto-balance ()
        (interactive)
        (setq evil-auto-balance-windows (not evil-auto-balance-windows))
        (message "evil-auto-balance-windows: %s" evil-auto-balance-windows)
        (setq window-state--execute-once t))
    (defmacro window-state--define-operator (name &rest body)
        (let ((operator-name (intern (concat (symbol-name name) "-operator"))))
            `(list
                 (defun ,name (&optional window)
                     (interactive)
                     (let ((origin-window (selected-window)))
                         (setq-if-nil window origin-window)
                         ,@body
                         (run-hooks 'buffer-list-update-hook)
                         (window-state--normal)))
                 (defun ,operator-name ()
                     (interactive)
                     (cond
                         ((eq window-state--action ',name)
                             (,name))
                         ((eq window-state--action 'aw-switch-to-window)
                             (setq window-state--action ',name)
                             (setq window-state window-state-target-pending)
                             (setq window-state--execute-once t))
                         (t
                             (keyboard-quit)))))))
    (defmacro window-state-define-operator (name &rest body)
        (let ((move-name (intern (concat (symbol-name name) "-move"))))
            `(append
                 (window-state--define-operator ,name
                     (with-selected-window window
                         ,@body))
                 (window-state--define-operator ,move-name
                     (select-window window t)
                     ,@body))))
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
        (jump-to-marker-with-scroll
            (if (or (not window-state-this-register)
                    (equal window-state-this-register ?\"))
                (ring-ref window-state--register-ring 0)
                (if (<= ?1 window-state-this-register ?9)
                    (ring-ref window-state--register-ring
                        (- window-state-this-register ?1))
                    (gethash window-state-this-register
                        window-state--register-bank)))))
    (window-state-define-operator window-state-swap
        (let ((marker1 (point-marker-with-scroll))
              (marker2 nil))
            (with-selected-window origin-window
                (setq marker2 (point-marker-with-scroll))
                (jump-to-marker-with-scroll marker1))
            (jump-to-marker-with-scroll marker2)))
    (window-state-define-operator window-state-fast-paste
        (let ((marker (with-selected-window origin-window
                          (point-marker-with-scroll))))
            (jump-to-marker-with-scroll marker)))
    (window-state-define-operator window-state-search
        (condition-case _error
            (with-advice (('read-buffer-to-switch :override 'read-buffer))
                (become-command 'switch-to-buffer))
            (quit)))
    (window-state-define-operator window-state-open
        (condition-case _error
            (call-interactively 'find-file)
            (quit)))
    (window-state-define-operator window-state-target-window-prefix
        (setq prefix-arg current-prefix-arg)
        (display-buffer-override-next-command
            (lambda-let (window) (&rest _)
                (cons window 'reuse))
            (lambda-let (origin-window window) (&rest _)
                (when (window-parameter window 'window-state--target+split)
                    (set-window-parameter window
                        'window-state--target+split nil)
                    (let ((buffer (window-buffer window)))
                        (set-window-parameter window
                            'quit-restore
                            `(window window ,origin-window ,buffer)))
                    (set-window-prev-buffers window nil)))
            "[target-window]")
        (message "Display next command buffer in target window..."))
    (window-state-define-operator window-state-bury
        (bury-buffer))
    (window-state-define-operator window-state-unbury
        (switch-to-buffer (last-buffer nil t)))
    (window-state-define-operator window-state-kill
        (kill-buffer))
    (defun window-state-use-register ()
        (condition-case _error
            (setq window-state-this-register
                (call-interactively 'evil-use-register))
            (quit))
        (setq window-state--execute-once t))
    (window-state-define-operator window-state-send
        (condition-case _error
            (let* ((keys (read-key-sequence nil))
                   (binding (key-binding keys t)))
                (setq last-command-event (aref keys (1- (length keys))))
                (if binding
                    (call-interactively binding)
                    (undefined)))
            (quit)))
    (define-prefix-command 'window-state-map)
    (define-key window-state-map "h" 'window-state-move-left)
    (define-key window-state-map "j" 'window-state-move-down)
    (define-key window-state-map "k" 'window-state-move-up)
    (define-key window-state-map "l" 'window-state-move-right)
    (define-key window-state-map "H" 'window-state-split-left)
    (define-key window-state-map "J" 'window-state-split-down)
    (define-key window-state-map "K" 'window-state-split-up)
    (define-key window-state-map "L" 'window-state-split-right)
    (define-key window-state-map "a" 'window-state-toggle-auto-balance)
    (define-key window-state-map "A" 'balance-windows)
    (define-key window-state-map "y" 'window-state-yank-operator)
    (define-key window-state-map "Y" 'window-state-yank)
    (define-key window-state-map "d" 'window-state-delete-operator)
    (define-key window-state-map "D" 'window-state-delete)
    (define-key window-state-map "p" 'window-state-paste-move-operator)
    (define-key window-state-map "P" 'window-state-paste)
    (define-key window-state-map "x" 'window-state-swap-move-operator)
    (define-key window-state-map "X" 'window-state-swap-operator)
    (define-key window-state-map "c" 'window-state-search-move-operator)
    (define-key window-state-map "C" 'window-state-search)
    (define-key window-state-map "/" 'window-state-search)
    (define-key window-state-map "n" 'switch-to-buffer-next)
    (define-key window-state-map "N" 'switch-to-buffer-previous)
    (define-key window-state-map "o" 'window-state-open-move-operator)
    (define-key window-state-map "O" 'window-state-open)
    (define-key window-state-map "w"
        'window-state-target-window-prefix-operator)
    (define-key window-state-map "W" 'window-state-target-window-prefix)
    (define-key window-state-map "f" 'window-state-bury)
    (define-key window-state-map "F" 'window-state-unbury)
    (define-key window-state-map "b" 'window-state-bury-move-operator)
    (define-key window-state-map "B" 'window-state-unbury-move-operator)
    (define-key window-state-map "r" 'window-state-kill-move-operator)
    (define-key window-state-map "R" 'window-state-kill)
    (define-key window-state-map "\"" 'window-state-use-register)
    (define-key window-state-map "s" 'window-state-send-operator)
    (define-key window-state-map "q" 'window-state-quit)
    (define-key window-state-map [escape] 'window-state-quit)
    (define-key window-state-map "\C-g" 'window-state-quit)
    (define-prefix-command 'window-state-g-map)
    (define-key window-state-map "g" 'window-state-g-map)
    (define-key window-state-g-map "p" 'window-state-fast-paste-move-operator)
    (define-key window-state-g-map "P" 'window-state-fast-paste-operator)
    (define-key window-state-g-map "q" 'window-state-quit)
    (define-key window-state-g-map [escape] 'window-state-quit)
    (define-key window-state-g-map "\C-g" 'window-state-quit)
    (setq aw-dispatch-function
        (lambda (character)
            (setq unread-command-events (cons character unread-command-events))
            (command-execute-in-keymap window-state-map)
            (let ((aw-dispatch-alist '((?q ignore))))
                (aw-dispatch-default ?q))))
    (define-key evil-motion-state-map "s" 'window-state-for-one-command)
    (define-key evil-normal-state-map "s" nil)
    (define-key evil-motion-state-map "S" 'window-state)
    (define-key evil-normal-state-map "S" nil))

(use-packages ace-window evil undo-tree
    :config
    (dolist (key '("s" "S"))
        (evil-define-key 'replace undo-tree-visualizer-mode-map
            key (lookup-key evil-motion-state-map key))))

(use-package with-editor
    :config
    (defun fixed-with-editor-return (with-editor-return cancel)
        (with-advice (('delete-file :override 'ignore)
                      ((if cancel 'save-buffer nil) :override 'ignore)
                      ('kill-buffer
                          :filter-return
                          (lambda (killed)
                              (unless killed
                                  (user-error "Not cancelled")))))
            (funcall with-editor-return cancel)))
    (advice-add 'with-editor-return :around 'fixed-with-editor-return)
    (defun fixed-with-editor-kill-buffer ()
        (call-interactively 'with-editor-cancel)
        t)
    (advice-add 'with-editor-kill-buffer-noop
        :override 'fixed-with-editor-kill-buffer)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)
    (shell-command-with-editor-mode 1))

(use-package rainbow-mode
    :config
    (define-key space-map "C" 'rainbow-mode))

(use-package markdown-mode
    :config
    (define-key markdown-mode-map [backtab] nil))

(use-packages evil markdown-mode
    :config
    (evil-define-key 'normal markdown-mode-map
        "\C-m" 'markdown-follow-thing-at-point))

(use-package denote
    :config
    (setq denote-file-type 'markdown-yaml)
    (setq denote-known-keywords '())
    (setq denote-sort-keywords nil)
    (setq denote-history-completion-in-prompts nil)
    (defun denote-file-note-type (path)
        (when-let ((extension (denote-get-file-extension-sans-encryption path))
                   (types (denote--file-types-with-extension extension)))
            (car (seq-find
                     (lambda-let (path) (type)
                         (denote--regexp-in-file-p
                             (plist-get (cdr type) :title-key-regexp)
                             path))
                     types))))
    (defconst date-t-time-regex
        (concat
            "\\(?1:[0-9][0-9][0-9][0-9]\\)"
            "\\(?2:[0-9][0-9]\\)"
            "\\(?3:[0-9][0-9]\\)"
            "\\(?10:T\\)"
            "\\(?4:[0-9][0-9]\\)"
            "\\(?5:[0-9][0-9]\\)"
            "\\(?6:[0-9][0-9]\\)"))
    (defconst p-duration-regex
        (concat
            "\\(?20:P\\)"
            "\\(?1:\\(?11:[0-9]+\\)\\(?21:Y\\)\\)?"
            "\\(?2:\\(?12:[0-9]+\\)\\(?22:M\\)\\)?"
            "\\(?7:\\(?17:[0-9]+\\)\\(?27:W\\)\\)?"
            "\\(?3:\\(?13:[0-9]+\\)\\(?23:D\\)\\)?"
            "\\(?:"
            "\\(?10:T\\)"
            "\\(?4:\\(?14:[0-9]+\\)\\(?24:H\\)\\)?"
            "\\(?5:\\(?15:[0-9]+\\)\\(?25:M\\)\\)?"
            "\\(?6:\\(?16:[0-9]+\\)\\(?26:S\\)\\)?"
            "\\)?"))
    (defconst r-repeat-p-duration-regex
        (concat
            "\\(?30:R\\)"
            "\\(?18:[0-9]*\\)"
            p-duration-regex))
    (defconst point-to-match-beginning-form
        '(progn
             (goto-char (match-beginning 0))
             (point)))
    (defconst point-to-match-end-form
        '(progn
             (goto-char (match-end 0))
             (point)))
    (setq denote-faces-file-name-keywords
        `((dired-filename-search-forward
           (,date-t-time-regex
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (1  'denote-faces-year)
               (2  'denote-faces-month)
               (3  'denote-faces-day)
               (10 'denote-faces-delimiter)
               (4  'denote-faces-hour)
               (5  'denote-faces-minute)
               (6  'denote-faces-second))
           (,r-repeat-p-duration-regex
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (30 'task-faces-repeat nil t)
               (20 'task-faces-repeat nil t)
               (21 'task-faces-repeat nil t)
               (22 'task-faces-repeat nil t)
               (27 'task-faces-repeat nil t)
               (23 'task-faces-repeat nil t)
               (10 'denote-faces-delimiter nil t)
               (24 'task-faces-repeat nil t)
               (25 'task-faces-repeat nil t)
               (26 'task-faces-repeat nil t))
           (,p-duration-regex
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (20 'task-faces-duration nil t)
               (21 'task-faces-duration nil t)
               (22 'task-faces-duration nil t)
               (27 'task-faces-duration nil t)
               (23 'task-faces-duration nil t)
               (10 'denote-faces-delimiter nil t)
               (24 'task-faces-duration nil t)
               (25 'task-faces-duration nil t)
               (26 'task-faces-duration nil t))
           ("\\..*$"
               (progn
                   (goto-char (match-beginning 0))
                   (when (equal (char-after) ?.)
                       (forward-char))
                   (point))
               ,point-to-match-end-form
               (0 'denote-faces-extension))
           ("\\(=+\\)\\([^-_=.]+\\)"
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (1 'denote-faces-delimiter)
               (2 'denote-faces-signature))
           ("\\(_+\\)\\([^_=.]+\\)"
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (1 'denote-faces-delimiter)
               (2 'denote-faces-keywords))
           ("-+\\|=+\\|_+"
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (0 'denote-faces-delimiter))
           ("[^-_=]+"
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (0 'denote-faces-title)))))
    (set-face-foreground 'denote-faces-month  "#FFA060")
    (set-face-foreground 'denote-faces-minute "#FFA060")
    (set-face-foreground 'denote-faces-extension "grey30")
    (defface task-faces-repeat   '((t :inherit default)) "")
    (defface task-faces-duration '((t :inherit default)) "")
    (set-face-foreground 'task-faces-repeat   "#FF0000")
    (set-face-foreground 'task-faces-duration "#00FF00")
    (setq denote-title-regexp "--\\([^.]*?\\)\\(==.*\\|__.*\\|\\..*\\)*$")
    (defun denoted--add-nil-id (path)
        (let ((name (file-name-nondirectory path)))
            (concat
                (file-name-directory path)
                "00000000T000000"
                (if (or (string-prefix-p "==" name)
                        (string-prefix-p "__" name))
                    ""
                    "--")
                name)))
    (defun denoted-title-get (path)
        (let ((note-type (denote-file-note-type path)))
            (if note-type
                (denote-retrieve-title-value path note-type)
                (let ((name (file-name-nondirectory path)))
                    (when (string-prefix-p "." name)
                        (setq path (substring name 1))))
                (if (denote-file-has-identifier-p path)
                    (denote-retrieve-filename-title path)
                    (denote-retrieve-filename-title
                        (denoted--add-nil-id path))))))
    (defun denoted-tag-get (path)
        (denote-extract-keywords-from-path path))
    (defun denoted-suffix-get (path)
        (denote-retrieve-filename-signature path))
    (defun denoted-datetime-get (path)
        (denote-extract-id-from-string path))
    (defun denoted-extension-get (path)
        (let ((name (file-name-nondirectory path)))
            (when (string-prefix-p "." name)
                (setq name (substring name 1)))
            (when (string-match "\\..*" name)
                (match-string 0 name))))
    (defconst denoted--exclude-regex "[^-[:alnum:]]")
    (defun denoted--slug (string)
        (string-trim
            (replace-regexp-in-string denoted--exclude-regex ""
                (replace-regexp-in-string "[ _]+" "-"
                    (downcase string)))
            "-+" "-+"))
    (defun denoted-title-slug (title)
        (replace-regexp-in-string "---+" "--"
            (denoted--slug
                (replace-regexp-in-string ";+" "-" title))))
    (defun denoted-tag--slug (tag)
        (replace-regexp-in-string "--+" "-"
            (denoted--slug tag)))
    (defun denoted-tag-slug (tags)
        (mapcar 'denoted-tag--slug tags))
    (defconst denoted-suffix--exclude-regex "[^=[:alnum:]]")
    (defun denoted-suffix-slug (suffix)
        (string-trim
            (replace-regexp-in-string "==+" "="
                (replace-regexp-in-string denoted-suffix--exclude-regex ""
                    (replace-regexp-in-string "[- _]" "="
                        (upcase suffix))))
            "=+" "=+"))
    (advice-add 'denote-sluggify-keywords :override 'identity)
    (defun denoted-rename-file-prompt (old-path new-path)
        (y-or-n-p
            (format "Rename %s to %s?"
                (propertize (file-name-nondirectory old-path)
                    'face 'denote-faces-prompt-old-name)
                (propertize (file-name-nondirectory new-path)
                    'face 'denote-faces-prompt-new-name))))
    (defun hack-rename-file (rename-file &rest arguments)
        (condition-case _error
            (apply rename-file arguments)
            (file-missing)))
    (defun denoted-rewrite-front-matter (path title tags type)
        (let* ((buffers (buffer-list))
               (buffer  (find-file-noselect path))
               (was-already-open    (memq buffer buffers))
               (had-unsaved-changes (when was-already-open
                                        (refresh-modified-state buffer)
                                        (buffer-modified-p buffer))))
            (with-advice (('y-or-n-p :override 'always))
                (denote-rewrite-front-matter path title tags type))
            (unless had-unsaved-changes
                (with-current-buffer buffer
                    (save-buffer)))
            (unless was-already-open
                (kill-buffer buffer))))
    (defun denoted--rename (path new-path directory title tags)
        (if (not (or denote-rename-no-confirm
                     (denoted-rename-file-prompt path new-path)))
            path
            (with-advice (('rename-file :around 'hack-rename-file))
                (denote-rename-file-and-buffer path new-path))
            (let ((denote-directory directory))
                (denote-update-dired-buffers))
            (when-let (type (denote-file-note-type new-path))
                (denoted-rewrite-front-matter new-path title tags type))
            new-path))
    (defun denoted-format-file-name (datetime title suffix tags extension)
        (let ((parts (list extension)))
            (when (and (length> extension 0)
                       (not (string-prefix-p "." extension)))
                (push "." parts))
            (when tags
                (dolist (tag (reverse tags))
                    (push tag parts)
                    (push "_" parts))
                (push "_" parts))
            (when (length> suffix 0)
                (push suffix parts)
                (push "==" parts))
            (when (length> title 0)
                (push title parts)
                (when (length> datetime 0)
                    (push "--" parts)))
            (when (length> datetime 0)
                (push datetime parts))
            (string-join parts)))
    (defun denoted-rename-file (path datetime title suffix tags)
        (setq-if-nil datetime "")
        (setq-if-nil title "")
        (setq-if-nil suffix "")
        (setq tags (denoted-tag-slug tags))
        (let* ((directory (file-name-directory (expand-file-name path)))
               (extension (denoted-extension-get path))
               (new-name  (denoted-format-file-name
                              datetime
                              (denoted-title-slug title)
                              (denoted-suffix-slug suffix)
                              tags
                              extension))
               (new-path  (concat directory new-name)))
            (when (string-prefix-p "." (file-name-nondirectory path))
                (setq new-path
                    (concat
                        (file-name-directory new-path)
                        "."
                        (file-name-nondirectory new-path))))
            (denoted--rename path new-path directory title tags)))
    (defun denoted-title-set (path title)
        (let ((tags     (denoted-tag-get path))
              (suffix   (denoted-suffix-get path))
              (datetime (denoted-datetime-get path)))
            (denoted-rename-file path datetime title suffix tags)))
    (defun denoted-title-edit (path)
        (let* ((title (denoted-title-get path))
               (new   (denote-title-prompt title "File title")))
            (denoted-title-set path new)))
    (defun denoted-tag-set (path tags)
        (let ((title    (denoted-title-get path))
              (suffix   (denoted-suffix-get path))
              (datetime (denoted-datetime-get path)))
            (denoted-rename-file path datetime title suffix tags)))
    (defun denoted-tag-sort (tags)
        (sort (copy-sequence tags) #'string-lessp))
    (defun denoted-tag-prompt (tags)
        (denoted-tag-slug (denote--keywords-crm tags "File tags")))
    (defun denoted-tag-add (path)
        (let* ((tags    (denoted-tag-get path))
               (options (seq-difference (denote-keywords) tags))
               (added   (denoted-tag-prompt options))
               (merged  (append tags added))
               (unique  (seq-uniq merged)))
            (denoted-tag-set path unique)))
    (defun denoted-tag-remove (path)
        (let* ((tags      (denoted-tag-get path))
               (deleted   (denoted-tag-prompt tags))
               (remaining (seq-difference tags deleted)))
            (denoted-tag-set path remaining)))
    (defun denoted-datetime-prompt (&optional default)
        (let ((input (datetime-read "File date+time: " default t)))
            (if (equal input "")
                nil
                (datetime-floor input))))
    (defun denoted-datetime-set (path datetime)
        (let ((title  (denoted-title-get path))
              (tags   (denoted-tag-get path))
              (suffix (denoted-suffix-get path)))
            (denoted-rename-file path datetime title suffix tags)))
    (defun denoted-datetime-edit (path)
        (let* ((datetime (denoted-datetime-get path))
               (new      (denoted-datetime-prompt datetime)))
            (denoted-datetime-set path new)))
    (defun denoted-suffix-set (path suffix)
        (let ((title    (denoted-title-get path))
              (tags     (denoted-tag-get path))
              (datetime (denoted-datetime-get path)))
            (denoted-rename-file path datetime title suffix tags)))
    (defun denoted-suffix-edit (path)
        (let* ((suffix (denoted-suffix-get path))
               (new    (denote-signature-prompt suffix "File suffix")))
            (denoted-suffix-set path new)))
    (defun denoted-name-prompt (&optional default)
        (read-string "File name: " default))
    (defun denoted-name-set (path name)
        (let* ((directory (file-name-directory (expand-file-name path)))
               (datetime  (denoted-datetime-get name))
               (title     (denote-retrieve-filename-title name))
               (tags      (denoted-tag-get name))
               (new-path  (concat directory name)))
            (when (equal title (denote-retrieve-filename-title path))
                (setq title (denoted-title-get path)))
            (denoted--rename path new-path directory title tags)))
    (defun denoted-name-edit (path)
        (let* ((name (file-name-nondirectory path))
               (new  (denoted-name-prompt name)))
            (denoted-name-set path new)))
    (defconst denoted-try--default-fallback
        '((user-error "%s is not visiting a file or directory" (buffer-name))))
    (defmacro denoted-try (function &rest fallback-body)
        (setq-if-nil fallback-body denoted-try--default-fallback)
        `(if-let (path (buffer-file-name))
             (,function path)
             (if (derived-mode-p 'dired-mode)
                 (dired-goto-file (,function (dired-get-filename)))
                 ,@fallback-body)))
    (defun history-or-tag-execute-or-add (prefix-argument)
        (interactive "P")
        (denoted-try denoted-tag-add
            (become-command 'history-execute)))
    (defun history-or-tag-remove (prefix-argument)
        (interactive "P")
        (denoted-try denoted-tag-remove
            (become-command 'history-remove)))
    (define-key evil-motion-state-map "gh" 'history-or-tag-execute-or-add)
    (define-key evil-motion-state-map "gr" 'history-or-tag-remove)
    (defun history-or-title-change (prefix-argument)
        (interactive "P")
        (denoted-try denoted-title-edit
            (become-command 'history-change)))
    (define-key evil-motion-state-map "gc" 'history-or-title-change)
    (defun datetime-edit ()
        (interactive)
        (denoted-try denoted-datetime-edit))
    (define-key evil-motion-state-map "gs" 'datetime-edit)
    (defun suffix-edit ()
        (interactive)
        (denoted-try denoted-suffix-edit))
    (define-key evil-motion-state-map "gS" 'suffix-edit)
    (defun name-change (prefix-argument)
        (interactive "P")
        (denoted-try denoted-name-edit))
    (define-key evil-motion-state-map "gC" 'name-change)
    (evil-define-command note (prefix-argument &optional register)
        (interactive "P<x>")
        (denote)
        (if (or prefix-argument register)
            (evil-paste-after 1 register)
            (set-buffer-modified-p nil)
            (evil-insert-state)))
    (define-key space-map "m" 'note)
    (defun note--filter-dired ()
        (dired-mark-if (member (dired-get-filename t t) '("." "..")) nil)
        (dired-do-kill-lines))
    (defun note-list ()
        (interactive)
        (dired denote-directory)
        (dired-hide-details-mode 1)
        (denote-dired-mode 1)
        (revert-buffer)
        (note--filter-dired))
    (define-key space-map "M"
        (lambda ()
            (interactive)
            (note-list)
            (dired-goto-last-file)))
    (define-key space-search-map "m"
        (lambda ()
            (interactive)
            (note-list)
            (dired-goto-first-file)
            (run-with-idle-timer 0 nil
                (lambda ()
                    (condition-case error
                        (consult-line)
                        (quit
                            (quit-window)
                            (signal (car error) (cdr error))))
                    (dired-find-file)
                    (dired denote-directory)
                    (bury-buffer)))))
    (defconst task-tag "qq")
    (setq denote-excluded-keywords-regexp "qq.*")
    (defun task-prompt ()
        (interactive)
        (denote-title-prompt nil "Task"))
    (defun task--filter-dired ()
        (dired-mark-files-regexp (concat "_" task-tag))
        (dired-toggle-marks)
        (dired-do-kill-lines))
    (defun task--narrow-dired ()
        (widen)
        (narrow-to-region
            (save-excursion
                (goto-char 1)
                (beginning-of-line 2)
                (point))
            (point-max)))
    (defun task--buffer (title regex)
        (reuse-independent-dired title denote-directory)
        (dired-hide-details-mode 1)
        (denote-dired-mode 1)
        (setq-local revert-buffer-function (task--buffer-revert regex))
        (revert-buffer)
        (dired-goto-first-file))
    (defun task--buffer-revert (filter-regex)
        (lambda-let (filter-regex) (&rest arguments)
            (apply 'dired-revert arguments)
            (note--filter-dired)
            (task--filter-dired)
            (when filter-regex
                (dired-mark-files-regexp filter-regex)
                (dired-toggle-marks)
                (dired-do-kill-lines))
            (task--narrow-dired)))
    (defun task-list ()
        (interactive)
        (task--buffer "*Tasks*" nil))
    (defvar-local task-list--tags ())
    (defvar-local task-list--datetime nil)
    (defun task--delete-update (&rest _)
        (when (equal (expand-file-name dired-directory) denote-directory)
            (denote-update-dired-buffers)))
    (advice-add 'dired-do-delete :after 'task--delete-update)
    (advice-add 'dired-do-flagged-delete :after 'task--delete-update)
    (defun task-create (prefix-argument)
        (interactive "P")
        (let (path
              (datetime (if prefix-argument
                            (datetime-read nil task-list--datetime)
                            (when task-list--datetime
                                (datetime-floor task-list--datetime)))))
            (denote (task-prompt) (cons task-tag task-list--tags)
                    nil nil datetime)
            (setq path buffer-file-name)
            (basic-save-buffer)
            (kill-buffer)
            (denote-update-dired-buffers)
            (unless (string-prefix-p "*Tasks" (buffer-name))
                (task-list))
            (dired-goto-file path)))
    (define-key space-map "g" 'task-create)
    (define-key space-map "G" 'task-list)
    (defun task-tags ()
        (denoted-tag-sort
            (seq-uniq
                (delete task-tag
                    (mapcan 'denote-extract-keywords-from-path
                        (denote-directory-files (concat "_" task-tag)))))))
    (defun task-tag-search (tags)
        (interactive (list (denote--keywords-crm (task-tags) "Task tags")))
        (let* ((tags  (denoted-tag-sort tags))
               (regex (concat "_" (regexp-opt-group tags t) "[_.]"))
               (title (concat "*Tasks (" (string-join tags ",") ")*")))
            (task--buffer title regex)
            (setq-local task-list--tags tags)))
    (define-key space-search-map "h" 'task-tag-search)
    (defun task-datetime-search (datetime)
        (interactive (list (datetime-read "Task date+time: " nil t)))
        (let ((regex (concat "^" datetime))
              (title (format "*Tasks (%s)*" (datetime-expand datetime t))))
            (task--buffer title regex)
            (setq-local task-list--datetime datetime)))
    (define-key space-search-map "a" 'task-datetime-search)
    (defun task-agenda (prefix-argument)
        (interactive "P")
        (let ((day (decode-time (current-time))))
            (when prefix-argument
                (let ((one-day (make-decoded-time :day 1)))
                    (setq day (fixed-decoded-time-add day one-day))))
            (let ((date (format-time-string "%Y%m%d" (encode-time day))))
                (task-datetime-search date))))
    (define-key space-map "a" 'task-agenda)
    (defun task--parse-datetime (datetime)
        (string-match date-t-time-regex datetime)
        (let ((years   (string-to-number-or-nil (match-string 1 datetime)))
              (months  (string-to-number-or-nil (match-string 2 datetime)))
              (days    (string-to-number-or-nil (match-string 3 datetime)))
              (hours   (string-to-number-or-nil (match-string 4 datetime)))
              (minutes (string-to-number-or-nil (match-string 5 datetime)))
              (seconds (string-to-number-or-nil (match-string 6 datetime))))
            (list seconds minutes hours days months years)))
    (defun task--parse-repetition (string)
        (when (equal (string-match r-repeat-p-duration-regex string) 0)
            (let ((count   (string-to-number-or-nil (match-string 18 string)))
                  (years   (string-to-number-or-nil (match-string 11 string)))
                  (months  (string-to-number-or-nil (match-string 12 string)))
                  (weeks   (string-to-number-or-nil (match-string 17 string)))
                  (days    (string-to-number-or-nil (match-string 13 string)))
                  (hours   (string-to-number-or-nil (match-string 14 string)))
                  (minutes (string-to-number-or-nil (match-string 15 string)))
                  (seconds (string-to-number-or-nil (match-string 16 string))))
                (setq days (+ (or days 0) (* (or weeks 0) 7)))
                (list count seconds minutes hours days months years))))
    (defun task--step-datetime (datetime repetition steps)
        (let* ((parsed (task--parse-datetime datetime))
               (delta  (cdr repetition))
               (count  (car repetition))
               (steps  (min steps (or count steps)))
               (next   (decoded-time-iterate parsed delta steps)))
            (apply 'format "%6$04d%5$02d%4$02dT%3$02d%2$02d%1$02d" next)))
    (defun task--step-repetition (string parsed steps)
        (let ((count  (pop parsed))
              (period (cadr (string-split string "P"))))
            (if count
                (progn
                    (setq steps (min steps count))
                    (setq count (- count steps))
                    (format "R%sP%s" count period))
                (format "RP%s" period))))
    (defun task-schedule (datetime)
        (let ((denote-rename-no-confirm t))
            (dired-goto-file
                (denoted-datetime-set (dired-get-filename) datetime))))
    (defun task-schedule-repetition (path steps)
        (let* ((datetime (denoted-datetime-get path))
               (suffix   (denoted-suffix-get path))
               (parts    (string-split suffix "=")))
            (catch 'break
                (dotimes (index (length parts))
                    (let ((part (nth index parts)))
                        (when-let (repetition (task--parse-repetition part))
                            (setq datetime
                                (task--step-datetime datetime repetition steps))
                            (setcar (nthcdr index parts)
                                (task--step-repetition part repetition steps))
                            (throw 'break nil)))))
            (setq suffix (string-join parts "="))
            (let ((denote-rename-no-confirm t))
                (dired-goto-file
                    (denoted-rename-file
                        path
                        datetime
                        (denoted-title-get path)
                        suffix
                        (denoted-tag-get path))))))
    (defun task-next-repetition (count)
        (interactive "p")
        (task-schedule-repetition (dired-get-filename) count))
    (defun task-previous-repetition (count)
        (interactive "p")
        (task-schedule-repetition (dired-get-filename) (- count)))
    (define-key evil-motion-state-map "zj" 'task-next-repetition)
    (define-key evil-motion-state-map "zk" 'task-previous-repetition)
    (define-key space-map "j"
        (lambda (prefix-argument)
            (interactive "P")
                (let ((position (point-position-with-scroll)))
                    (if prefix-argument
                        (task-next-repetition 1)
                        (task-schedule (format-time-string
                                           denote-id-format (current-time))))
                    (jump-to-position-with-scroll position))))
    (define-key space-map "J"
        (lambda (prefix-argument)
            (interactive "P")
            (task-schedule
                (datetime-read nil
                    (when prefix-argument
                        (denoted-datetime-get (dired-get-filename))))))))


(defconst tumblr--python (expand-file-name "~/.tumblr/venv/bin/python"))
(defconst tumblr--script (expand-file-name "~/.tumblr/tumblr.py"))
(defvar tumblr-blogs '(
    "mentalisttraceur"
    "mentalisttraceur-humor"
    "mentalisttraceur-process"
    "mentalisttraceur-reactions"
    "mentalisttraceur-software"))
(defun tumblr-prompt-for-blog ()
    (completing-read "Tumblr blog: " tumblr-blogs nil 'confirm))
(defun tumblr (&rest arguments)
    (let-unpack ((status output) (apply-process tumblr--python
                                     tumblr--script arguments))
         (if (equal status 0)
             output
             (error "tumblr error %s" output))))
(defmacro tumblr--ensure-file (&rest body)
    `(let ((already-existed (file-exists-p buffer-file-name)))
         (basic-save-buffer)
         (unwind-protect
             (progn
                 ,@body
                 (revert-buffer t t))
             (unless already-existed
                 (delete-file buffer-file-name)
                 (refresh-modified-state)))
         (run-hooks 'buffer-list-update-hook)))
(defun tumblr-publish ()
    (interactive)
    (tumblr--ensure-file
        (let ((blog (tumblr "get" "blog" buffer-file-name)))
            (when (equal blog "")
                (setq blog (tumblr-prompt-for-blog)))
            (tumblr "publish" buffer-file-name blog))))
(defun tumblr-delete ()
    (interactive)
    (tumblr--ensure-file
        (tumblr "delete" buffer-file-name)))
(define-key space-misc-map "w" 'tumblr-publish)
(define-key space-misc-map "d" 'tumblr-delete)

(defconst russian-vi-letter-pairs
    '(("й" "q")
      ("ц" "w")
      ("у" "e")
      ("к" "r")
      ("е" "t")
      ("н" "y")
      ("г" "u")
      ("ш" "i")
      ("щ" "o")
      ("з" "p")
      ("ф" "a")
      ("ы" "s")
      ("в" "d")
      ("а" "f")
      ("п" "g")
      ("р" "h")
      ("о" "j")
      ("л" "k")
      ("д" "l")
      ("я" "z")
      ("ч" "x")
      ("с" "c")
      ("м" "v")
      ("и" "b")
      ("т" "n")
      ("ь" "m")))
(defconst russian-vi-symbol-pairs
    '(("ё" "`")
      ("Ё" "~")
      ("№" "#")
      ("х" "[")
      ("Х" "{")
      ("ъ" "]")
      ("Ъ" "}")
      ("ж" ";")
      ("Ж" ":")
      ("э" "'")
      ("Э" "\"")
      ("б" ",")
      ("Б" "<")
      ("ю" ".")
      ("Ю" ">")))
(defun russian-vi-bind--1 (state keymap russian-key english-key)
    (when-let (binding (lookup-evil-key state keymap english-key))
        (evil-define-key state keymap russian-key binding)
        (when (keymapp binding)
            (when (symbolp binding)
                (setq binding (symbol-function binding)))
            (russian-vi-bind binding state))))
(defun russian-vi-bind (map &optional state)
    (dolist (pair russian-vi-symbol-pairs)
        (let-unpack ((russian english) pair)
            (russian-vi-bind--1 state map russian english)))
    (dolist (pair russian-vi-letter-pairs)
        (let-unpack ((russian english) pair)
            (russian-vi-bind--1 state map russian english)
            (let ((russian (upcase russian))
                  (english (upcase english)))
                (russian-vi-bind--1 state map russian english))
            (let ((russian (kbd (concat "C-" russian)))
                  (english (kbd (concat "C-" english))))
                (russian-vi-bind--1 state map russian english)))))
(defun russian-vi-letter-map--1 (string pair)
    (let-unpack ((russian english) pair)
        (string-replace english russian string)))
(defun russian-vi-letter-map (english-string)
    (seq-reduce 'russian-vi-letter-map--1
        russian-vi-letter-pairs english-string))

(use-package evil
    :config
    (dolist (ex-command evil-ex-commands)
        (let-uncons (command definition ex-command)
            (let ((mapped (russian-vi-letter-map command)))
                (unless (or (equal mapped command)
                            (member mapped evil-ex-commands))
                    (evil-ex-define-cmd mapped definition))))))
(use-packages calendar help ace-window consult evil undo-tree
    :config
    (dolist (map (list evil-motion-state-map evil-normal-state-map
                       evil-visual-state-map evil-operator-state-map
                       space-map
                       window-state-map
                       undo-tree-visualizer-mode-map
                       calendar-mode-map global-map help-map))
        (russian-vi-bind map))
    (russian-vi-bind undo-tree-visualizer-mode-map 'replace))


(setq gc-cons-threshold init-gc-cons-threshold
      file-name-handler-alist init-file-name-handler-alist)


(setq initial-buffer-choice 'eshell)
