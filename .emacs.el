(setq backup-inhibited t)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)


(defconst termux (getenv "TERMUX_VERSION"))
(defconst wsl (getenv "WSL_DISTRO_NAME"))


(menu-bar-mode -1)
(defun init-graphic-frame (frame)
    (if (not (display-graphic-p frame))
        (if termux
            (set-face-background 'default "#202020")
            (set-face-background 'default "#141414"))
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (select-frame frame)
        (set-frame-font "DejaVu Sans Mono-11.5")
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


(defun fixed-native-compile-async-skip-p
        (native-compile-async-skip-p file load selector)
    (let* ((naive-elc-file (file-name-with-extension file "elc"))
           (elc-file       (replace-regexp-in-string
                               "\\.el\\.elc$" ".elc" naive-elc-file)))
        (or (gethash elc-file comp--no-native-compile)
            (funcall native-compile-async-skip-p file load selector))))

(advice-add 'native-compile-async-skip-p
    :around 'fixed-native-compile-async-skip-p)


(when wsl
    (defun replace-invalid-unicode-1 (character)
        (if (> character #x10FFFF)
            #xFFFD
            character))
    (defun replace-invalid-unicode (string)
        (apply 'string (mapcar 'replace-invalid-unicode-1 string)))
    (defun hack-xselect--encode-string (arguments)
        (let ((type (car arguments))
              (string (cadr arguments)))
            (when (and type string)
                (setq string (replace-invalid-unicode string))
                (setcar (cdr arguments) string)))
        arguments)
    (advice-add 'xselect--encode-string
        :filter-args 'hack-xselect--encode-string)
    (define-key global-map [XF86AudioLowerVolume] 'ignore)
    (define-key global-map [XF86AudioRaiseVolume] 'ignore))


(set-face-background 'highlight "#003800")

(when termux
    (define-key minibuffer-inactive-mode-map [mouse-1] nil)
    (define-key mode-line-buffer-identification-keymap
        [mode-line mouse-1] 'ignore)
    (defun gui-backend-set-selection (_selection-type data)
        (call-process-region data nil "termux-clipboard-set" nil 0))
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


(defun fixed-mouse-set-point (&rest _)
    (setq temporary-goal-column
          (if (equal (char-after) ?\n)
              most-positive-fixnum
              (current-column))))

(advice-add 'mouse-set-point :after 'fixed-mouse-set-point)


(setq-default mode-line-buffer-identification
    (propertized-buffer-identification "%b"))

(setcar (member "   " mode-line-format) "  ")

(setcar (cdr (assq 'vc-mode mode-line-format)) "")


(when termux
    (define-key mode-line-major-mode-keymap [mode-line down-mouse-1] 'ignore)
    (define-key mode-line-minor-mode-keymap [mode-line down-mouse-1] 'ignore))


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


(setq display-buffer-alist '((t display-buffer-same-window)))


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
    (let* ((counter 1)
           (bindings ())
           (function (pop functions))
           (function (if (memq (car-safe function) '(quote function))
                         (list 'function (cadr function))
                         (compose--gensym)))
           (form `(apply ,function arguments)))
        (while functions
            (setq counter (1+ counter))
            (setq function (pop functions))
            (if (memq (car-safe function) '(quote function))
                (setq form `(,(cadr function) ,form))
                (setq form `(funcall ,(compose--gensym) ,form))))
        (push 'list bindings)
        `(eval
             '(lambda (&rest arguments)
                  ,form)
             ,bindings)))

(defmacro compose--gensym ()
    `(let* ((gensym-counter counter)
            (symbol (if (symbolp function)
                        function
                        (gensym "f"))))
         (push `(cons ',symbol ,function) bindings)
         symbol))


(defvar multiple-values--rest nil)

(defun values (&rest objects)
    (setq multiple-values--rest nil)
    (prog1
        (pop objects)
        (setq multiple-values--rest objects)))

(defmacro multiple-values-bind (names form &rest body)
    (let ((name (pop names))
          (rest (make-symbol "rest")))
        `(let (,name ,rest)
             (let ((multiple-values--rest nil))
                 (setq ,name ,form)
                 (setq ,rest multiple-values--rest))
             (seq-let ,names ,rest
                 ,@body))))


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


(defmacro augmented-assignment (operator place &rest operands)
    (gv-get place
        (lambda-let (operator operands) (getter setter)
            (funcall setter `(,operator ,getter ,@operands)))))

(defmacro define-augmented-assignment (name operator)
    `(defmacro ,name (place &rest operands)
         (apply 'list 'augmented-assignment ',operator place operands)))

(define-augmented-assignment += +)
(define-augmented-assignment -= -)
(define-augmented-assignment ^= logxor)

(defmacro <<= (place count)
    `(augmented-assignment ash ,place ,count))


(defmacro until (test &rest body)
    `(while (not ,test)
         ,@body))


(defmacro wait-while (test check-interval)
    `(let ((--wait-while--interval-- ,check-interval)
           (--wait-while--seconds--)
           (--wait-while--result--))
         (while (and (setq --wait-while--result-- ,test)
                     (setq --wait-while--seconds--
                           (cond
                               ((functionp --wait-while--interval--)
                                   (funcall --wait-while--interval--))
                               ((listp --wait-while--interval--)
                                   (pop --wait-while--interval--))
                               (t
                                   --wait-while--interval--))))
             (sleep-for --wait-while--seconds--))
           --wait-while--result--))

(defmacro wait-until (test check-interval)
    `(let ((--wait-until-- nil))
         (wait-while
            (not (setq --wait-until-- ,test))
            ,check-interval)
         --wait-until--))


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


(defun form-replace (from-forms to-forms in-forms)
    (when in-forms
        (let ((unmatched-in-forms in-forms)
              (unmatched-from-forms from-forms))
            (while (and unmatched-in-forms
                        unmatched-from-forms
                        (equal (car unmatched-in-forms)
                               (car unmatched-from-forms)))
                (pop unmatched-in-forms)
                (pop unmatched-from-forms))
            (if unmatched-from-forms
                (nconc
                    (form-replace--car from-forms to-forms (car in-forms))
                    (form-replace      from-forms to-forms (cdr in-forms)))
                (nconc
                    (copy-sequence to-forms)
                    (form-replace from-forms to-forms unmatched-in-forms))))))

(defun form-replace--car (from-forms to-forms nested-form)
    (if (consp nested-form)
        (list (form-replace from-forms to-forms nested-form))
        (if (and (equal nested-form (car from-forms))
                 (not (cdr from-forms)))
            (copy-sequence to-forms)
            (list nested-form))))


(defun function-lisp (function)
    (let ((raw (indirect-function function)))
        (while (advice--p raw)
            (setq raw (advice--cdr raw)))
        (if (or (subr-native-elisp-p raw)
                (byte-code-function-p raw)
                (autoloadp raw))
            (if-let ((source (find-function-library function))
                     (file (cdr source)))
                (function-lisp--read-from-source (car source) file)
                nil)
            (if (subrp raw)
                nil
                raw))))

(autoload 'find-function-library "find-func")

(defun function-lisp--read-from-source (name file)
    (let* ((buffers (buffer-list))
           (found (find-function-search-for-symbol name nil file))
           (buffer   (car found))
           (position (cdr found))
           (was-already-open (memq buffer buffers)))
        (prog1
            (read (set-marker (make-marker) position buffer))
            (unless was-already-open
                (kill-buffer buffer)))))

(defun function-lisp-anonymize (definition)
    (cond
        ((memq (car definition) '(defun defsubst))
            (cons 'lambda (cddr definition)))
        ((eq (car definition) 'defmacro)
            (cons 'macro (cons 'lambda (cddr definition))))
        (t
            definition)))


(defun advice-how (symbol function)
    (when-let (advice (advice-member-p function symbol))
        (advice--how advice)))

(defun advice-list (symbol)
    (let ((advice-list ()))
        (advice-mapc
            (lambda-let (symbol) (function properties)
                (let ((how (advice-how symbol function))
                      (advice ()))
                    (when properties
                        (push properties advice))
                    (push function advice)
                    (push how advice)
                    (push advice advice-list)))
            symbol)
        advice-list))

(defun advice-remove-all (symbol)
    (dolist (advice (advice-list symbol))
        (let-unpack ((_how function) advice)
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
                 (setcdr (cdr --with-advice-1--) nil)
                 (apply 'advice-remove --with-advice-1--)))))

(defmacro with-advice (advice-list &rest body)
    `(apply-split-nest with-advice-1 ,advice-list 1 ,body))

(defmacro without-advice-1 (advice-remove-arguments &rest body)
    `(let ((--without-advice-1-- (list ,@advice-remove-arguments)))
         (if-let (how (apply 'advice-how --without-advice-1--))
             (unwind-protect
                 (progn
                     (apply 'advice-remove --without-advice-1--)
                     ,@body)
                 (setcdr --without-advice-1--
                         (cons how (cdr --without-advice-1--)))
                 (apply 'advice-add --without-advice-1--))
             ,@body)))

(defmacro without-advice (advice-list &rest body)
    `(apply-split-nest without-advice-1 ,advice-list 1 ,body))

(defmacro without-advice-all-1 (symbol &rest body)
    `(let ((--without-advice-all-1-- (advice-list ,symbol)))
         (unwind-protect
             (progn
                 (advice-remove-all ,symbol)
                 ,@body)
             (dolist (advice --without-advice-all-1--)
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


(defmacro with-variable-watcher-1 (add-variable-watcher-arguments &rest body)
    `(let ((--with-variable-watcher-1-- (list ,@add-variable-watcher-arguments)))
         (unwind-protect
             (progn
                 (condition-case error
                     (apply 'add-variable-watcher --with-variable-watcher-1--)
                     (wrong-number-of-arguments
                         (setq --with-variable-watcher-1-- nil)
                         (signal (car error) (cdr error))))
                 ,@body)
             (when --with-variable-watcher-1--
                 (apply 'remove-variable-watcher --with-variable-watcher-1--)))))

(defmacro with-variable-watcher (watcher-list &rest body)
    `(apply-split-nest with-variable-watcher-1 ,watcher-list 1 ,body))


(defmacro without-local-variable-1 (symbol &rest body)
    `(let ((--without-local-variable-1-- (when (local-variable-p ,symbol)
                                             (if (boundp ,symbol)
                                                 (cons t (symbol-value ,symbol))
                                                 (cons nil nil)))))
         (unwind-protect
             (progn
                 (kill-local-variable ,symbol)
                 ,@body)
             (when --without-local-variable-1--
                 (make-local-variable ,symbol)
                 (when (car --without-local-variable-1--)
                     (set ,symbol (cdr --without-local-variable-1--)))))))

(defmacro without-local-variable (symbol-list &rest body)
    `(apply-split-nest without-local-variable-1 ,symbol-list 1 ,body))


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


(defvar temporary-goal-column-preserving-commands
    '(next-line previous-line))
(defun hack-line-move-1 (line-move-1 &rest arguments)
    (if (memq last-command temporary-goal-column-preserving-commands)
        (let ((last-command 'next-line))
            (apply line-move-1 arguments))
        (apply line-move-1 arguments)))
(advice-add 'line-move-1 :around 'hack-line-move-1)


(defun loglsb (integer)
    (logand integer (- integer)))
(defun logfls (integer)
    (1- (logb (logxor 1 (ash integer 1)))))
(defun logffs (integer)
    (logfls (loglsb integer)))
(defun logmsb (integer)
    (ash 1 (logfls integer)))


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
        (dolist (item (nreverse items) dlist)
            (setq dlist (dlist-cons item dlist)))))

(defun dlist-list (dlist)
    (cdr dlist))

(defun dlist-car (dlist)
    (cadr dlist))

(defun dlist-cdr (dlist)
    (cdar dlist))

(defun dlist-cpr (dlist)
    (caar dlist))

(defun dlist-nthcdr (n dlist)
    (while (and dlist (> n 0))
        (setq dlist (dlist-cdr dlist))
        (setq n (1- n)))
    dlist)

(defun dlist-nthcpr (n dlist)
    (while (and dlist (> n 0))
        (setq dlist (dlist-cpr dlist))
        (setq n (1- n)))
    dlist)

(defun dlist-nth (n dlist)
    (if (< n 0)
        (dlist-car (dlist-nthcpr (- n) dlist))
        (dlist-car (dlist-nthcdr n dlist))))

(defun dlist-setcar (dlist newcar)
    (setcar (cdr dlist) newcar))

(defun dlist-setcdr (dlist newcdr)
    (setcdr (cdr dlist) (cdr newcdr))
    (setcdr (car dlist) newcdr))

(defun dlist-setcpr (dlist newcpr)
    (setcar (car dlist) newcpr))

(defun dlist-link (link1 link2)
    (when link1
        (dlist-setcdr link1 link2))
    (when link2
        (dlist-setcpr link2 link1))
    nil)

(defun dlist-unlink (link)
    (let ((headward-link (caar link))
          (tailward-link (cdar link)))
        (dlist-link headward-link tailward-link)))

(defun dlist-first (dlist)
    (while-let ((previous (dlist-cpr dlist)))
        (setq dlist previous))
    dlist)

(defun dlist-last (dlist)
    (while-let ((next (dlist-cdr dlist)))
        (setq dlist next))
    dlist)

(defun dlist-p (object)
    (and (listp object)
         (listp (car object))
         (listp (caar object))
         (listp (cdar object))
         (listp (cdr object))))

(defun dlist-insert (link1 item link2)
    (let ((new-link (dlist-cons item link2)))
        (dlist-link link1 new-link)
        new-link))

(defun dlist-insert-before (link item)
    (dlist-insert (dlist-cpr link) item link))

(defun dlist-insert-after (link item)
    (dlist-insert link item (dlist-cdr link)))


(defun make-ordered-hash-table (&rest arguments)
    (record 'ordered-hash-table (apply 'make-hash-table arguments) nil))

(defun ordered-hash-table-get (table key &optional default)
    (if-let (entry (gethash key (aref table 1)))
        (dlist-car entry)
        default))

(defun ordered-hash-table-pop (table key)
    (let ((hash-table (aref table 1)))
        (when-let (entry (gethash key hash-table))
            (remhash key hash-table)
            (when (eq entry (aref table 2))
                (aset table 2 (dlist-cdr entry)))
            (dlist-unlink entry)
            (dlist-car entry))))

(defun ordered-hash-table-put (table key value)
    (ordered-hash-table-pop table key)
    (let ((entry (dlist-cons value (aref table 2))))
        (puthash key entry (aref table 1))
        (aset table 2 entry))
    value)

(defun ordered-hash-table-list (table)
    (dlist-list (aref table 2)))


(defun make-bihash (&rest arguments)
    (let ((tail arguments)
          (key-test nil)
          (value-test nil)
          table
          inverse-table
          bihash
          inverse-bihash)
        (setq arguments nil)
        (while (and (consp tail) (consp (cdr tail)))
            (cond
                ((and (not key-test) (eq (car tail) :key-test))
                    (setq key-test (cadr tail)))
                ((and (not value-test) (eq (car tail) :value-test))
                    (setq value-test (cadr tail)))
                (t
                    (push (car tail) arguments)
                    (push (cadr tail) arguments)))
            (setq tail (cddr tail)))
        (setq-if-nil key-test 'eql)
        (setq-if-nil value-test 'eql)
        (setq arguments (nreverse arguments))
        (setq arguments (nconc arguments tail))
        (push key-test arguments)
        (push :test arguments)
        (setq table (apply 'make-hash-table arguments))
        (setq weakness (plist-get arguments :weakness))
        (setq weakness (cond
                           ((eq weakness 'key)
                               'value)
                           ((eq weakness 'value)
                               'key)
                           (t
                               weakness)))
        (plist-put arguments :weakness weakness)
        (setcar (cdr arguments) value-test)
        (setq inverse-table (apply 'make-hash-table arguments))
        (setq inverse-bihash (record 'bihash inverse-table nil))
        (setq bihash (record 'bihash table inverse-bihash))
        (aset inverse-bihash 2 bihash)))

(defun bihash-p (object)
    (eq (type-of object) 'bihash))

(defun bihash--table (bihash)
    (aref bihash 1))

(defun bihash-inverse (bihash)
    (aref bihash 2))

(defun bihash-get (bihash key &optional default)
    (gethash key (bihash--table bihash) default))

(defun bihash-pop (bihash key)
    (let ((forward-table (bihash--table bihash))
          (inverse-table (bihash--table (bihash-inverse bihash))))
        (when-let (value (gethash key forward-table))
            (remhash key forward-table)
            (remhash value inverse-table)
            value)))

(defun bihash-put (bihash key value)
    (bihash-pop bihash key)
    (bihash-pop (bihash-inverse bihash) value)
    (puthash value key (bihash--table (bihash-inverse bihash)))
    (puthash key value (bihash--table bihash)))

(defun bihash-count (bihash)
    (hash-table-count (bihash--table bihash)))


(defun make-sorted-hash-table (predicate &rest arguments)
    (if-let (test-cell (plist-member arguments :test))
        (setcar test-cell :key-test)
        (push 'eql arguments)
        (push :key-test arguments))
    (push 'eq arguments)
    (push :value-test arguments)
    (record 'sorted-hash-table
        predicate
        (apply 'make-bihash arguments)
        nil
        nil))

(defun sorted-hash-table-get (table key &optional default)
    (if-let (entry (bihash-get (aref table 2) key))
        (dlist-car entry)
        default))

(defun sorted-hash-table-pop (table key)
    (let ((bihash (aref table 2)))
        (when-let (entry (bihash-pop bihash key))
            (when (eq entry (aref table 3))
                (aset table 3 (dlist-cdr entry)))
            (when (eq entry (aref table 4))
                (aset table 4 (dlist-cpr entry)))
            (dlist-unlink entry)
            (dlist-car entry))))

(defun sorted-hash-table-put (table key value)
    (if-let (entry (bihash-get (aref table 2) key))
        (dlist-setcar entry value)
        (let ((predicate (aref table 1))
              (keys      (bihash-inverse (aref table 2)))
              (link1     (aref table 3))
              (link2     (aref table 4))
              (new-link  nil)
              key1 key2)
            (when (not link1)
                (setq new-link (dlist value)))
            (while (not new-link)
                (setq key1 (bihash-get keys link1))
                (if (not (funcall predicate key1 key))
                    (setq new-link (dlist-insert-before link1 value))
                    (setq link1 (dlist-cdr link1))
                    (setq key2 (bihash-get keys link2))
                    (if (funcall predicate key2 key)
                        (setq new-link (dlist-insert-after link2 value))
                        (setq link2 (dlist-cpr link2)))))
            (unless (dlist-cpr new-link)
                (aset table 3 new-link))
            (unless (dlist-cdr new-link)
                (aset table 4 new-link))
            (bihash-put (aref table 2) key new-link))
        value))

(defun sorted-hash-table-count (table)
    (bihash-count (aref table 2)))


(defun fixed-line-move-visual (count &optional no-error to-end try-vscroll)
    (let ((line-move-visual t)
          step)
        (if (> count 0)
            (setq step 1)
            (setq step -1)
            (setq count (- count)))
        (dotimes (_ count)
            (line-move step no-error to-end try-vscroll))))

(defun count-visual-lines (start end &optional max-count)
    (if truncate-lines
        (count-lines start end)
        (setq start (max (point-min) (min start (point-max))))
        (setq end (max (point-min) (min end (point-max))))
        (when (< end start)
            (let ((temporary start))
                (setq start end)
                (setq end temporary)))
        (unless max-count
            (setq max-count most-positive-fixnum))
        (save-excursion
            (goto-char start)
            (let ((count 0))
                (while (and (< (point) end)
                            (< count max-count))
                    (setq start (point))
                    (end-of-visual-line)
                    (when (and (eolp) (not (eobp)))
                        (forward-char))
                    (if (= (point) start)
                        (forward-char)
                        (setq count (1+ count))))
                count))))


(defun delete-forward-in-line (start count)
    (delete-region start (save-excursion
        (goto-char start)
        (move-to-column (+ (current-column) count))
        (point))))

(defun delete-lines (start count)
    (save-excursion
        (goto-char start)
        (delete-region (pos-bol) (pos-bol (+ count 1)))))


(defun scroll-to-fill-window ()
    (let ((window-height (floor (window-screen-lines)))
          (used-lines (count-visual-lines
                          (window-start)
                          (window-end nil t))))
        (when (< used-lines window-height)
            (ignore-error 'beginning-of-buffer
                (scroll-down (- window-height used-lines))))))


(defun repeat-string (string count)
    (string-join (make-list count string)))

(defun string-common-prefix (string1 string2)
    (let ((comparison (compare-strings string1 0 nil string2 0 nil))
          index)
        (if (eq comparison t)
            string1
            (if (< comparison 0)
               (setq index (- -1 comparison))
               (setq index (- comparison 1)))
            (substring string1 0 index))))


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

(defmacro save-point-position-with-scroll (&rest body)
    `(let ((--save-point-- (point-position-with-scroll)))
         (unwind-protect
             (progn
                 ,@body)
             (jump-to-position-with-scroll --save-point--))))

(defun point-line-and-column-with-scroll ()
    (list
        (line-number-at-pos)
        (current-column)
        (window-start)
        (window-vscroll nil t)
        (window-hscroll)))

(defun jump-to-line-and-column-with-scroll (line-and-column-with-scroll)
    (let-unpack ((line column start vscroll hscroll)
                     line-and-column-with-scroll)
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column column)
        (let ((window (selected-window)))
            (set-window-start window start t)
            (set-window-vscroll window vscroll t)
            (set-window-hscroll window hscroll))))

(defmacro save-point-line-and-column-with-scroll (&rest body)
    `(let ((--save-point-- (point-line-and-column-with-scroll)))
         (unwind-protect
             (progn
                 ,@body)
             (jump-to-line-and-column-with-scroll --save-point--))))

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

(defmacro save-point-marker-with-scroll (&rest body)
    `(let ((--save-point-- (point-marker-with-scroll)))
         (unwind-protect
             (progn
                 ,@body)
             (jump-to-marker-with-scroll --save-point--))))


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
                    (run-post-command-hook))
                (setq this-command binding)
                (when run-hooks
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


(defun next-string-split-separator
        (position separators &optional omit-nulls object limit)
    (when omit-nulls
        (setq separators (concat "\\(" separators "\\)+")))
    (if (stringp object)
        (let ((string (substring object position limit)))
            (if (string-match separators string)
                (if (> (match-beginning 0) 0)
                    (+ position (match-beginning 0))
                    (setq position (+ position (match-end 0)))
                    (setq string (substring string (match-end 0)))
                    (if (string-match separators string)
                        (+ position (match-beginning 0))
                        limit))
                limit))
        (save-excursion
            (when object
                (set-buffer object))
            (goto-char position)
            (if (search-forward-regexp separators limit 'x)
                (if (> (match-beginning 0) position)
                    (match-beginning 0)
                    (if (search-forward-regexp separators limit 'x)
                        (match-beginning 0)
                        limit))
                limit))))


(defun text-property-values (start end property &optional object)
    (unless start
        (if (stringp object)
            (setq start 0)
            (setq start 1)))
    (let ((values ()))
        (while (and start (or (not end) (< start end)))
            (let ((value (get-text-property
                             start property object)))
                (when value
                    (push value values)))
            (setq start (next-single-property-change
                            start property object end)))
        (nreverse values)))


(defun front-sticky-p (property &optional position object)
    (setq-if-nil position (point))
    (let ((sticky (get-text-property position 'front-sticky object)))
        (or (eq sticky t)
            (memq property sticky))))

(defun rear-nonsticky-p (property &optional position object)
    (setq-if-nil position (point))
    (if (or (and (stringp object)
                 (= position 0))
            (= position 1))
        t
        (let ((nonsticky (get-text-property (1- position) 'rear-nonsticky)))
            (or (eq nonsticky t)
                (memq property nonsticky)))))


(defun field-bounds (&optional position)
    (let ((beginning (field-beginning position))
          (end       (field-end       position)))
        (let ((field-1 (get-char-property beginning 'field))
              (field-2 (get-char-property (1- end)  'field)))
            (if (eq field-1 field-2)
                (cons beginning end)
                (setq-if-nil position (point))
                (if (rear-nonsticky-p 'field position)
                    (if (front-sticky-p 'field position)
                        (cons position end)
                        (cons position position))
                    (cons beginning position))))))

(defun fixed-field-at-position (&optional position)
    (let ((beginning (field-beginning position))
          (end       (field-end       position)))
        (let ((field-1 (get-char-property beginning 'field))
              (field-2 (get-char-property (1- end)  'field)))
            (if (eq field-1 field-2)
                field-1
                (if (rear-nonsticky-p 'field position)
                    (if (front-sticky-p 'field position)
                        field-2
                        nil)
                    field-1)))))

(defun fixed-field-string (&optional position)
    (let-uncons (beginning end (field-bounds position))
        (buffer-substring beginning end)))
(advice-add 'field-string :override 'fixed-field-string)

(defun fixed-field-string-no-properties (&optional position)
    (let-uncons (beginning end (field-bounds position))
        (buffer-substring-no-properties beginning end)))
(advice-add 'field-string-no-properties
    :override 'fixed-field-string-no-properties)

(defun fixed-field-delete (&optional position)
    (let-uncons (beginning end (field-bounds position))
        (delete-region beginning end)))
(advice-add 'delete-field :override 'fixed-field-delete)

(defun replace-field (new-contents &optional position)
    (delete-field position)
    (if position
        (save-excursion
            (goto-char position)
            (insert new-contents))
        (insert new-contents)))


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


(defun call-process-string (string program &optional buffer display &rest args)
    (apply 'call-process-region string nil program nil buffer display args))


(defun funcall-process (program &rest arguments)
    (let ((stdin))
        (when-let (stdin-cell (memq :stdin arguments))
            (setq stdin (cadr stdin-cell))
            (setcdr stdin-cell (cddr stdin-cell))
            (setq arguments (delq :stdin arguments)))
        (with-temp-buffer
            (let* ((result (apply 'call-process-string
                               stdin program t nil arguments))
                   (output (string-remove-suffix "\n" (buffer-string))))
                (list result output)))))

(defmacro apply-process (&rest command)
    `(apply 'funcall-process ,@command))


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


(defun buffer-name-idempotent (buffer-or-name)
    (if (bufferp buffer-or-name)
        (buffer-name buffer-or-name)
        buffer-or-name))


(defun buffer-file-or-directory (&optional buffer-or-name)
    (save-current-buffer
        (when buffer-or-name
            (set-buffer buffer-or-name))
        (or buffer-file-name dired-directory)))


(defun minibuffer-current-buffer ()
    (if-let (window (minibuffer-selected-window))
        (window-buffer window)
        nil))


(defun selected-window-excluding-minibuffers ()
    (if-let (window (minibuffer-selected-window))
        window
        (selected-window)))

(defun current-buffer-excluding-minibuffers ()
    (window-buffer (minibuffer-selected-window)))


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


(defun prompt-with-default (prompt &optional default)
    (if default
        (format-prompt
            (string-remove-suffix ":"
                (string-remove-suffix " " prompt))
            default)
        prompt))


(defun read-other-buffer (prompt)
    (with-advice (('confirm-nonexistent-file-or-buffer :override 'always))
        (read-buffer-to-switch prompt)))

(defun complete-buffer (&optional exclude include)
    (unless (listp exclude)
        (setq exclude (list exclude)))
    (setq exclude (mapcar 'get-buffer exclude))
    (unless (listp include)
        (setq include (list include)))
    (setq include (mapcar 'get-buffer include))
    (completion-table-dynamic
        (lambda-let (exclude include) (string)
            (let ((buffers (buffer-list))
                  (hidden-p (lambda (buffer)
                                (string-prefix-p " " (buffer-name buffer)))))
                (setq buffers (seq-difference buffers exclude))
                (if (equal (char-after (car (crm--current-element))) ? )
                    (setq buffers (seq-filter hidden-p buffers))
                    (setq buffers (seq-remove hidden-p buffers)))
                (setq buffers (append include buffers))
                (mapcar 'buffer-name buffers)))))

(defun read-buffer-multiple
        (prompt &optional except default require-match predicate)
    (when (bufferp default)
        (setq default (buffer-name default)))
    (let* ((history ())
           (choices (completing-read-multiple
                        prompt
                        (if except
                            (complete-buffer except default)
                            (complete-buffer nil default))
                        predicate require-match nil 'history default)))
        (prog1
            choices
            (while choices
                (push (pop choices) buffer-name-history)))))


(defun smoother-kill-buffer (&optional buffer)
    (unless buffer
        (setq buffer (current-buffer)))
    (set-buffer buffer)
    (add-hook 'kill-buffer-hook
        (lambda ()
            (let ((buffer (current-buffer)))
                (dolist (window (get-buffer-window-list buffer nil t))
                    (quit-window nil window)
                    (set-buffer buffer))))
            nil t)
    (kill-buffer buffer))

(defun smoother-kill-buffers (buffers)
    (interactive (list
                     (let ((current (current-buffer-excluding-minibuffers)))
                         (read-buffer-multiple
                             "Kill buffers: " nil current t))))
    (let ((count 0))
        (dolist (buffer buffers)
            (when (smoother-kill-buffer buffer)
                (setq count (1+ count))))
        (if (equal count 1)
            (message "Killed 1 buffer")
            (message "Killed %d buffers" count))))


(defun smoother-rename-buffer (unique)
    (interactive "P")
    (let* ((old-name (buffer-name))
           (new-name (read-string "Buffer name: " old-name)))
        (rename-buffer new-name unique)))


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
(define-key y-or-n-p-map "н" "n")
(define-key y-or-n-p-map "д" "y")
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


(defun read-event-from-minibuffer
        (&optional prompt inherit-input-method &rest _)
    (setq-if-nil prompt (or (current-message) ""))
    (let ((map (make-sparse-keymap)))
        (define-key map [t] 'exit-minibuffer)
        (with-nested-command-state
            (read-from-minibuffer
                prompt nil map nil t nil inherit-input-method))
        last-command-event))


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


(defun recursive-exit (&optional count function)
    (setq-if-nil function 'exit-recursive-edit)
    (let ((depth (recursion-depth)))
        (setq-if-nil count depth)
        (setq count (1- (min depth count)))
        (when (> count 0)
            (throw 'exit (apply-partially 'recursive-exit count function)))
        (funcall function)))

(defun recursive-abort (&optional count)
    (recursive-exit count 'abort-recursive-edit))


(defun hack-save-buffers-kill-emacs (save-buffers-kill-emacs &rest arguments)
    (with-advice (('save-some-buffers :override 'ignore))
        (apply save-buffers-kill-emacs arguments)))
(advice-add 'save-buffers-kill-emacs :around 'hack-save-buffers-kill-emacs)


(defun fixed-start-of-paragraph-text (&optional include-whole-line)
    (interactive "P")
    (let ((point (point)))
        (unwind-protect
            (progn
                (backward-paragraph)
                (skip-chars-forward " \t\n" point)
                (when include-whole-line
                    (skip-chars-backward " \t"))
                (when (save-excursion
                          (forward-paragraph)
                          (skip-chars-backward " \t\n")
                          (when include-whole-line
                              (skip-chars-forward " \t"))
                          (>= (point) point))
                    (setq point (point))))
            (goto-char point))
        point))

(defun fixed-end-of-paragraph-text (&optional include-whole-line)
    (interactive "P")
    (let ((point (point)))
        (unwind-protect
            (progn
                (forward-paragraph)
                (skip-chars-backward " \t\n" point)
                (when include-whole-line
                    (skip-chars-forward " \t"))
                (when (save-excursion
                          (backward-paragraph)
                          (skip-chars-forward " \t\n")
                          (when include-whole-line
                              (skip-chars-backward " \t"))
                          (<= (point) point))
                    (setq point (point))))
            (goto-char point))
        point))

(defun count-lines-paragraph (&optional include-whole-line)
    (save-excursion
        (count-lines
            (fixed-start-of-paragraph-text include-whole-line)
            (fixed-end-of-paragraph-text include-whole-line))))

(defun in-paragraph-p (&optional include-whole-line)
    (save-excursion
        (< (fixed-start-of-paragraph-text include-whole-line)
           (fixed-end-of-paragraph-text include-whole-line))))


(defvar minimum-fill-column 40)

(defun smoother-fill-paragraph (prefix-argument)
    (interactive "P")
    (when (in-paragraph-p t)
        (let* ((fill-column (if prefix-argument
                                (prefix-numeric-value prefix-argument)
                                fill-column))
               (_ (fill-paragraph))
               (best-fill-column fill-column)
               (metrics (smoother-fill-paragraph--metrics fill-column))
               (best-jaggedness (caddr metrics))
               (best-height     (cadr metrics))
               (best-width      (car metrics))
               (maximum-lines (1+ best-height))
               (minimum-fill-column (max minimum-fill-column
                                         (* fill-column 0.5)))
               (fill-column (1- best-width)))
            (while (and (>= fill-column minimum-fill-column)
                        (prog1 t (fill-paragraph))
                        (<= (count-lines-paragraph) maximum-lines))
                (setq metrics (smoother-fill-paragraph--metrics fill-column))
                (let ((jaggedness (caddr metrics))
                      (height     (cadr metrics))
                      (width      (car metrics)))
                    (if (if (> height best-height)
                            (< (+ jaggedness 2) best-jaggedness)
                            (< jaggedness best-jaggedness))
                        (setq best-fill-column fill-column
                              best-jaggedness jaggedness
                              best-width width)
                        (when (and (= jaggedness best-jaggedness)
                                   (= height best-height)
                                   (< width best-width))
                            (setq best-fill-column fill-column
                                  best-width width)))
                    (setq fill-column (1- width))))
            (setq fill-column best-fill-column)
            (fill-paragraph))))

(defun smoother-fill-paragraph--metrics (&optional cutoff)
    (let* ((paragraph (save-excursion
                          (buffer-substring-no-properties
                              (fixed-start-of-paragraph-text t)
                              (fixed-end-of-paragraph-text t))))
           (lines (string-split paragraph "\n"))
           (height (length lines))
           (longest (length (car lines)))
           (shortest (length (pop lines)))
           (jaggedness 0))
        (if cutoff
            (setq longest (min longest cutoff)
                  shortest (min shortest cutoff))
            (setq cutoff most-positive-fixnum))
        (while lines
            (let* ((line (pop lines))
                   (length (length line)))
                (setq longest (max longest length))
                (setq longest (min longest cutoff))
                (setq shortest (min shortest length))
                (when lines
                    (setq jaggedness (max jaggedness (- longest shortest))))))
        (list longest height jaggedness)))

(defun smoother-fill-paragraph-post-command ()
    (with-undo-amalgamate
        (save-excursion
            (let* ((origin (point))
                   (end-with-whitespace (fixed-end-of-paragraph-text t))
                   (end-without-whitespace (progn
                                               (skip-chars-backward " \t")
                                               (point)))
                   (split (max origin end-without-whitespace))
                   (trailing-whitespace-before-point (buffer-substring
                                                      end-without-whitespace
                                                      split))
                   (trailing-whitespace-after-point (buffer-substring
                                                     split
                                                     end-with-whitespace)))
                (smoother-fill-paragraph)
                (insert-before-markers trailing-whitespace-before-point)
                (insert trailing-whitespace-after-point)))))

(define-key global-map "\M-q" 'smoother-fill-paragraph)

(setq sentence-end-double-space nil)


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

(use-package generator
    :config
    (defun fixed-cps-generate-evaluator (generator)
        `(record 'iter ,generator))
    (advice-add 'cps-generate-evaluator
        :filter-return 'fixed-cps-generate-evaluator)
    (defun iter-p (object)
        (eq (type-of object) 'iter))
    (defun fixed-iter-method (arguments)
        (let ((iterator (car arguments)))
            (when (iter-p iterator)
                (setcar arguments (aref iterator 1)))
            arguments))
    (advice-add 'iter-next :filter-args 'fixed-iter-method)
    (advice-add 'iter-close :filter-args 'fixed-iter-method)
    (defun try-iter-next (iterator &optional yield-result)
        (ignore-error 'iter-end-of-sequence
            (iter-next iterator yield-result))))

(defvar-local parent-buffer--list nil)
(defun parent-buffer (&optional buffer)
    (save-current-buffer
        (when buffer
            (set-buffer buffer))
        (seq-find 'buffer-live-p parent-buffer--list)))
(defun set-parent-buffer (new-parent-buffer &optional buffer)
    (save-current-buffer
        (when buffer
            (set-buffer buffer))
        (unless (eq (car parent-buffer--list) new-parent-buffer)
            (setq parent-buffer--list
                (cons
                    new-parent-buffer
                    (buffer-local-value
                        'parent-buffer--list
                        new-parent-buffer))))))
(defun parent-buffer-setter (&optional new-parent-buffer)
    (setq-if-nil new-parent-buffer (current-buffer))
    (apply-partially 'set-parent-buffer new-parent-buffer))
(defun parent-buffer-search (predicate &optional buffer)
    (save-current-buffer
        (when buffer
            (set-buffer buffer))
        (seq-find
            (lambda-let (predicate) (buffer)
                (and (buffer-live-p buffer)
                     (funcall predicate buffer)))
            parent-buffer--list)))
(defun parent-buffer-file-or-directory (&optional buffer)
    (when-let (found (parent-buffer-search 'buffer-file-or-directory buffer))
        (buffer-file-or-directory found)))
(provide 'parent-buffer)

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
    (setq find-function-C-source-directory
        (expand-file-name
            (format "%s/emacs-%s/src/"
                user-emacs-directory
                emacs-version)))
    (define-key help-map "t" 'describe-face)
    (define-key help-map "g" nil)
    (define-key help-map "r" nil)
    (define-key help-map "h" nil)
    (defun fixed-help-view-source (&rest _)
        (set-window-start (selected-window) (point)))
    (advice-add 'help-function-def--button-function
        :after 'fixed-help-view-source)
    (define-key help-mode-map "\C-m" 'help-view-source)
    (defun fixed-package--print-email-button
            (package--print-email-button recipient)
        (if (consp (car-safe recipient))
            (while recipient
                (funcall package--print-email-button (pop recipient))
                (when recipient
                    (insert "             ")))
            (funcall package--print-email-button recipient)))
    (advice-add 'package--print-email-button
        :around 'fixed-package--print-email-button)
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
    (let ((table (make-sorted-hash-table 'string-greaterp :test 'equal))
          (calls (make-hash-table :test 'equal)))
        (vector table calls nil nil)))
(defun histdir-history-table (history)
    (aref history 0))
(defun histdir-history-calls (history)
    (aref history 1))
(defun histdir-history-buffers (history)
    (aref history 2))
(defun histdir-history--set-buffers (history buffers)
    (aset history 2 buffers))
(defun histdir-history--register-buffer (history buffer)
    (let ((buffers (histdir-history-buffers history)))
        (unless (memq buffer buffers)
            (histdir-history--set-buffers history
                (cons buffer buffers)))))
(defun histdir-history-watch (history)
    (aref history 3))
(defun histdir-history--set-watch (history descriptor)
    (aset history 3 descriptor))
(defvar-local histdir-buffer-local-history-list nil)
(defvar-local histdir-buffer-local-history--head nil)
(defun histdir--update-buffer-local-history-pointers-1 (shared)
    (if-let (local histdir-buffer-local-history--head)
        (progn
            (dlist-setcdr local shared)
            (setq histdir-buffer-local-history-list (dlist-list local)))
        (setq histdir-buffer-local-history-list (dlist-list shared))))
(defun histdir--update-buffer-local-history-pointers (history)
    (let ((shared  (aref (histdir-history-table history) 3))
          (buffers (histdir-history-buffers history)))
        (dolist (buffer buffers)
            (if (buffer-live-p buffer)
                (with-current-buffer buffer
                    (histdir--update-buffer-local-history-pointers-1 shared))
                (setq buffers (delq buffer buffers))))
        (histdir-history--set-buffers history buffers)))
(defun histdir--hash (entry)
    (secure-hash 'sha256 (concat entry "\n")))
(defun histdir-history--add-call (history hash datetime)
    (let* ((calls (histdir-history-calls history))
           (datetimes (gethash hash calls)))
        (unless (member datetime datetimes)
            (push datetime datetimes)
            (puthash hash datetimes calls))))
(defun histdir-history--remove-call (history hash datetime)
    (let* ((calls (histdir-history-calls history))
           (datetimes (gethash hash calls)))
        (if-let (datetimes (delete datetime datetimes))
            (puthash hash datetimes calls)
            (remhash hash calls))))
(defun histdir-history--add (history datetime hash string)
    (let ((table (histdir-history-table history)))
        (when-let (replaced-string (sorted-hash-table-get table datetime))
            (unless (equal replaced-string string)
                (let ((replaced-hash (histdir--hash replaced-string)))
                    (histdir-history--remove-call
                        history replaced-hash datetime))))
        (sorted-hash-table-put table datetime string)
        (histdir-history--add-call history hash datetime)
        (histdir--update-buffer-local-history-pointers history)))
(defun histdir-history--remove (history datetime)
    (let ((table (histdir-history-table history)))
        (when-let (removed (bihash-get (aref table 2) datetime))
            (let* ((string (sorted-hash-table-pop table datetime))
                   (hash   (histdir--hash string)))
                (histdir-history--remove-call history hash datetime))
            (dlist-setcar removed nil)
            (histdir--update-buffer-local-history-pointers history))))
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
(defconst histdir--datetime-regex "^[0-9]\\{8\\}T[0-9]\\{6\\},[0-9]\\{9\\}Z$")
(defun histdir--datetime-from-path (path)
    (let ((file (file-name-nondirectory path)))
        (when (string-match-p histdir--datetime-regex file)
            file)))
(defconst histdir--hash-regex "^[0-9a-f]\\{64\\}$")
(defun histdir--hash-from-path (path)
    (let ((file (file-name-nondirectory path)))
        (when (string-match-p histdir--hash-regex file)
            file)))
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
                (histdir--read-call (concat "call/" file) history)))))
(defun histdir--read-call (call-file history)
    (when-let ((datetime (histdir--datetime-from-path call-file))
               (hash     (histdir--read-file call-file))
               (_        (length> hash 0)))
        (thread-yield)
        (let* ((call-directory   (file-name-directory call-file))
               (parent-directory (file-name-parent-directory call-directory))
               (string-file      (concat parent-directory "string/" hash))
               (string           (histdir--read-file string-file)))
            (if (length> string 0)
                (histdir-history--add history datetime hash string)
                (histdir-history--add-call history hash datetime)))))
(defun histdir--read-string (string-file history)
    (when-let ((hash   (histdir--hash-from-path string-file))
               (string (histdir--read-file string-file))
               (_      (length> string 0)))
        (dolist (datetime (gethash hash (histdir-history-calls history)))
            (histdir-history--add history datetime hash string))))
(defun histdir--see-call-change (history watch-event)
    (let-unpack ((_descriptor action file) watch-event)
        (when (memq action '(deleted renamed))
            (when-let (datetime (histdir--datetime-from-path file))
                (histdir-history--remove history datetime)))
        (when (eq action 'renamed)
            (setq file (nth 3 watch-event)))
        (when (memq action '(created changed renamed))
            (with-temp-buffer
                (histdir--read-call file history)))))
(defun histdir--see-string-change (history watch-event)
    (let-unpack ((_descriptor action file) watch-event)
        (when (eq action 'renamed)
            (setq file (nth 3 watch-event)))
        (when (memq action '(created changed renamed))
            (with-temp-buffer
                (histdir--read-string file history)))))
(defun histdir--watch (path history)
    (let ((directory (concat path "/v1")))
        (list
            (file-notify-add-watch (concat directory "/string") '(change)
                (apply-partially 'histdir--see-string-change history))
            (file-notify-add-watch (concat directory "/call") '(change)
                (apply-partially 'histdir--see-call-change history)))))
(defvar histdir)
(make-variable-buffer-local 'histdir)
(defconst histdir--histories (make-hash-table :test 'equal))
(defun histdir--history ()
    (gethash (expand-file-name histdir) histdir--histories))
(defun histdir--init (path)
    (make-directory (concat path "/v1/string") t)
    (make-directory (concat path "/v1/call") t))
(defun histdir-watch+read (histdir)
    (require 'filenotify)
    (let* ((path       (expand-file-name histdir))
           (history    (gethash path histdir--histories))
           (first-read (not history)))
        (when first-read
            (histdir--init path)
            (setq history (make-histdir-history))
            (puthash path history histdir--histories))
        (histdir-history--register-buffer history (current-buffer))
        (histdir--update-buffer-local-history-pointers history)
        (unless (histdir-history-watch history)
            (let ((descriptor (histdir--watch path history)))
                (histdir-history--set-watch history descriptor)))
        (when first-read
            (make-thread (apply-partially 'histdir--read path history)))))
(defun histdir-add (entry &optional deduplicate)
    (let ((hash     (histdir--hash entry))
          (datetime (format-time-string "%Y%m%dT%H%M%S,%NZ" nil t))
          (path     (expand-file-name histdir)))
        (let ((string-directory (concat path "/v1/string/"))
              (call-directory   (concat path "/v1/call/")))
            (make-directory string-directory t)
            (make-directory call-directory t)
            (let ((string-file (concat string-directory hash))
                  (call-file   (concat call-directory datetime)))
                (with-temp-file string-file
                    (insert entry "\n"))
                (with-temp-file call-file
                    (insert hash "\n")))
            (when deduplicate
                (let* ((history (gethash path histdir--histories))
                       (calls (histdir-history-calls history))
                       (other-datetimes (gethash hash calls)))
                    (dolist (other-datetime (remove datetime other-datetimes))
                        (let ((file (concat call-directory other-datetime)))
                            (delete-file file))))))))
(defun histdir-remove (entry)
    (let* ((hash (histdir--hash entry))
           (path (expand-file-name histdir))
           (string-directory (concat path "/v1/string/"))
           (call-directory   (concat path "/v1/call/"))
           (history          (gethash path histdir--histories))
           (calls (histdir-history-calls history))
           (datetimes (gethash hash calls))
           (string-file (concat string-directory hash)))
        (delete-file string-file)
        (dolist (datetime datetimes)
            (let ((call-file (concat call-directory   datetime)))
                (delete-file call-file)))))
(defun histdir-change (old-entry new-entry)
    (let* ((new-hash (histdir--hash new-entry))
           (old-hash (histdir--hash old-entry))
           (path (expand-file-name histdir))
           (string-directory (concat path "/v1/string/"))
           (call-directory   (concat path "/v1/call/"))
           (history          (gethash path histdir--histories))
           (calls (histdir-history-calls history))
           (datetimes (gethash old-hash calls))
           (new-string-file (concat string-directory new-hash))
           (old-string-file (concat string-directory old-hash)))
        (with-temp-file new-string-file
            (insert new-entry "\n"))
        (dolist (datetime datetimes)
            (let ((call-file (concat call-directory   datetime)))
                (with-temp-file call-file
                    (insert new-hash "\n"))))
        (delete-file old-string-file)))
(defvar-local histdir-buffer-local-history--position nil)
(defun histdir-input-add (input &optional deduplicate)
    (setq histdir-buffer-local-history--position nil)
    (unless (equal (string-trim input) "")
        (if histdir-buffer-local-history--head
            (dlist-setcar histdir-buffer-local-history--head input)
            (setq histdir-buffer-local-history--head (dlist input)))
        (histdir-add input deduplicate)))
(defun histdir-input-remove (entry)
    (when (equal entry (dlist-car histdir-buffer-local-history--head))
        (setq histdir-buffer-local-history--head nil))
    (histdir-remove entry))
(defun histdir-input-change (old-entry new-entry)
    (when (equal old-entry (dlist-car histdir-buffer-local-history--head))
        (dlist-setcar histdir-buffer-local-history--head new-entry))
    (histdir-change old-entry new-entry))
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
        (if-let ((shared (aref (histdir-history-table (histdir--history)) 3))
                 (local  histdir-buffer-local-history--head)
                 (_      (not (equal (dlist-car local) (dlist-car shared)))))
            local
            shared)))
(defun histdir-input--older (position)
    (if-let (older (histdir-input--cycle-older position))
        older
        position))
(defun histdir-input--newer (position)
    (let ((newer (dlist-cpr position)))
        (while (and newer (not (dlist-car newer)))
            (setq newer (dlist-cpr newer)))
        newer))
(defun histdir-input--cycle-newer (position)
    (let ((history (histdir--history)))
        (if position
            (histdir-input--newer position)
            (aref (histdir-history-table history) 3))))
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
    (defun eshell-quote-argument-2 (string)
        (let ((eshell-special-chars-outside-quoting
                  (cons ?$ eshell-special-chars-inside-quoting)))
            (concat
                "\""
                (eshell-quote-argument string)
                "\"")))
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
    (defvar latest-eshell nil)
    (defun latest-eshell--add (eshell &rest arguments)
        (let ((buffer (apply eshell arguments)))
            (add-hook 'kill-buffer-hook 'latest-eshell--remove nil t)
            (setq latest-eshell (cons buffer (delete buffer latest-eshell)))
            buffer))
    (defun latest-eshell--remove ()
        (setq latest-eshell (delete (current-buffer) latest-eshell)))
    (advice-add 'eshell :around 'latest-eshell--add)
    (defun latest-eshell (&optional prefix-argument)
        (interactive "P")
        (if prefix-argument
            (if (equal prefix-argument 1)
                (eshell)
                (eshell prefix-argument))
            (if latest-eshell
                (pop-to-buffer (car latest-eshell))
                (eshell))))
    (setq eshell-history-size 0)
    (advice-add 'eshell-hist-initialize
        :before
        (lambda (&rest _)
            (setq histdir "~/.history/eshell")))
    (advice-add 'eshell-read-history
        :override
        (lambda (&rest _)
            (histdir-watch+read histdir)))
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
    (defun foreground-process-in-eshell-p ()
        (let ((found-foreground-process nil)
              (processes eshell-process-list))
            (while (and (not found-foreground-process)
                        processes)
                (let ((entry (pop processes)))
                    (setq found-foregroung-process (not (cdr entry)))))
            found-foreground-process))
    (defun fixed-eshell-interrupt-process (&rest _)
        (unless (foreground-process-in-eshell-p)
            (save-excursion
                (goto-char (point-max))
                (insert
                    (propertize "\n"
                        'field 'end-of-input
                        'rear-nonsticky t)))))
    (advice-add 'eshell-interrupt-process
        :before 'fixed-eshell-interrupt-process)
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

(defun smoother-eval-last-sexp (prefix-argument)
    (interactive "P")
    (condition-case error
        (scan-sexps (1+ (point)) -1)
        (:success
            (eval-last-sexp prefix-argument))
        (scan-error
            (when (and (equal (caddr error) (point))
                       (equal (cadddr error) (point))
                       (equal (char-after (point)) ?\())
                (save-excursion
                    (forward-sexp)
                    (backward-char)
                    (eval-last-sexp prefix-argument))))))
(add-to-list 'temporary-goal-column-preserving-commands
    'smoother-eval-last-sexp)

(use-package dired
    :config
    (defun hack-dired-readin (dired-readin &rest arguments)
        (combine-change-calls 1 (1+ (buffer-size))
            (let ((cell (cons nil nil))
                  result)
                (with-hook (('dired-before-readin-hook
                                (lambda-let (cell) ()
                                    (setcar cell buffer-undo-list))
                                99 t))
                    (with-advice (('erase-buffer
                                      :before
                                      (lambda-let (cell) (&rest _)
                                          (setq buffer-undo-list (car cell))))
                                  ('dired-readin-insert
                                      :after
                                      (lambda-let (cell) (&rest _)
                                          (setcar cell buffer-undo-list))))
                        (setq result (apply dired-readin arguments))))
                (setq buffer-undo-list (car cell))
                result)))
    (advice-add 'dired-readin :around 'hack-dired-readin)
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
        (let ((last-point -1))
            (until (or (dired-get-filename nil t)
                       (= last-point (point)))
                (setq last-point (point))
                (dired-next-line 1)))
        (dired-move-to-filename))
    (defun dired-goto-last-file ()
        (interactive)
        (goto-char (point-max))
        (dired-previous-line 1))
    (defun dired-filename-search-forward (bound)
        (let ((start-of-search (point)))
            (while (and (< (pos-eol) (point-max))
                        (or (eolp)
                            (not (dired-get-filename nil t))))
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
    (defun add-dired-file (path &optional marker force predicate)
        (dired-fun-in-all-buffers
            (file-name-directory path)
            (file-name-nondirectory path)
            'add-dired-entry path marker nil force predicate))
    (defun add-dired-entry (path &optional marker relative force predicate)
        (setq-if-nil predicate 'string-lessp)
        (setq path (directory-file-name path))
        (when (or force
                  (/= (aref (file-name-nondirectory path) 0) ?.)
                  (dired-check-switches dired-actual-switches "[aA]"))
            (save-excursion
                (unless (dired-goto-file path)
                    (dired-goto-first-file)
                    (while-let ((next-path (dired-get-filename nil t))
                                (_ (funcall predicate next-path path)))
                        (dired-next-line 1))
                    (dired-add-entry path marker relative)))))
    (defun update-dired-file (path &optional old-path)
        (when (and old-path
                   (not (equal (file-name-directory path)
                               (file-name-directory old-path))))
            (dired-fun-in-all-buffers
                (file-name-directory old-path)
                (file-name-nondirectory old-path)
                'update-dired-entry path old-path))
        (dired-fun-in-all-buffers
            (file-name-directory path)
            (file-name-nondirectory path)
            'update-dired-entry path old-path))
    (defun update-dired-entry (path &optional old-path)
        (setq-if-nil old-path path)
        (save-excursion
            (if (not (dired-goto-file old-path))
                (dired-add-entry (directory-file-name path))
                (beginning-of-line)
                (let ((marker-character (following-char)))
                    (when (eq marker-character ? )
                        (setq marker-character nil))
                    (let ((buffer-read-only nil))
	                (delete-region
                            (point)
                            (line-beginning-position 2)))
                    (dired-add-entry
                        (directory-file-name path)
                        marker-character)))))
    (defun fixed-dired-rename-file
            (dired-rename-file path new-path &rest arguments)
        (with-advice (('dired-remove-file
                          :override
                          (lambda-let (new-path) (path)
                              (update-dired-file new-path path))))
            (apply dired-rename-file path new-path arguments)))
    (advice-add 'dired-rename-file :around 'fixed-dired-rename-file)
    (defun fixed-dired-copy-file (from to &rest arguments)
        (when (directory-name-p to)
            (setq from (directory-file-name from))
            (let ((name (file-name-nondirectory from)))
                (setq to (concat to name))))
        (dired-add-file (expand-file-name to)))
    (advice-add 'dired-copy-file :after 'fixed-dired-copy-file)
    (defun fixed-dired-delete-file (path &rest _)
        (setq path (directory-file-name path))
        (dired-remove-file (expand-file-name path)))
    (advice-add 'dired-delete-file :after 'fixed-dired-delete-file)
    (defun fixed-dired-mode (dired-mode &rest arguments)
        (let ((mode-line-buffer-identification))
            (apply dired-mode arguments)))
    (advice-add 'dired-mode :around 'fixed-dired-mode)
    (defun fixed-dired-save-positions (positions)
        (let* ((buffer-position (car positions))
               (line-cell (cddr buffer-position)))
            (when (eobp)
                (setcar line-cell nil))
            (let ((column (current-column))
                  (lines-from-bottom (- (count-visual-lines
                                            (point)
                                            (window-end nil t)))))
                (setcdr line-cell (list column lines-from-bottom))))
        positions)
    (advice-add 'dired-save-positions
        :filter-return 'fixed-dired-save-positions)
    (defun fixed-dired-restore-positions (dired-restore-positions positions)
        (let* ((buffer-position (car positions))
               (buffer (nth 0 buffer-position))
               (line   (nth 2 buffer-position))
               (column (nth 3 buffer-position)))
            (unless line
                (setcar (nthcdr 2 buffer-position) 1))
            (funcall dired-restore-positions positions)
            (if (not line)
                (goto-char (point-max))
                (move-to-column column)
                (when (eq (window-buffer) buffer)
                    (let ((lines-from-bottom (nth 4 buffer-position)))
                        (if (>= (- lines-from-bottom) (window-screen-lines))
                            (recenter 0)
                            (recenter lines-from-bottom)))
                    (scroll-to-fill-window)
                    (unless line
                        (goto-char (point-max)))))))
    (advice-add 'dired-restore-positions
        :around 'fixed-dired-restore-positions)
    (defun pulsed-dired-revert--view ()
        (list (count-visual-lines
                  (window-start)
                  (save-excursion
                      (beginning-of-visual-line)))
              (window-hscroll)
              (dired-get-filename t t)))
    (defvar pulsed-dired-revert--nested nil)
    (defmacro with-pulsed-dired-revert (&rest body)
        (let ((before (make-symbol "pulsed-dired-revert--before"))
              (after  (make-symbol "pulsed-dired-revert--after")))
            `(let ((,before (unless pulsed-dired-revert--nested
                                (pulsed-dired-revert--view))))
                 (prog1
                     (let ((pulsed-dired-revert--nested t))
                         ,@body)
                     (unless pulsed-dired-revert--nested
                         (let ((,after (pulsed-dired-revert--view)))
                             (unless (equal ,before ,after)
                                 (pulse-momentary-highlight-region
                                     (pos-bol) (pos-eol)))))))))
    (defun pulsed-dired-revert--advice (dired-revert &rest arguments)
        (with-pulsed-dired-revert
            (apply dired-revert arguments)))
    (advice-add 'dired-revert :around 'pulsed-dired-revert--advice)
    (defvar-local independent-dired-buffer nil)
    (defun independent-dired--filter-p (cell)
        (let ((buffer (cdr cell)))
            (not (buffer-local-value 'independent-dired-buffer buffer))))
    (defun independent-dired--filter (dired-buffers)
        (seq-filter 'independent-dired--filter-p dired-buffers))
    (defun independent-dired--find-buffer-nocreate
            (dired-find-buffer-nocreate &rest arguments)
        (let ((dired-buffers (independent-dired--filter dired-buffers)))
            (apply dired-find-buffer-nocreate arguments)))
    (advice-add 'dired-find-buffer-nocreate
        :around 'independent-dired--find-buffer-nocreate)
    (defun independent-dired (directory &optional switches)
        (let (buffer new)
            (let ((dired-buffers nil))
                (setq buffer (dired directory switches))
                (with-current-buffer buffer
                    (setq independent-dired-buffer t))
                (setq new (pop dired-buffers)))
            (push new dired-buffers)
            buffer))
    (defun reuse-independent-dired (name directory &optional switches revert)
        (let ((buffer (get-buffer name)))
            (if buffer
                (progn
                    (pop-to-buffer-same-window buffer)
                    (when revert
                        (revert-buffer)))
                (setq buffer (independent-dired directory switches))
                (rename-buffer name))
            buffer)))

(use-packages dired parent-buffer
    :config
    (defun smoother-dired ()
        (interactive)
        (let ((path (or dired-directory
                        (get-text-property (point) 'full-path)
                        buffer-file-name
                        (parent-buffer-file-or-directory))))
            (if (not path)
                (dired default-directory)
                (dired (file-name-parent-directory path))
                (setq path (expand-file-name path))
                (add-dired-entry path nil nil t)
                (dired-goto-file path)
                (recenter)
                (scroll-to-fill-window)
                (pulse-momentary-highlight-region
                    (pos-bol) (pos-eol))))))

(add-to-list 'text-property-default-nonsticky
    (cons 'full-path t))
(defun full-path-property-split (start end &optional object separators trim)
    (setq-if-nil separators "\n")
    (if (stringp object)
        (setq object (substring object start end))
        (setq-if-nil start (point-min))
        (setq-if-nil end   (point-max))
        (setq object (filter-buffer-substring start end)))
    (let ((parts (split-string object separators t trim))
          (result ()))
        (dolist (part parts)
            (if-let (paths (text-property-values nil nil 'full-path part))
                (while-let ((path (pop paths)))
                    (unless (eq path (car result))
                        (push path result)))
                (push part result)))
        (nreverse result)))
(defun full-path-property--eshell-ls (fileinfo)
    (if (stringp fileinfo)
        (propertize fileinfo 'full-path (expand-file-name fileinfo))
        (let ((file (pop fileinfo)))
            (cons
                (propertize file 'full-path (expand-file-name file))
                fileinfo))))
(advice-add 'eshell-ls-annotate :filter-return 'full-path-property--eshell-ls)
(defun full-path-property--dired ()
    (save-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t))
            (while (< (point) (point-max))
                (when-let (path (dired-get-filename nil t))
                    (put-text-property
                        (line-beginning-position)
                        (line-end-position)
                        'full-path path))
                (forward-line 1)))))
(add-hook 'dired-after-readin-hook 'full-path-property--dired)
(provide 'full-path-property)

(defun android-trash-p (path)
    (setq path (directory-file-name path))
    (let ((name (file-name-nondirectory path)))
        (if (string-match-p "^\\.trashed-[0-9]+-.*$" name)
            t
            nil)))
(defun android-trash--trashed-name (path)
    (let* ((name    (file-name-nondirectory path))
           (now     (time-convert (current-time) 'integer))
           (30-days (* 30 24 60 60))
           (expires (+ now 30-days))
           (trashed (format ".trashed-%d-%s" expires name)))
        trashed))
(defun android-trash--trashed-path (path)
    (let* ((trashed   (android-trash--trashed-name path))
           (directory (file-name-directory path)))
        (concat directory trashed)))
(defun android-trash (path)
    (if (android-trash-p path)
        path
        (setq path (directory-file-name path))
        (setq path (expand-file-name path))
        (let ((trashed-path (android-trash--trashed-path path)))
            (dired-rename-file path trashed-path nil)
            trashed-path)))
(defun android-trash--original-name (trashed-path)
    (let ((trashed-name (file-name-nondirectory trashed-path)))
        (string-match "^\\.trashed-[0-9]+-\\(.*\\)$" trashed-name)
        (match-string 1 trashed-name)))
(defun android-trash--restored-path (trashed-path &optional directory)
    (setq-if-nil directory (file-name-directory trashed-path))
    (let ((name (android-trash--original-name trashed-path)))
        (concat directory name)))
(defun android-trash--find-trash (path)
    (let* ((name      (file-name-nondirectory path))
           (directory (file-name-directory path))
           (trashed (concat
                        ".trashed-[0-9]+-"
                        (regexp-quote name)
                        "$")))
        (setq-if-nil directory default-directory)
        (directory-files directory t trashed)))
(define-error 'android-trash-missing "Trash is missing" 'file-missing)
(defun android-trash-restore (path &optional to-path ok-if-already-exists)
    (setq path (directory-file-name path))
    (setq path (expand-file-name path))
    (when to-path
        (setq to-path (expand-file-name to-path)))
    (unless (android-trash-p path)
        (setq path (car (last (android-trash--find-trash path))))
        (unless path
            (signal 'android-trash-missing (list path))))
    (when (or (not to-path)
              (directory-name-p to-path))
        (setq to-path (android-trash--restored-path path to-path)))
    (dired-rename-file path to-path ok-if-already-exists)
    to-path)
(provide 'android-trash)

(when termux
    (use-package image-mode
        :config
        (defun hack-image-mode ()
            (add-single-use-hook 'post-command-hook 'kill-buffer)
            (browse-url-xdg-open (buffer-file-name)))
        (advice-add 'image-mode :override 'hack-image-mode)))

(defun git--read-link (path)
    (condition-case _error
        (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-max))
            (when (equal (char-before) ?\n)
                (delete-char -1))
            (buffer-string))
        (file-missing)))
(defun git--not-in-repository-error-p (error-output)
    (string-prefix-p "fatal: not a git repository" error-output))
(defun git--not-in-worktree-error-p (error-output)
    (equal error-output "fatal: this operation must be run in a work tree"))
(defconst git--worktree-root '("git" "rev-parse" "--show-toplevel"))
(defconst git--git-dir '("git" "rev-parse" "--absolute-git-dir"))
(defun git-worktree-root ()
    (seq-let (status output) (apply-process git--worktree-root)
        (if (equal status 0)
            (abbreviate-file-name output)
            (when (git--not-in-worktree-error-p output)
                (seq-setq (status output) (apply-process git--git-dir)))
            (if (equal status 0)
                (let* ((backlink (concat output "/gitdir"))
                       (worktree-.git-file (git--read-link backlink)))
                    (abbreviate-file-name
                        (directory-file-name
                            (file-name-directory
                                (if worktree-.git-file
                                    worktree-.git-file
                                    output)))))
                (if (git--not-in-repository-error-p output)
                    nil
                    (error "git rev-parse error: (%S) %s" status output))))))
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
                (git-worktree-root))))
    (advice-add 'vc-git-root :override 'fixed-vc-git-root)
    (add-hook 'vc-annotate-mode-hook
        (lambda ()
            (setq truncate-lines nil)))
    (defun hack-vc-dir (&rest _)
        (error "vc-dir called"))
    (advice-add 'vc-dir :override 'hack-vc-dir))

(use-packages vc parent-buffer
    :config
    (add-hook 'vc-annotate-mode-hook
        (lambda ()
            (set-parent-buffer vc-parent-buffer))))

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
    (setq-default fill-column 79)
    (defun control-fill-column (prefix-argument)
        (interactive "P")
        (let ((overlong-regexp (format ".\\{%d\\}\\(.*\\)" fill-column)))
            (unhighlight-regexp overlong-regexp))
        (if prefix-argument
            (if (integerp prefix-argument)
                (if (< prefix-argument 1)
                    (display-fill-column-indicator-mode -1)
                    (setq fill-column prefix-argument)
                    (display-fill-column-indicator-mode 1))
                (display-fill-column-indicator-mode 1))
            (when (eq last-command 'control-fill-column)
                (display-fill-column-indicator-mode 'toggle)))
        (let ((overlong-regexp (format ".\\{%d\\}\\(.*\\)" fill-column)))
            (when display-fill-column-indicator-mode
                (highlight-regexp overlong-regexp 'hi-yellow 1)))
        (message "fill-column: %S display: %s"
            fill-column
            display-fill-column-indicator-mode)))

(use-package gnutls
    :config
    (setq gnutls-verify-error t))

(use-package crm
    :config
    (defun hide-chosen-crm-completions--predicate (candidate)
        (let* ((input (minibuffer-contents-no-properties))
               (already-chosen (butlast (split-string input crm-separator))))
            (when (consp candidate)
                (setq candidate (car candidate)))
            (not (member candidate already-chosen))))
    (defun hide-chosen-crm-completions ()
        (setq crm-completion-table
              (apply-partially
                  'completion-table-with-predicate
                  crm-completion-table
                  'hide-chosen-crm-completions--predicate
                  t))))

(use-package package
    :config
    (add-hook 'package-menu-mode-hook 'visual-line-mode)
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
        (setq-if-nil count 1)
        (dotimes (_ count)
            (hexl-nibble-insert-1 nibble)))
    (defun fixed-hexl-self-insert-command (count &optional character)
        (interactive "p")
        (setq-if-nil character last-command-event)
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

(use-package debug
    :config
    (defvar fixed-debug--parent-depths nil)
    (defun fixed-debug--setup ()
        (setq fixed-debug--parent-depths nil)
        (pop kill-buffer-hook)
        (add-hook 'kill-buffer-query-functions 'fixed-debug--kill nil t))
    (add-hook 'debugger-mode-hook 'fixed-debug--setup)
    (defun fixed-debug--kill ()
        (if (not fixed-debug--parent-depths)
            t
            (when (y-or-n-p "Debugged execution paused; kill anyway?")
                (add-single-use-hook 'post-command-hook
                    (apply-partially 'kill-buffer (current-buffer)))
                (add-single-use-hook 'post-command-hook 'debug-top-level))
            nil))
    (defun debug-top-level ()
        (interactive)
        (while (cdr fixed-debug--parent-depths)
            (pop fixed-debug--parent-depths))
        (debug-parent-level))
    (setq debug-allow-recursive-debug t)
    (defun debug-parent-level ()
        (interactive)
        (fixed-debug--collect-garbage)
        (when-let ((depth (pop fixed-debug--parent-depths)))
            (let ((count (- (recursion-depth) depth)))
                (when (> count 0)
                    (recursive-abort count)))))
    (defun fixed-debug--collect-garbage ()
        (while-let ((parent-depth (car fixed-debug--parent-depths))
                    (_            (>= parent-depth (recursion-depth))))
            (pop fixed-debug--parent-depths)))
    (defun fixed-debugger-setup-buffer (&rest _)
        (setq inhibit-debugger (not debug-allow-recursive-debug))
        (push (recursion-depth) fixed-debug--parent-depths))
    (advice-add 'debugger-setup-buffer :before 'fixed-debugger-setup-buffer)
    (defun fixed-debugger-quit ()
        (when (fixed-debug--in-debugger-p)
            (fixed-debug--exit-checklist)
            (abort-recursive-edit))
        (quit-window))
    (advice-add 'debugger-quit :override 'fixed-debugger-quit)
    (defun fixed-debug--in-debugger-p ()
        (fixed-debug--collect-garbage)
        (when-let ((depth (car fixed-debug--parent-depths)))
            (> (recursion-depth) depth)))
    (defun fixed-debug--exit-checklist (&rest _)
        (fixed-debug--collect-garbage)
        (if-let ((parent-depth (car fixed-debug--parent-depths)))
            (let ((debugger-depth (1+ parent-depth)))
                (when (> (recursion-depth) debugger-depth)
                    (user-error "Not in most nested command loop")))
            (user-error "No debug session in progress"))
            (pop fixed-debug--parent-depths))
    (dolist (function '(debugger-jump
                        debugger-return-value
                        debugger-continue
                        debugger-step-through))
        (advice-add function :before 'fixed-debug--exit-checklist))
    (defun smoother-debugger-exit ()
        (interactive)
        (if (fixed-debug--in-debugger-p)
            (debugger-continue)
            (quit-window)))
    (define-key debugger-mode-map "q" 'smoother-debugger-exit))

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
          (words (split-string string " "))
          word)
        (setq word words)
        (while word
            (when (length> (car word) 0)
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
                    (setq integers-in-span 0)))
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
        (setq string (string-join words " "))
        (if (or (and (< (- (length (remove "" words)) integers-before-span) 2)
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
    (when-let (overlay (car cell))
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
    (let-unpack ((previous-parsed bindings _string) preview-info)
        (concat
            (datetime-read--preview-format-1
                'year  parsed previous-parsed nil)
            (datetime-read--preview-format-1
                'month parsed previous-parsed bindings)
            (datetime-read--preview-format-1
                'day   parsed previous-parsed bindings)
            (when-let (final-value (decoded-time-weekday parsed))
                (let* ((names '(Sun Mon Tue Wed Thu Fri Sat))
                       (name  (nth final-value names))
                       (prior-value (decoded-time-weekday previous-parsed)))
                    (if (and prior-value (not (= prior-value final-value)))
                        (format " (%s->%s)" (nth prior-value names) name)
                        (format " (%s)" name))))
            (datetime-read--preview-format-1
                'hour   parsed previous-parsed bindings)
            (datetime-read--preview-format-1
                'minute parsed previous-parsed bindings)
            (datetime-read--preview-format-1
                'second parsed previous-parsed bindings))))

(defun datetime-read--preview-format-1 (slot parsed previous-parsed bindings)
    (let* ((format-string (if (eq slot 'year) "%04d" "%02d"))
           (name (symbol-name slot))
           (getter (intern (concat "decoded-time-" name)))
           (face (intern (concat "datetime-read-preview-" name "-face")))
           (final-value (funcall getter parsed))
           (prior-value (funcall getter previous-parsed))
           (bound-value (funcall getter bindings))
           (prefix      (cond ((memq slot '(second minute)) ":")
                              ((eq   slot 'hour)            " ")
                              ((memq slot '(day month))     "-")
                              ((eq   slot 'year)            ""))))
        (when final-value
            (if (or (and prior-value
                         (not (= prior-value final-value)))
                    (and (numberp bound-value)
                         (not (eq bound-value final-value))))
                (format (concat prefix
                                "{"
                                (if (eq bound-value t)
                                    (propertize format-string 'face face)
                                    format-string)
                                "->"
                                (if (eq bound-value final-value)
                                    (propertize format-string 'face face)
                                    format-string)
                                "}")
                        (or prior-value bound-value) final-value)
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
        (minibuffer-with-setup-hook
            (lambda-let (cell) ()
                (setcar cell (make-overlay (point-max) (point-max) nil t t))
                (add-hook 'post-command-hook
                    (lambda-let (short now cell) ()
                        (datetime-read--preview short now cell))
                    nil t))
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

(provide 'datetime)


(use-package undo-tree
    :config
    (setq undo-tree-auto-save-history nil)
    (add-to-list 'undo-tree-incompatible-major-modes
        'undo-tree-visualizer-mode)
    (add-hook 'undo-tree-visualizer-mode-hook
        (lambda ()
            (set-window-dedicated-p (selected-window) nil)
            (setq mode-name "Undo")))
    (defun undo-tree-visualizer-quit ()
        (interactive)
        (undo-tree-clear-visualizer-data buffer-undo-tree)
        (when undo-tree-visualizer-parent-buffer
            (with-current-buffer undo-tree-visualizer-parent-buffer
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
        (undo-tree-visualizer-selection-mode -1))
    (defun independent-undo-tree-name ()
        (concat " *undo-tree (" (buffer-name) ")*"))
    (defun independent-undo-tree ()
        (interactive)
        (setq-local undo-tree-visualizer-buffer-name
            (independent-undo-tree-name))
        (call-interactively 'undo-tree-visualize)))

(use-packages undo-tree parent-buffer
    :config
    (add-hook 'undo-tree-visualizer-mode-hook
        (lambda ()
            (set-parent-buffer undo-tree-visualizer-parent-buffer))))

(use-package vertico
    :config
    (require 'display-line-numbers)
    (vertico-mode 1)
    (setq vertico-resize nil)
    (defun count-vertico-lines ()
        (save-excursion
            (goto-char (point-max))
            (save-mutation
                (insert (overlay-get vertico--candidates-ov 'after-string))
                (count-visual-lines (point-min) (point-max)))))
    (defvar-local fixed-vertico-resize--state nil)
    (defun fixed-vertico-resize--before (&rest _)
        (setq fixed-vertico-resize--state
            (cons visual-line-mode truncate-lines)))
    (defun fixed-vertico-resize (&rest _)
        (when (car fixed-vertico-resize--state)
            (visual-line-mode 1))
        (setq truncate-lines (cdr fixed-vertico-resize--state))
        (unless truncate-lines
            (set-window-hscroll nil 0))
        (let* ((desired-height (count-vertico-lines))
               (delta (- desired-height (window-height))))
            (window-resize nil delta)))
    (advice-add 'vertico--resize :before 'fixed-vertico-resize--before)
    (advice-add 'vertico--resize :after 'fixed-vertico-resize)
    (defconst fixed-vertico--multiline-wrap
        (propertize "\n↳" 'face 'vertico-multiline))
    (defun fixed-vertico--truncate-multiline
            (vertico--truncate-multiline string &rest arguments)
        (if truncate-lines
            (apply vertico--truncate-multiline string arguments)
            (string-replace "\n" fixed-vertico--multiline-wrap string)))
    (advice-add 'vertico--truncate-multiline
        :around 'fixed-vertico--truncate-multiline)
    (setq vertico-cycle t)
    (setq minibuffer-prompt-properties
        (append '(cursor-intangible t) minibuffer-prompt-properties))
    (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
    (setq vertico-scroll-margin 0)
    (set-face-foreground 'vertico-multiline "#FF0000")
    (defvar-local next-vertico-index nil)
    (defvar-local next-vertico-scroll nil)
    (defun hack-vertico--recompute (state)
        (when next-vertico-index
            (setf (alist-get 'vertico--index state) next-vertico-index)
            (setq next-vertico-index nil))
        (when next-vertico-scroll
            (setf (alist-get 'vertico--scroll state) next-vertico-scroll)
            (setq next-vertico-scroll nil))
        state)
    (advice-add 'vertico--recompute :filter-return 'hack-vertico--recompute))

(use-packages crm vertico
    :config
    (setq crm-separator (propertize "\x36E1CA"
                            'display (propertize "," 'face 'escape-glyph)))
    (defun insert-vertico-candidates-for-crm ()
        (interactive)
        (goto-char (point-max))
        (search-backward crm-separator (minibuffer-prompt-end) 'x)
        (delete-region (point) (point-max))
        (unless (= (point) (minibuffer-prompt-end))
            (insert crm-separator))
        (insert (string-join vertico--candidates crm-separator))
        (insert crm-separator))
    (defun insert-vertico-candidate-for-crm ()
        (interactive)
        (let* ((bounds (crm--current-element))
               (start  (car bounds))
               (end    (cdr bounds))
               (input  (buffer-substring-no-properties start end))
               (count  (length vertico--candidates)))
            (setq next-vertico-index (max 0 (min vertico--index (- count 2))))
            (setq next-vertico-scroll vertico--scroll)
            (vertico-insert)
            (goto-char (minibuffer-prompt-end))
            (while (search-forward crm-separator nil 'x)
                (delete-char -1)
                (insert crm-separator))
            (insert crm-separator input)
            (unless (> count 1)
                (delete-region
                    (save-excursion
                        (1+ (search-backward crm-separator nil t)))
                    (point)))))
    (defun hack-completing-read-multiple
            (completing-read-multiple &rest arguments)
        (minibuffer-with-setup-hook
            (lambda ()
                (hide-chosen-crm-completions)
                (let ((map (make-sparse-keymap)))
                    (set-keymap-parent map (current-local-map))
                    (use-local-map map))
                (local-set-key "\t" 'insert-vertico-candidates-for-crm)
                (local-set-key "," 'insert-vertico-candidate-for-crm)
                (local-set-key "\M-,"
                    (lambda ()
                        (interactive)
                        (insert crm-separator)))
                (local-set-key "\C-m"
                    (lambda ()
                        (interactive)
                        (if (equal #x36E1CA (char-before (point)))
                            (progn
                                (delete-char -1)
                                (vertico--update)
                                (vertico-exit-input))
                            (vertico-exit)))))
            (apply completing-read-multiple arguments)))
    (advice-add 'completing-read-multiple
        :around 'hack-completing-read-multiple))

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
                (dlist-link (dlist-last new) new)
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
                (setq stepper 'dlist-cpr))
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
    (defun fixed-consult-completion-in-region
            (start end table &optional predicate)
        (let* ((string     (buffer-substring-no-properties start end))
               (point      (- (point) start))
               (candidates (completion-all-completions
                               string table predicate point)))
            (if (length= candidates 1)
                (completion--in-region start end table predicate)
                (let ((completion-reference-buffer (current-buffer)))
                    (consult-completion-in-region start end table predicate)))))
    (setq completion-in-region-function 'fixed-consult-completion-in-region)
    (add-to-list 'consult-mode-histories
        '(eshell-mode
          histdir-buffer-local-history-list
          nil
          eshell-bol))
    (setq consult-find-args "find .")
    (setq consult-project-function (lambda (_may-prompt) (git-worktree-root)))
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
                (let ((case-fold-search ignore-case)
                      (completion-ignore-case ignore-case))
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
    (when wsl
        (defun hack-eat-term-resize (arguments)
            (let ((width (cadr arguments)))
                (setcar (cdr arguments) (1- width)))
            arguments)
        (advice-add 'eat-term-resize :filter-args 'hack-eat-term-resize))
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
(defun pop-to-command-buffer-name (command &optional context name)
    (setq-if-nil name (string-join command " "))
    (if context
        (concat "*" name " (" context ")*")
        (concat "*" name "*")))
(defun pop-to-command-eshell (command &optional context name callback)
    (require 'eshell)
    (setq name (pop-to-command-buffer-name command context name))
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
    (require 'eat)
    (setq name (pop-to-command-buffer-name command context name))
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
    (setq evil-respect-visual-line-mode t)
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
    (defun evil-motion-post-command ()
        (with-advice (('evil-normal-state-p :override 'evil-motion-state-p))
            (evil-normal-post-command)))
    (add-hook 'evil-motion-state-entry-hook
        (lambda ()
            (add-hook 'post-command-hook 'evil-motion-post-command nil t)))
    (add-hook 'evil-motion-state-exit-hook
        (lambda ()
            (remove-hook 'post-command-hook 'evil-motion-post-command t)))
    (defvar-local evil-yank-incomplete-line-linewise t)
    (defun fixed-evil-yank-lines (evil-yank-lines start end &rest arguments)
        (if evil-yank-incomplete-line-linewise
            (apply evil-yank-lines start end arguments)
            (let ((string (filter-buffer-substring start end)))
                (if (or (string-match-p "\n" string)
                        (equal (char-before start) ?\n))
                    (apply evil-yank-lines start end arguments)
                    (apply 'evil-yank-characters start end arguments)))))
    (advice-add 'evil-yank-lines :around 'fixed-evil-yank-lines)
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
    (define-key evil-motion-state-map "ga" 'what-cursor-position)
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
            (let ((end (point-max)))
                (evil-ensure-column
                    (if (equal (char-before end) ?\n)
                        (goto-char (1- end))
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
    (defun fixed-evil-looking-at-start-comment
            (evil-looking-at-start-comment &rest arguments)
        (when (= (point) (point-max))
            (backward-char))
        (apply evil-looking-at-start-comment arguments))
    (advice-add 'evil-looking-at-start-comment
        :around 'fixed-evil-looking-at-start-comment)
    (setq evil-shift-round nil)
    (define-key evil-replace-state-map [escape] 'evil-insert-state)
    (define-key evil-motion-state-map "\C-m" nil)
    (evil-define-motion fixed-evil-next-visual-line (count)
        :type exclusive
        (interactive "p")
        (evil-signal-without-movement
            (fixed-line-move-visual count)))
    (advice-add 'evil-next-visual-line
        :override 'fixed-evil-next-visual-line)
    (evil-define-motion fixed-evil-previous-visual-line (count)
        :type exclusive
        (interactive "p")
        (evil-signal-without-movement
            (fixed-line-move-visual (- count))))
    (advice-add 'evil-previous-visual-line
        :override 'fixed-evil-previous-visual-line)
    (defun fixed-newline ()
        (interactive)
        (when (memq evil-state '(insert replace emacs))
            (become-command 'newline)))
    (evil-declare-not-repeat 'fixed-newline)
    (define-key global-map "\C-m" 'fixed-newline)
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
    (add-to-list 'temporary-goal-column-preserving-commands 'evil-repeat)
    (defun fixed-evil-repeat (evil-repeat &rest arguments)
        (evil-save-state
            (apply evil-repeat arguments)))
    (advice-add 'evil-repeat :around 'fixed-evil-repeat)
    (defun fixed-evil-execute-repeat-info-with-count (&rest arguments)
        (pop kill-buffer-hook))
    (advice-add 'evil-execute-repeat-info-with-count
        :before 'fixed-evil-execute-repeat-info-with-count)
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
    (defun evil-find-char-pair-1 (direction char1 char2)
        (let ((point (point))
              (evil-last-find))
            (condition-case _error
                (while (progn
                           (evil-find-char direction char1)
                           (not (equal (char-after (+ (point) 1)) char2))))
                (user-error
                    (goto-char point)
                    (user-error "Can't find %c%c" char1 char2)))))
    (evil-define-motion evil-find-char-pair (count char1 char2)
        :type inclusive
        (interactive "p<C><C>")
        (let ((direction 1))
            (if (< count 0)
                (dotimes (_ (- count))
                    (evil-find-char-pair-1 -1 char1 char2))
                (dotimes (_ count)
                    (evil-find-char-pair-1 1 char1 char2)))
            (when (> count 0)
                (forward-char))))
    (evil-define-motion evil-find-char-pair-to (count char1 char2)
        :type inclusive
        (interactive "p<C><C>")
        (evil-find-char-pair count char1 char2)
        (when (> count 0)
            (backward-char 2)))
    (evil-define-motion evil-find-char-pair-backward (count char1 char2)
        :type inclusive
        (interactive "p<C><C>")
        (when (and (equal (char-before (point)) char1)
                   (equal (char-after (point)) char2))
            (backward-char))
        (evil-find-char-pair (- count) char1 char2))
    (evil-define-motion evil-find-char-pair-to-backward (count char1 char2)
        :type exclusive
        (interactive "p<C><C>")
        (evil-find-char-pair (- count) char1 char2)
        (forward-char 2))
    (dolist (map (list evil-motion-state-map evil-normal-state-map))
        (define-key map "gf" 'evil-find-char-pair)
        (define-key map "gF" 'evil-find-char-pair-backward)
        (define-key map "gt" 'evil-find-char-pair-to)
        (define-key map "gT" 'evil-find-char-pair-to-backward))
    (when termux
        (define-key evil-motion-state-map [down-mouse-1] nil))
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
                 (define-key ,map "-" ',negative)
                 '(,universal ,digit ,negative))))
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
    (define-key space-map "e" 'smoother-eval-last-sexp)
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
    (define-key space-map "B" 'smoother-rename-buffer)
    (define-key space-map "k" 'smoother-kill-buffers)
    (define-key space-map "f" 'find-file)
    (define-key space-map "F" 'find-alternate-file)
    (define-key space-map "d" 'smoother-dired)
    (add-to-list 'evil-motion-state-modes 'dired-mode)
    (define-key space-map "y" 'execute-extended-command)
    (define-key space-map "," 'eval-expression)
    (define-key space-map "h" help-map)
    (evil-define-key 'motion help-mode-map "H" 'help-go-back)
    (evil-define-key 'motion help-mode-map "L" 'help-go-forward)
    (define-key space-map "P" 'list-packages)
    (add-to-list 'evil-motion-state-modes 'package-menu-mode)
    (defun add-evil-repeat (function)
        (evil-repeat-start)
        (add-to-list 'evil-repeat-info `(,function))
        (evil-repeat-stop))
    (defvar history-quit nil)
    (defun history--require-match-p (_)
        (or history-quit
            (>= vertico--index 0)))
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
            (add-evil-repeat
                (lambda-let (history-quit command run) ()
                    (end-of-buffer)
                    (replace-command command)
                    (unless history-quit
                        (funcall run))))
            (unless history-quit
                (funcall run))))
    (defun history-remove (prefix-argument)
        (interactive "P")
        (let* ((history (car (consult--current-history)))
               (vertico-sort-function 'identity)
               (choices (completing-read-multiple
                            "Remove history: " history nil t))
               (count (length choices)))
            (end-of-buffer)
            (dolist (entry choices)
                (replace-command entry)
                (evil-end-undo-step)
                (if histdir
                    (histdir-input-remove entry)
                    (when (ring-p history)
                        (while-let ((index (ring-member history entry)))
                            (ring-remove history index))))
                (evil-start-undo-step)
                (delete-command))
            (if (= count 1)
                (message "Removed 1 history entry")
                (message "Removed %d history entries" count))))
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
                    (histdir-input-change old-entry new-entry)
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
    (setq truncate-partial-width-windows nil)
    (defun cycle-line-wrap ()
        (interactive)
        (if truncate-lines
            (progn
                (setq truncate-lines nil)
                (set-window-hscroll nil 0))
            (visual-line-mode 'toggle)
            (when (not visual-line-mode)
                (setq truncate-lines t)))
        (message "visual-line-mode: %s truncate-lines: %s"
            visual-line-mode truncate-lines))
    (evil-declare-not-repeat 'cycle-line-wrap)
    (define-key space-map ";" 'cycle-line-wrap)
    (define-key space-map "[" 'delete-trailing-whitespace)
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
    (define-universal-argument-space-keys space-search-map " s")
    (define-key space-search-map "l" 'consult-ripgrep)
    (define-key space-search-map "f" 'consult-fd)
    (define-key space-search-map "t"
        (lambda ()
            (interactive)
            (consult-fd "~/storage/shared" "\\.trashed -- -H")))
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
        (when (not buffer-file-name)
            (when-let (buffer (parent-buffer-search 'buffer-file-name))
                (set-buffer buffer)))
        (add-single-use-hook 'pop-to-command-setup-hook
            (parent-buffer-setter))
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
    (defun diff-buffer--pick (prompt)
        (if-let (window (next-window-other-buffer nil 'never))
            (window-buffer window)
            (read-other-buffer prompt)))
    (defun diff-buffer (buffer-1 buffer-2)
        (interactive (list
                         (diff-buffer--pick "Diff from buffer: ")
                         (current-buffer)))
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
                (add-single-use-hook 'pop-to-command-setup-hook
                    (parent-buffer-setter))
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
        (when (not buffer-file-name)
            (when-let (buffer (parent-buffer-search 'buffer-file-name))
                (set-buffer buffer)))
        (add-single-use-hook 'pop-to-command-setup-hook
            (parent-buffer-setter))
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
    (defun partial-revert ()
        (interactive)
        (when (not buffer-file-name)
            (when-let (buffer (parent-buffer-search 'buffer-file-name))
                (set-buffer buffer)))
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
                    (add-single-use-hook 'pop-to-command-setup-hook
                        (parent-buffer-setter))
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
    (defvar partial-copy-from-current-to-target nil)
    (defun partial-copy (buffer-1 buffer-2)
        (interactive (if partial-copy-from-current-to-target
                         (list
                             (current-buffer)
                             (diff-buffer--pick "Copy to buffer: "))
                         (list
                             (diff-buffer--pick "Copy from buffer: ")
                             (current-buffer))))
        (setq buffer-1 (get-buffer buffer-1))
        (setq buffer-2 (get-buffer buffer-2))
        (add-single-use-hook 'pop-to-command-setup-hook
            (parent-buffer-setter))
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
    (define-key space-map "w"
        (lambda (prefix-argument)
            (interactive "P")
            (if prefix-argument
                (let ((partial-copy-from-current-to-target t))
                    (become-command 'partial-copy))
                (become-command 'partial-save))))
    (define-key space-map "W" 'save-buffer)
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
    (defun git-pop-to-command (command &optional use-visited)
        (add-single-use-hook 'pop-to-command-setup-hook
            (parent-buffer-setter))
        (if-let (root (git-worktree-root))
            (let ((default-directory root)
                  (path (git--target-path use-visited)))
                (when path
                    (setq path (file-relative-name path root))
                    (nconc command (list path)))
                (pop-to-command-eshell command default-directory))
            (pop-to-command-eshell--not-in-a-git-repository
                (string-join command " "))))
    (defun git--target-path (use-visited)
        (if use-visited
            (or (buffer-file-or-directory)
                (parent-buffer-file-or-directory)
                default-directory)
            nil))
    (defmacro git (&rest arguments)
        (let ((command (cons "git" (mapcar 'symbol-name arguments))))
            `(lambda (prefix-argument)
                 (interactive "P")
                 (let ((command (list ,@command)))
                     (git-pop-to-command command prefix-argument)))))
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
    (define-key git-map "f" (git stash list -p))
    (define-key git-map "d" (git diff))
    (define-key git-map "s" (git diff --staged))
    (define-key git-map "a" (git add -p))
    (defun git-add-new (prefix-argument)
        (interactive "P")
        (let ((command (list "git add")))
            (when prefix-argument
                (nconc command (list "--force")))
            (git-pop-to-command command t)))
    (define-key git-map "A" 'git-add-new)
    (define-key git-map "q" (git checkout -p))
    (define-key git-map "w" (git reset -p))
    (define-key git-map "e" (git stash push -p))
    (defun git-pop (prefix-argument)
        (interactive "P")
        (let ((command (list "git" "pop")))
            (when prefix-argument
                (nconc command (list (format "stash@{%d}" prefix-argument))))
            (git-pop-to-command command)))
    (define-key git-map "r" 'git-pop)
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
    (define-key git-map "h" 'git-reset)
    (defun git-annotate ()
        (interactive)
        (if (git-worktree-root)
            (if (or dired-directory buffer-file-name)
                (call-interactively 'vc-annotate)
                (if-let (buffer (parent-buffer-search 'buffer-file-name))
                    (with-current-buffer buffer
                        (call-interactively 'vc-annotate))
                    (pop-to-command-eshell--not-a-file "Annotate")))
            (pop-to-command-eshell--not-in-a-git-repository "Annotate")))
    (define-key git-map "b" 'git-annotate)
    (define-universal-argument-space-keys git-map " v")
    (define-key git-map [escape] 'ignore)
    (define-prefix-command 'space-misc-map)
    (define-key space-map "z" 'space-misc-map)
    (define-universal-argument-space-keys space-misc-map " z")
    (define-key space-misc-map "y"
        (lambda ()
            (interactive)
            (let ((default-directory "~/Downloads"))
                (pop-to-command-eshell
                    '("sh" "-c" "yt-dlp -f bestaudio --no-playlist \"`p`\"")
                    nil "yt-dlp"))))
    (define-key space-misc-map "m"
        (lambda ()
            (interactive)
            (let ((default-directory "~"))
                (pop-to-command-eshell
                    '("termux-media-scan" "/storage/emulated/0")))))
    (define-key space-misc-map "+" 'text-scale-increase)
    (define-key space-misc-map "=" 'text-scale-increase)
    (define-key space-misc-map "-" 'text-scale-decrease)
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
        (setq evil-yank-incomplete-line-linewise nil)
        (dolist (key '([down] "j" "о"))
            (evil-local-set-key 'normal key 'evil-vertico-next-line))
        (dolist (key '([up] "k" "л"))
            (evil-local-set-key 'normal key 'evil-vertico-previous-line))
        (let ((quit (cond
                        ((memq this-command '(history-execute history-change))
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
    (add-to-list 'evil-motion-state-modes 'messages-buffer-mode)
    (with-current-buffer (messages-buffer)
        (evil-motion-state)))

(use-packages calendar evil
    :config
    (evil-define-key 'motion calendar-mode-map "H" 'calendar-scroll-right)
    (evil-define-key 'motion calendar-mode-map "L" 'calendar-scroll-left)
    (toggle datetime-read-popup-calendar t))

(define-derived-mode shred-mode nil "shred")
(defvar shred-candidate nil)
(defvar shred--previous-candidate-content nil)
(defun shred--show-candidate ()
    (when shred-candidate
        (let ((content (evil-get-register shred-candidate t)))
            (unless (eq content shred--previous-candidate-content)
                (erase-buffer)
                (when content
                    (insert content)
                    (goto-char 1))
                (setq shred--previous-candidate-content content)))))
(defun shred-buffer-create ()
    (if-let (buffer (get-buffer " *shred*"))
        buffer
        (let ((buffer (get-buffer-create " *shred*")))
            (with-current-buffer buffer
                (shred-mode)
                (add-hook 'post-command-hook 'shred--show-candidate nil t))
            buffer)))
(defun shred-buffer ()
    (interactive)
    (pop-to-buffer-same-window (shred-buffer-create)))
(define-key shred-mode-map "q" 'quit-window)
(add-to-list 'evil-motion-state-modes 'shred-mode)
(defmacro shred-define-key (key def &rest bindings)
    `(evil-define-key 'motion shred-mode-map ,key ,def ,@bindings))
(defun shred--paste (register)
    (if (or (not register) (equal register ?\"))
        (setq register ?1))
    (evil-get-register register)
    (setq shred-candidate register))
(evil-define-command shred-paste-after (register)
    (interactive "<x>")
    :suppress-operator t
    (shred--paste register)
    (goto-char (point-max)))
(evil-define-command shred-paste-before (register)
    (interactive "<x>")
    :suppress-operator t
    (shred--paste register)
    (goto-char 1))
(shred-define-key "p" 'shred-paste-after)
(shred-define-key "P" 'shred-paste-before)
(shred-define-key "gp" 'shred-paste-after)
(shred-define-key "gP" 'shred-paste-before)
(defun shred-run-handler (string)
    (when-let ((shred-handler (get-text-property 0 'shred-handler string))
               (function (car-safe shred-handler))
               (_ (functionp function)))
        (let ((argument (car-safe (cdr shred-handler))))
            (funcall function (or argument string)))))
(defun shred-kill (n)
    (when-let ((link    (nthcdr (1- n) kill-ring))
               (_       (>= n 0))
               (removed (if (equal n 0)
                            kill-ring
                            (cdr link))))
        (shred-run-handler (car removed))
        (if (equal n 0)
            (pop kill-ring)
            (setcdr link (cdr removed)))
        (when (eq removed kill-ring-yank-pointer)
            (pop kill-ring-yank-pointer)
            (unless kill-ring-yank-pointer
                (setq kill-ring-yank-pointer kill-ring))))
    nil)
(defun shred (&optional register)
    (interactive (list shred-candidate))
    (if (or (not register) (equal register ?\"))
        (shred-kill 0)
        (if (<= ?1 register ?9)
            (shred-kill (- register ?1))
            (setq register (downcase register))
            (let ((string (get-register register)))
                (shred-run-handler string)
                (set-register register nil)))))
(define-key shred-mode-map "\C-m" 'shred)
(define-key space-map "x" 'shred-buffer)

(use-packages dired evil
    :config
    (setq dired-listing-switches "-l")
    (defun evil-dired-toggle-hidden ()
        (interactive)
        (let ((old-switches dired-actual-switches)
              new-switches)
            (if (not (dired-check-switches old-switches "[aA]"))
                (setq new-switches (concat old-switches "A"))
                (evil-save-column
                    (while-let ((name (dired-get-filename t t))
                                (_ (string-prefix-p "." name)))
                        (dired-next-line 1)))
                (setq new-switches (delete ?A (delete ?a old-switches))))
            (dired-sort-other new-switches)
            (message "ls %s" new-switches)))
    (evil-define-key 'motion dired-mode-map "H" 'evil-dired-toggle-hidden))

(use-packages dired android-trash evil
    :config
    (defvar evil-dired-delete-keep-entries nil)
    (evil-define-operator evil-dired-delete
            (start end type register yank-handler)
        :move-point nil
        :type line
        (interactive "<R><x><y>")
        (let ((paths (full-path-property-split start end))
              (lines 0)
              (line (line-number-at-pos))
              (column (current-column)))
            (dolist (path paths)
                (if (android-trash-p path)
                    (dired-delete-file path 'always)
                    (android-trash path)
                    (setq lines (1+ lines))))
            (goto-char start)
            (forward-line lines)
            (setq end (point))
            (if evil-dired-delete-keep-entries
                (let ((evil-was-yanked-without-register nil))
                    (evil-yank start end type register yank-handler))
                (let ((buffer-read-only nil))
                    (evil-delete start end type register yank-handler)))
            (goto-char (point-min))
            (forward-line (1- line))
            (move-to-column column)))
    (evil-define-key 'motion dired-mode-map "d" 'evil-dired-delete)
    (evil-define-operator evil-dired-delete-line (register yank-handler)
        :move-point nil
        :motion nil
        (interactive "<x><y>")
        (evil-dired-delete (pos-bol) (pos-eol) 'line register yank-handler))
    (evil-define-key 'motion dired-mode-map "D" 'evil-dired-delete-line)
    (defun evil-dired--paste-1 (path to-path)
        (if (android-trash-p path)
            (let ((new-path (android-trash-restore path to-path)))
                (dired-remove-entry new-path)
                (dired-add-entry new-path)
                t)
            (let ((dired-recursive-copies 'always))
                (dired-copy-file path to-path nil)
                nil)))
    (defun evil-dired--replacing-paste-1 (path _to-path)
        (let ((new-path (dired-get-filename)))
            (dired-delete-file new-path 'always)
            (evil-dired--paste-1 path new-path)))
    (defun evil-dired--paste (register &optional replace)
        (let* ((text (evil-paste-to-string 1 register))
               (paths (full-path-property-split nil nil text))
               (paste-1 (if replace
                            'evil-dired--replacing-paste-1
                            'evil-dired--paste-1))
               start end yank-handler
               (moved nil))
            (unless replace
                (forward-line))
            (setq start (pos-bol))
            (dolist (path paths)
                (when (funcall paste-1 path dired-directory)
                    (setq moved t))
                (forward-line))
            (when moved
                (setq end (pos-bol))
                (setq yank-handler (get-text-property 0 'yank-handler text))
                (setq text (filter-buffer-substring start end))
                (setq text (propertize text 'yank-handler yank-handler))
                (if register
                    (setq register (downcase register))
                    (setq register ?1))
                (evil-set-register register text)))
        (forward-line -1))
    (evil-define-command evil-dired-paste-after (count register)
        :suppress-operator t
        (interactive "p<x>")
        (evil-save-column
            (dotimes (_ count)
                (evil-dired--paste register))))
    (evil-define-key 'motion dired-mode-map "p" 'evil-dired-paste-after)
    (evil-define-command evil-dired-paste-before (count register)
        :suppress-operator t
        (interactive "p<x>")
        (evil-save-column
            (let ((line (line-number-at-pos)))
                (if (= line 1)
                    (setq line 2)
                    (forward-line -1))
                (dotimes (_ count)
                    (evil-dired--paste register))
                (goto-char (point-min))
                (forward-line (1- line)))))
    (evil-define-key 'motion dired-mode-map "P" 'evil-dired-paste-before)
    (evil-define-command evil-dired-replacing-paste-after (count register)
        :suppress-operator t
        (interactive "p<x>")
        (evil-save-column
            (dotimes (_ count)
                (evil-dired--paste register t))))
    (evil-define-key 'motion dired-mode-map "gp" 'evil-dired-replacing-paste-after)
    (evil-define-command evil-dired-replacing-paste-before (count register)
        :suppress-operator t
        (interactive "p<x>")
        (evil-save-column
            (let ((line (line-number-at-pos)))
                (dotimes (_ count)
                    (evil-dired--paste register t))
                (goto-char (point-min))
                (forward-line (1- line)))))
    (evil-define-key 'motion dired-mode-map "gP" 'evil-dired-replacing-paste-before)
    (evil-define-key 'motion dired-mode-map "." 'evil-repeat))

(use-packages eshell evil
    :config
    (defvar-local evil-eshell-state-for-next-input 'normal)
    (advice-add 'eshell-send-input
        :before
        (lambda (&rest _)
            (setq evil-eshell-state-for-next-input evil-state)))
    (defun evil-eshell-force-normal-state ()
        (interactive)
        (evil-force-normal-state)
        (setq evil-eshell-state-for-next-input evil-state))
    (defun evil-eshell-setup ()
        (setq evil-yank-incomplete-line-linewise nil)
        (evil-local-set-key 'normal [escape] 'eshell-interrupt-process)
        (evil-local-set-key 'insert [escape] 'evil-eshell-force-normal-state)
        (evil-local-set-key 'operator [escape] 'evil-force-normal-state)
        (evil-local-set-key 'insert "\C-d" 'eshell-send-eof-to-process))
    (add-hook 'eshell-mode-hook 'evil-eshell-setup)
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
    (add-to-list 'evil-normal-state-modes 'eshell-mode)
    (define-key space-map "t" 'latest-eshell))

(defun user-friendly-path (path)
    (let ((from-home (concat "~/" (file-relative-name path "~/"))))
        (if (> (length from-home) (length path))
            path
            from-home)))

(use-packages dired eshell evil
    :config
    (evil-define-operator evil-dired-eshell (start end)
        (interactive "<r>")
        (let* ((paths (full-path-property-split start end))
               (nicer (mapcar 'user-friendly-path paths))
               (quoted (mapcar 'eshell-quote-argument-2 nicer)))
            (eshell t)
            (when (in-eshell-scrollback-p)
                (end-of-buffer))
            (save-excursion
                (insert " " (string-join quoted " "))))
        (evil-insert-state))
    (evil-define-key 'motion dired-mode-map "x" 'evil-dired-eshell))

(use-packages dired evil undo-tree
    :config
    (add-hook 'dired-mode-hook 'undo-tree-mode)
    (defun evil-dired-undo ()
        (interactive)
        (let ((buffer-read-only nil))
            (become-command 'evil-undo)))
    (defun evil-dired-redo ()
        (interactive)
        (let ((buffer-read-only nil))
            (become-command 'evil-redo)))
    (evil-define-key 'motion dired-mode-map "u" 'evil-dired-undo)
    (evil-define-key 'motion dired-mode-map "U" 'evil-dired-redo))

(use-packages display-fill-column-indicator evil
    :config
    (evil-declare-not-repeat 'control-fill-column)
    (define-key space-map "|" 'control-fill-column))

(use-packages eshell eat evil
    :config
    (evil-define-command evil-eat-eshell-paste (count &optional register)
        :suppress-operator t
        (interactive "*P<x>")
        (let* ((string (evil-paste-to-string count register))
               (string (string-remove-suffix "\n" string)))
            (process-send-string nil string)))
    (dolist (key '("p" "P"))
        (evil-define-key 'normal eat-eshell-semi-char-mode-map
            key 'evil-eat-eshell-paste))
    (evil-define-command evil-eat-eshell-replacing-paste
            (count &optional register)
        (interactive "*P<x>")
        (user-error "replacing paste unsupported for child process"))
    (dolist (key '("gp" "gP"))
        (evil-define-key 'normal eat-eshell-semi-char-mode-map
            key 'evil-eat-eshell-replacing-paste))
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
    (define-key space-map "u" 'independent-undo-tree))

(use-packages evil parent-buffer pop-to-command undo-tree
    :config
    (add-to-list 'display-buffer-alist
        (list "^\\*undo-tree Diff .*\\*$"))
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
                    (buffer-name)
                    "undo-tree Diff"
                    (apply-partially 'delete-directory directory t))
                (setq directory nil)
                (let ((name (buffer-name)))
                    (with-current-buffer parent-buffer
                        (setq-local undo-tree-diff-buffer-name name)))
                (when-let (window (get-buffer-window parent-buffer))
                    (select-window window)))))
    (defun hack-undo-tree-visualizer-show-diff (&optional node)
        (setq undo-tree-visualizer-diff t)
        (hack-undo-tree-visualizer-update-diff node))
    (advice-add 'undo-tree-visualizer-show-diff
        :override 'hack-undo-tree-visualizer-show-diff)
    (defun hack-undo-tree-visualizer-update-diff (&optional node)
        (add-single-use-hook 'pop-to-command-setup-hook
            (parent-buffer-setter))
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
(defmacro histdir-repl-define-key (state key def &rest bindings)
    `(evil-define-key ,state histdir-repl-mode-map ,key ,def ,@bindings))
(histdir-repl-define-key 'normal "q" 'quit-window)
(histdir-repl-define-key 'normal "\C-m" 'histdir-repl-send-input)
(histdir-repl-define-key 'insert "\C-m" 'histdir-repl-send-input)
(histdir-repl-define-key 'normal "\C-c\C-c" 'histdir-repl-interrupt)
(histdir-repl-define-key 'insert "\C-c" 'histdir-repl-interrupt)
(histdir-repl-define-key 'normal [escape] 'histdir-repl-interrupt)
(histdir-repl-define-key 'normal "d" 'histdir-repl-evil-delete-input)
(histdir-repl-define-key 'normal "D" 'histdir-repl-evil-delete-input-line)
(histdir-repl-define-key 'normal "c" 'histdir-repl-evil-change-input)
(histdir-repl-define-key 'normal "C" 'histdir-repl-evil-change-input-line)
(histdir-repl-define-key 'normal "i" 'histdir-repl-insert)
(histdir-repl-define-key 'normal "I" 'histdir-repl-insert-at-beginning)
(histdir-repl-define-key 'normal "a" 'histdir-repl-append)
(histdir-repl-define-key 'normal "A" 'histdir-repl-append-at-end)
(histdir-repl-define-key 'normal "o" 'histdir-repl-append-at-end)
(histdir-repl-define-key 'normal "O" 'histdir-repl-insert-at-beginning)
(histdir-repl-define-key 'motion "0" 'histdir-repl-beginning-of-line)
(histdir-repl-define-key 'motion "$" 'histdir-repl-end-of-line)
(dolist (key '("x" [delete] [deletechar]))
    (histdir-repl-define-key 'normal key 'histdir-repl-delete-char))
(dolist (key '("X" [backspace] "\C-?"))
    (histdir-repl-define-key 'normal key 'histdir-repl-backspace-char))
(histdir-repl-define-key 'normal "r" 'histdir-repl-evil-replace)
(histdir-repl-define-key 'normal "R" 'histdir-repl-evil-replace-state)
(histdir-repl-define-key 'replace
    [remap eat-self-input] 'histdir-repl-self-input+replace)
(dolist (key '([backspace] "\C-?"))
    (histdir-repl-define-key 'replace key 'histdir-repl-evil-replace-backspace))
(dolist (key '([delete] [deletechar]))
    (histdir-repl-define-key 'replace key 'histdir-repl-evil-replace-delete))
(histdir-repl-define-key 'replace "\C-m" 'histdir-repl-evil-replace-send-input)
(histdir-repl-define-key 'replace "\C-c" 'histdir-repl-evil-replace-interrupt)
(histdir-repl-define-key 'replace "\t" 'histdir-repl-evil-replace-tab)
(histdir-repl-define-key 'normal "p" 'histdir-repl-evil-paste-after)
(histdir-repl-define-key 'normal "P" 'histdir-repl-evil-paste-before)
(histdir-repl-define-key 'normal "gp" 'histdir-repl-evil-replacing-paste-after)
(histdir-repl-define-key 'normal "gP" 'histdir-repl-evil-replacing-paste-before)
(histdir-repl-define-key 'insert "\C-q" 'eat-quoted-input)
(dolist (key '("\C-a" "\C-d" "\C-e" "\C-k" "\C-l" "\C-u" [delete] [deletechar]))
    (histdir-repl-define-key 'insert key 'eat-self-input))
(dolist (key '([\C-left] [\C-right]))
    (histdir-repl-define-key 'insert key 'eat-self-input)
    (histdir-repl-define-key 'replace key 'eat-self-input))
(histdir-repl-define-key 'insert [up] 'histdir-input-older)
(histdir-repl-define-key 'insert [down] 'histdir-input-newer)
(histdir-repl-define-key 'replace [up] 'histdir-repl-evil-replace-up)
(histdir-repl-define-key 'replace [down] 'histdir-repl-evil-replace-down)
(histdir-repl-define-key 'insert [mouse-1] 'histdir-repl-mouse-set-point)
(histdir-repl-define-key 'replace [down-mouse-1] 'histdir-repl-mouse-set-point)
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
(defun histdir-repl (command histdir &optional environment)
    (unless environment
        (setq environment process-environment))
    (let* ((program       (car command))
           (arguments     (cdr command))
           (buffer-locals (list default-directory histdir))
           (name          (concat "*" (string-join command " ") "*"))
           (buffer        (get-buffer name)))
        (unless buffer
            (setq buffer (get-buffer-create name))
            (set-buffer buffer)
            (histdir-repl-mode))
        (pop-to-buffer buffer)
        (setq buffer-undo-list t)
        (unpack (default-directory histdir) buffer-locals)
        (unless (get-buffer-process (current-buffer))
            (let ((process-environment environment))
                (eat-exec buffer name program nil arguments)))
        (evil-normal-state)
        (histdir-watch+read histdir)
        buffer))
(defun histdir-repl-beginning-of-input ()
    (interactive)
    (goto-char (eat-point))
    (process-send-string nil "\C-a")
    (sleep-for 0.04)
    (point))
(defun histdir-repl-end-of-input ()
    (interactive)
    (goto-char (eat-point))
    (process-send-string nil "\C-e")
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
    (process-send-string nil "x\C-a\C-k"))
(defun histdir-repl-replace-input (new-input &optional position)
    (interactive)
    (histdir-repl-delete-input)
    (process-send-string nil new-input))
(defun histdir-repl-send-input ()
    (interactive)
    (histdir-input-add (histdir-repl-get-input) t)
    (goto-char (eat-point))
    (process-send-string nil "\C-m"))
(defun histdir-repl-interrupt ()
    (interactive)
    (setq histdir-buffer-local-history--position nil)
    (goto-char (eat-point))
    (process-send-string nil "\C-c"))
(defun histdir-repl-forward-char-in-input (&optional count)
    (process-send-string nil (repeat-string "\e[C" (or count 1))))
(defun histdir-repl-backward-char-in-input (&optional count)
    (process-send-string nil (repeat-string "\e[D" (or count 1))))
(defun histdir-repl-backspace-char-in-input (&optional count)
    (process-send-string nil (repeat-string "\C-?" (or count 1))))
(defun histdir-repl-delete-char-in-input (&optional count)
    (process-send-string nil (repeat-string "\e[3~" (or count 1))))
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
    (process-send-string nil "\C-a")
    (evil-insert-state))
(defun histdir-repl-append ()
    (interactive)
    (histdir-repl-enter-input 1)
    (evil-insert-state))
(defun histdir-repl-append-at-end ()
    (interactive)
    (goto-char (eat-point))
    (process-send-string nil "\C-e")
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
        (process-send-string nil (make-string count character))
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
                (process-send-string nil "\C-d")
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
                        (process-send-string nil
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
    :move-point nil
    (interactive "<R><x>")
    (if (eq type 'line)
        (histdir-repl-evil-delete-input-line register)
        (let ((deleted (histdir-repl-enter+delete-input-range start end))
              (evil-was-yanked-without-register nil))
            (evil-yank-string deleted register))))
(evil-define-operator histdir-repl-evil-delete-input-line (&optional register)
    :move-point nil
    (interactive "<x>")
    (let ((input (histdir-repl-get-input))
          (evil-was-yanked-without-register nil))
        (evil-yank-string input register))
    (histdir-repl-delete-input))
(evil-define-operator histdir-repl-evil-change-input (start end type register)
    :move-point nil
    (interactive "<R><x>")
    (if (eq type 'line)
        (histdir-repl-evil-delete-input-line register)
        (let ((deleted (histdir-repl-enter+delete-input-range start end))
              (evil-was-yanked-without-register nil))
            (evil-yank-string deleted register)))
    (evil-insert-state))
(evil-define-operator histdir-repl-evil-change-input-line (&optional register)
    :move-point nil
    (interactive "<x>")
    (let ((input (histdir-repl-get-input))
          (evil-was-yanked-without-register nil))
        (evil-yank-string input register))
    (histdir-repl-delete-input)
    (evil-insert-state))
(evil-define-command histdir-repl-evil-paste-after (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-remove-suffix "\n" string)))
        (histdir-repl-enter-input 1)
        (process-send-string nil string)))
(evil-define-command histdir-repl-evil-paste-before (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-remove-suffix "\n" string)))
        (histdir-repl-enter-input 0)
        (process-send-string nil string)
        (histdir-repl-backward-char-in-input (length string))))
(evil-define-command histdir-repl-evil-replacing-paste-after
        (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-remove-suffix "\n" string))
           (length (length string)))
        (histdir-repl-enter+delete-input length)
        (process-send-string nil string)))
(evil-define-command histdir-repl-evil-replacing-paste-before
        (count &optional register)
    :suppress-operator t
    (interactive "*P<x>")
    (let* ((string (evil-paste-to-string count register))
           (string (string-remove-suffix "\n" string))
           (length (length string)))
        (histdir-repl-enter+delete-input length)
        (process-send-string nil string)
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
        (histdir-repl command histdir (cons "PAGER=cat" process-environment))))
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

(defvar norecord-override nil)
(defun norecord-override--1 (function window-or-frame &optional norecord)
    (when norecord-override
        (setq norecord (funcall norecord-override
                           function window-or-frame norecord)))
    (funcall function window-or-frame norecord))
(advice-add 'select-window :around 'norecord-override--1)
(advice-add 'select-frame :around 'norecord-override--1)
(defun norecord-override--2 (function frame window &optional norecord)
    (when norecord-override
        (setq norecord (funcall norecord-override
                           function frame window norecord)))
    (funcall function frame window norecord))
(advice-add 'set-frame-selected-window :around 'norecord-override--2)

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
    (defun hack-aw--lead-overlay (path leaf)
        (let ((window (cdr leaf)))
            (with-selected-window window
                (let* ((start (window-start))
                       (text (aw--overlay-str window (point-max) path))
                       (end (save-excursion
                                (goto-char start)
                                (let* ((width (string-width text))
                                       (initial (current-column))
                                       (target (+ initial width))
                                       (final (move-to-column target))
                                       (delta (- final target)))
                                    (when (> delta 0)
                                        (if (equal (char-before) ?\t)
                                            (backward-char)
                                            (let ((padding (make-string delta ? )))
                                                (setq text (concat text padding))))))
                                (point)))
                       (property (if (> (- end start) 0)
                                     'display
                                     'after-string))
                       (face (if (window-minibuffer-p window)
                                 'aw-minibuffer-leading-char-face
                                 'aw-leading-char-face))
                       (overlay (make-overlay start end
                                    (window-buffer window))))
                    (put-text-property 0 (length text) 'face face text)
                    (overlay-put overlay property text)
                    (overlay-put overlay 'window window)
                    (push overlay avy--overlays-lead)))))
    (advice-add 'aw--lead-overlay :override 'hack-aw--lead-overlay)
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
                     (let ((norecord-override 'always))
                         (save-selected-window
                             ,@body
                             (selected-window)))
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
        (when (eq window-state--action 'window-state-target-prefix)
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
                         (unwind-protect
                             (progn
                                 ,@body)
                             (window-state--normal))))
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
                     (let ((norecord-override nil))
                         (with-selected-window window
                             (unwind-protect
                                 (progn
                                     ,@body)
                                 (setq norecord-override 'ignore)))))
                 (window-state--define-operator ,move-name
                     (with-selected-window window
                         ,@body)
                     (select-window window)))))
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
        (with-advice (('read-buffer-to-switch :override 'read-buffer))
            (become-command 'switch-to-buffer)))
    (window-state-define-operator window-state-open
        (call-interactively 'find-file))
    (window-state-define-operator window-state-target-prefix
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
    (window-state-define-operator window-state-close
        (kill-buffer)
        (when (or (window-parent) (minibufferp))
            (evil-window-delete)))
    (defun window-state-use-register ()
        (interactive)
        (condition-case _error
            (setq window-state-this-register
                (call-interactively 'evil-use-register))
            (quit))
        (setq window-state--execute-once t))
    (window-state-define-operator window-state-send
        (select-window window)
        (let* ((keys (read-key-sequence nil))
               (binding (key-binding keys t)))
            (setq last-command-event (aref keys (1- (length keys))))
            (if binding
                (call-interactively binding)
                (undefined))))
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
    (define-key window-state-map "P" 'window-state-paste-operator)
    (define-key window-state-map "x" 'window-state-close-operator)
    (define-key window-state-map "X" 'window-state-close)
    (define-key window-state-map "c" 'window-state-search-move-operator)
    (define-key window-state-map "C" 'window-state-search)
    (define-key window-state-map "/" 'window-state-search)
    (define-key window-state-map "n" 'switch-to-buffer-next)
    (define-key window-state-map "N" 'switch-to-buffer-previous)
    (define-key window-state-map "o" 'window-state-open-move-operator)
    (define-key window-state-map "O" 'window-state-open)
    (define-key window-state-map "t" 'window-state-target-prefix-operator)
    (define-key window-state-map "T" 'window-state-target-prefix)
    (define-key window-state-map "f" 'window-state-unbury)
    (define-key window-state-map "F" 'window-state-unbury-operator)
    (define-key window-state-map "b" 'window-state-bury)
    (define-key window-state-map "B" 'window-state-bury-operator)
    (define-key window-state-map "w" 'window-state-swap-move-operator)
    (define-key window-state-map "W" 'window-state-swap-operator)
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
    (defun make-backtick-fence-regex (length &optional end-of-line)
        (string-replace "~" "`"
            (markdown-make-tilde-fence-regex length end-of-line)))
    (setcar
        (nth 1 (nth 2 markdown-fenced-block-pairs))
        'make-backtick-fence-regex)
    (defconst backtick-code-fence-start-regex
        (concat
            "^[[:blank:]]*"
            "\\(?1:```+\\)"
            "\\(?2:[[:blank:]]*\\)"
            "\\(?3:[^`{}[:space:]]+?\\)?"
            "\\(?:[[:blank:]]+\\(?4:.+?\\)\\)?"
            "\\(?5:[[:blank:]]*\\)$"))
    (defconst backtick-directive-fence-start-regex
        (concat
            "^[[:blank:]]*"
            "\\(?1:```+\\)"
            "\\(?2:[[:blank:]]*{[[:blank:]]*\\)"
            "\\(?3:[^`[:space:]]+?\\)?"
            "\\(?:[[:blank:]]+\\(?4:.+?\\)\\)?"
            "\\(?5:[[:blank:]]*}[[:blank:]]*\\)$"))
    (setcar
        (nth 0 (nth 2 markdown-fenced-block-pairs))
        backtick-code-fence-start-regex)
    (push
        (list
            (list
                backtick-directive-fence-start-regex
                'markdown-directive-block-begin)
            (list
                (car (nth 1 (nth 2 markdown-fenced-block-pairs)))
                'markdown-directive-block-end)
            nil)
        markdown-fenced-block-pairs)
    (define-key markdown-mode-map [backtab] nil))

(use-packages evil markdown-mode
    :config
    (evil-define-key 'normal markdown-mode-map
        "\C-m" 'markdown-follow-thing-at-point))

(use-package denote
    :config
    (setq denote-file-type 'markdown-yaml)
    (setq denote-known-keywords '())
    (setq denote-history-completion-in-prompts nil)
    (setq denote-rename-confirmations '(modify-file-name add-front-matter))
    (defun denote-file-note-type (path)
        (when-let ((_ (or (not (file-exists-p path))
                          (and (file-regular-p path)
                               (not (file-symlink-p path)))))
                   (extension (denote-get-file-extension-sans-encryption path))
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
    (setq denote-faces-file-name-keywords-for-dired
        `((dired-filename-search-forward
           ("\\.trashed-[0-9]+-"
               ,point-to-match-beginning-form
               ,point-to-match-end-form
               (0 'android-trash-face))
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
                   (search-forward-regexp "\\.trashed-[0-9]+-" (match-end 0) t)
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
    (set-face-foreground 'denote-faces-keywords "#8080FF")
    (set-face-foreground 'denote-faces-extension "grey30")
    (defface task-faces-repeat   '((t :inherit default)) "")
    (defface task-faces-duration '((t :inherit default)) "")
    (set-face-foreground 'task-faces-repeat   "#FF0000")
    (set-face-foreground 'task-faces-duration "#00FF00")
    (defface android-trash-face '((t :inherit default)) "")
    (set-face-foreground 'android-trash-face "#802020")
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
    (defun denoted-rewrite-front-matter (path title tags type)
        (let* ((buffers (buffer-list))
               (buffer  (find-file-noselect path))
               (was-already-open    (memq buffer buffers))
               (had-unsaved-changes (when was-already-open
                                        (refresh-modified-state buffer)
                                        (buffer-modified-p buffer))))
            (denote-rewrite-front-matter path title tags type)
            (unless had-unsaved-changes
                (with-current-buffer buffer
                    (save-buffer)))
            (unless was-already-open
                (kill-buffer buffer))))
    (defun denoted--rename (path new-path directory title tags)
        (if (and (memq 'modify-file-name denote-rename-confirmations)
                 (not (denoted-rename-file-prompt path new-path)))
            path
            (when (not (equal path new-path))
                (condition-case _error
                    (dired-rename-file path new-path 0)
                    (file-missing
                        (when-let (buffer (get-file-buffer path))
                            (with-current-buffer buffer
	                        (set-visited-file-name new-path nil t))))))
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
        (setq path (expand-file-name path))
        (let* ((directory (file-name-directory path))
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
    (defun denoted-try--default-fallback ()
        (interactive)
        (user-error "%s is not visiting a file or directory" (buffer-name)))
    (defun denoted-try (function &optional fallback-command)
        (setq-if-nil fallback-command 'denoted-try--default-fallback)
        (if-let (path (buffer-file-name))
             (funcall function path)
             (if (derived-mode-p 'dired-mode)
                 (when-let (path (funcall function (dired-get-filename)))
                     (dired-goto-file path))
                 (become-command fallback-command))))
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
            (prog1
                (denoted-tag-set path unique)
                (add-evil-repeat (denoted-tag-add--repeat added)))))
    (defun denoted-tag-add--repeat (added)
        (apply-partially 'denoted-try
            (lambda-let (added) (path)
                (let* ((tags   (denoted-tag-get path))
                       (merged (append tags added))
                       (unique (seq-uniq merged)))
                    (denoted-tag-set path unique)))))
    (defun denoted-tag-remove (path)
        (let* ((tags      (denoted-tag-get path))
               (deleted   (denoted-tag-prompt tags))
               (remaining (seq-difference tags deleted)))
            (prog1
                (denoted-tag-set path remaining)
                (add-evil-repeat (denoted-tag-remove--repeat deleted)))))
    (defun denoted-tag-remove--repeat (deleted)
        (apply-partially 'denoted-try
            (lambda-let (deleted) (path)
                (let* ((tags      (denoted-tag-get path))
                       (remaining (seq-difference tags deleted)))
                    (denoted-tag-set path remaining)))))
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
    (defun history-or-tag-execute-or-add (prefix-argument)
        (interactive "P")
        (denoted-try 'denoted-tag-add 'history-execute))
    (defun history-or-tag-remove (prefix-argument)
        (interactive "P")
        (denoted-try 'denoted-tag-remove 'history-remove))
    (define-key evil-motion-state-map "gh" 'history-or-tag-execute-or-add)
    (define-key evil-motion-state-map "gr" 'history-or-tag-remove)
    (defun history-or-title-change (prefix-argument)
        (interactive "P")
        (denoted-try 'denoted-title-edit 'history-change))
    (define-key evil-motion-state-map "gc" 'history-or-title-change)
    (defun datetime-edit ()
        (interactive)
        (denoted-try 'denoted-datetime-edit))
    (define-key evil-motion-state-map "gs" 'datetime-edit)
    (defun suffix-edit ()
        (interactive)
        (denoted-try 'denoted-suffix-edit))
    (define-key evil-motion-state-map "gS" 'suffix-edit)
    (defun name-change (prefix-argument)
        (interactive "P")
        (denoted-try 'denoted-name-edit))
    (define-key evil-motion-state-map "gC" 'name-change)
    (defun smoother-delete-file--1 (filename)
        (when (confirm-p (format "Delete %s?" (abbreviate-file-name filename)))
            (dired-delete-file filename)
            (when-let (buffer (find-buffer-visiting filename))
                (with-current-buffer buffer
                    (set-buffer-modified-p t)
                    (when (confirm-p (format "Kill %s?" (buffer-name)))
                        (with-buffer-modified-p nil
                            (smoother-kill-buffer))))))
        nil)
    (defun smoother-delete-file ()
        (interactive)
        (denoted-try 'smoother-delete-file--1))
    (define-key space-map "D" 'smoother-delete-file)
    (defun after-save-update-dired-buffers ()
        (add-dired-file buffer-file-name))
    (add-hook 'after-save-hook 'after-save-update-dired-buffers)
    (evil-define-command note (prefix-argument &optional register)
        (interactive "P<x>")
        (setq evil-this-register nil)
        (denote)
        (if (or prefix-argument register)
            (evil-paste-after 1 register)
            (set-buffer-modified-p nil)
            (evil-insert-state)))
    (define-key space-map "n" 'note)
    (defun note-list ()
        (interactive)
        (let* ((buffer (dired-find-buffer-nocreate denote-directory))
               (was-already-open (if buffer t nil))
               (was-already-focused (eq (current-buffer) buffer)))
            (if buffer
                (progn
                    (switch-to-buffer buffer)
                    (revert-buffer))
                (setq buffer (dired denote-directory))
                (dired-hide-details-mode 1)
                (denote-dired-mode 1))
            (list buffer was-already-open was-already-focused)))
    (define-key space-map "N"
        (lambda ()
            (interactive)
            (unless (cadr (note-list))
                (dired-goto-last-file))))
    (defun note-list-search ()
        (interactive)
        (let-unpack ((buffer was-already-open was-already-focused) (note-list))
            (run-with-idle-timer 0 nil
                (lambda-let (buffer was-already-open was-already-focused) ()
                    (condition-case error
                        (consult-line nil t)
                        (quit
                            (unless was-already-focused
                                (quit-window (not was-already-open)))
                            (signal (car error) (cdr error))))
                    (unless was-already-focused
                        (bury-buffer))
                    (set-buffer buffer)
                    (dired-find-file)
                    (unless was-already-open
                        (kill-buffer buffer))))))
    (define-key space-search-map "n" 'note-list-search)
    (defconst task-tag "qq")
    (setq denote-excluded-keywords-regexp "qq.*")
    (defun task-prompt ()
        (interactive)
        (denote-title-prompt nil "Task"))
    (defmacro task--with-filtered-ls (filter-regex &rest body)
        `(let ((insert-directory-program "emacs-task-ls")
               (process-environment process-environment))
             (when ,filter-regex
                 (push (concat "EMACS_TASK_FILTER=" ,filter-regex)
                     process-environment))
             ,@body))
    (defun task--buffer (title filter-regex)
        (without-local-variable ('process-environment)
            (task--with-filtered-ls filter-regex
                (reuse-independent-dired title denote-directory nil t)))
        (setq-local revert-buffer-function (task--buffer-revert filter-regex))
        (dired-hide-details-mode 1)
        (denote-dired-mode 1)
        (dired-goto-first-file))
    (defun task--buffer-revert (filter-regex)
        (lambda-let (filter-regex) (&rest arguments)
            (task--with-filtered-ls filter-regex
                (apply 'dired-revert arguments))))
    (defun task-list ()
        (interactive)
        (task--buffer "*Tasks*" nil))
    (defvar-local task-list--tags ())
    (defvar-local task-list--datetime nil)
    (defun task-create (prefix-argument)
        (interactive "P")
        (let* ((datetime (if prefix-argument
                             (datetime-read nil task-list--datetime)
                             (when task-list--datetime
                                 (datetime-floor task-list--datetime))))
               (denote-file-name-slug-functions
                   '((title . denoted-title-slug)))
               (denote-kill-buffers t)
               (inhibit-redisplay t)
               (path (denote (task-prompt) (cons task-tag task-list--tags)
                         nil nil datetime)))
            (unless (string-prefix-p "*Tasks" (buffer-name))
                (task-list))
            (dired-goto-file path)
            (pulse-momentary-highlight-region
                (pos-bol) (pos-eol))))
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
               (regex (string-replace "\\(?:" "\\(" regex))
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
        (let ((denote-rename-confirmations nil))
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
            (let ((denote-rename-confirmations nil))
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
(use-packages dired denote
    :config
    (define-key dired-mode-map "D" 'smoother-delete-file)
    (define-key dired-mode-map "R" 'denote-dired-mode))


(defconst mpv-ipc--server-option "--input-ipc-server=")
(defun mpv-ipc-socket-path-from-arguments (mpv-arguments)
    (let ((socket nil))
        (dolist (argument mpv-arguments socket)
            (when (string-prefix-p mpv-ipc--server-option argument)
                (setq socket (string-remove-prefix
                                 mpv-ipc--server-option argument))))))
(defun mpv-ipc-socket-path-from-process (mpv-process)
    (let* ((command   (process-command mpv-process))
           (arguments (cdr command)))
        (mpv-ipc-socket-path-from-arguments arguments)))
(define-error 'mpv-ipc-error "mpv-ipc error")
(define-error 'mpv-ipc-connect-error "mpv-ipc error connecting" 'mpv-ipc-error)
(define-error 'mpv-ipc-socket-missing "mpv-ipc socket missing"
    '(mpv-ipc-connect-error file-missing))
(define-error 'mpv-ipc-permission-denied "mpv-ipc permission denied"
    '(mpv-ipc-connect-error permission-denied))
(define-error 'mpv-ipc-connection-refused "mpv-ipc connection refused"
    '(mpv-ipc-connect-error file-error))
(defun mpv-ipc-connect (path)
    (let* ((expanded-path (expand-file-name path))
           (buffer-name (concat " *mpv-ipc: " path "*"))
           (buffer (generate-new-buffer buffer-name t)))
        (condition-case _error
            (condition-case error
                (make-network-process
                    :name path
                    :family 'local
                    :service expanded-path
                    :buffer buffer
                    :sentinel (lambda-let (buffer) (socket _event-string)
                                  (unless (process-live-p socket)
                                      (kill-buffer buffer))))
                (error
                    (ignore-errors
                        (kill-buffer buffer))
                    (signal (car error) (cdr error))))
            (file-missing
                (signal 'mpv-ipc-socket-missing (list path)))
            (permission-denied
                (signal 'mpv-ipc-permission-denied (list path)))
            (file-error
                (signal 'mpv-ipc-connection-refused (list path))))))
(defun mpv-ipc--consume (socket)
    (with-current-buffer (process-buffer socket)
        (goto-char (point-max))
        (while (not (search-backward "\n" nil t))
            (accept-process-output socket))
        (goto-char 1)
        (prog1
            (json-parse-buffer)
            (delete-region 1 (1+ (point))))))
(defun mpv-ipc (socket command)
    (let* ((message `("command" ,command))
           (message (json-encode-plist message))
           (message (concat message "\n"))
           (output  (process-buffer socket))
           (reply   nil))
        (process-send-string socket message)
        (while (gethash "event"
                   (setq reply (mpv-ipc--consume socket))))
        (let ((ipc-error (gethash "error" reply)))
            (unless (equal ipc-error "success")
                (signal 'mpv-ipc-error (list ipc-error))))
        (gethash "data" reply)))
(defun mpv-ipc-expand (socket format)
    (mpv-ipc socket `("expand-text" ,format)))
(defun mpv-ipc-expand-integer (socket format)
    (let ((string (mpv-ipc-expand socket format)))
        (unless (string-match-p "\\`-?[0-9]+\\'" string)
            (error "not integer: %s" string))
        (string-to-number string)))
(defun mpv-ipc-get (socket property)
    (let ((format (concat "${" property "}")))
        (mpv-ipc socket `("expand-text" ,format))))
(defun mpv-ipc-cycle (socket property &optional values backwards)
    (if values
        (if backwards
            (mpv-ipc socket `("cycle-values" "!reverse" ,property ,@values))
            (mpv-ipc socket `("cycle-values" ,property ,@values)))
        (if backwards
            (mpv-ipc socket `("cycle" ,property "down"))
            (mpv-ipc socket `("cycle" ,property))))
    (mpv-ipc-get socket property))
(defun mpv-ipc-add (socket property amount)
    (mpv-ipc socket `("add" ,property ,amount))
    (mpv-ipc-get socket property))
(defun mpv-ipc-set (socket property value)
    (when (numberp value)
        (setq value (number-to-string value)))
    (mpv-ipc socket `("set" ,property ,value))
    (mpv-ipc-get socket property))


(defconst music-directory "~/Music")
(defun music-select ()
    (let ((vertico-sort-function 'vertico-sort-alpha)
          (files (directory-files music-directory nil "^[^.]" t)))
        (let ((chosen (save-point-line-and-column-with-scroll
                          (completing-read-multiple "Music: " files)))
              (directory (file-name-as-directory music-directory)))
        (mapcar (apply-partially 'concat directory) chosen))))
(defconst music--command
    '("mpv" "--input-ipc-server=~/.music-socket"
         "--idle" "--loop-playlist" "--terminal=no"))
(define-derived-mode music-mode nil "music")
(defmacro music-define-key (state key def &rest bindings)
    `(evil-define-key ,state music-mode-map ,key ,def ,@bindings))
(defvar music--socket nil)
(defun music ()
    (interactive)
    (if-let ((buffer  (get-buffer "*Music*"))
             (process (get-buffer-process buffer)))
        (pop-to-buffer buffer)
        (setq buffer (get-buffer-create "*Music*"))
        (set-buffer buffer)
        (setq buffer-read-only t)
        (setq default-directory music-directory)
        (music-mode)
        (setq process (make-process
                          :name "music"
                          :command music--command
                          :buffer buffer
                          :noquery t))
        (let* ((socket-path (mpv-ipc-socket-path-from-process process))
               (socket1 (wait-until
                            (ignore-error mpv-ipc-connect-error
                                (mpv-ipc-connect socket-path))
                            0.1))
               (socket2 (mpv-ipc-connect socket-path)))
            (mpv-ipc socket1 '("disable_event" "all"))
            (mpv-ipc socket2 '("disable_event" "all"))
            (set-process-query-on-exit-flag socket1 nil)
            (set-process-query-on-exit-flag socket2 nil)
            (setq music--socket socket1)
            (let ((timer (run-with-timer 0 0.1 'music--refresh socket2 buffer)))
                (add-hook 'kill-buffer-hook
                    (lambda-let (timer socket-path) ()
                        (cancel-timer timer)
                        (delete-file socket-path))
                    nil t)))
        (add-hook 'post-command-hook 'music--post-command-seek nil t)
        (pop-to-buffer buffer)))
(defvar music--refresh-next-line nil)
(defvar music--refresh-next-index nil)
(defvar music--refresh-next-column nil)
(defun music--refresh (socket buffer)
    (when (get-buffer-window buffer 'visible)
        (with-current-buffer buffer
            (unless (or (evil-visual-state-p)
                        (evil-operator-state-p))
                (save-point-line-and-column-with-scroll
                    (let ((inhibit-read-only t))
                        (erase-buffer)
                        (let ((inhibit-quit nil))
                            (music--insert-playlist socket))))
                (when-let (position (next-single-property-change
                                        1 'mpv--position))
                    (progn
                        (goto-char position)
                        (setq temporary-goal-column (current-column))))
                (when music--refresh-next-line
                    (goto-line music--refresh-next-line)
                    (setq music--refresh-next-line nil))
                (when music--refresh-next-index
                    (goto-char 1)
                    (when-let (match (text-property-search-forward
                                         'mpv-index
                                         music--refresh-next-index
                                         'equal))
                        (goto-char (prop-match-beginning match)))
                    (setq music--refresh-next-index nil))
                (when music--refresh-next-column
                    (move-to-column music--refresh-next-column)
                    (setq music--refresh-next-column nil)
                    (setq temporary-goal-column (current-column)))))))
(defun music--line-move-after-refresh (count)
    (let ((current (line-number-at-pos (point))))
        (setq music--refresh-next-line (+ current count)))
    (setq music--refresh-next-column (current-column)))
(defun music--index-move-after-refresh (count)
    (let ((current (if (= (point) (buffer-end 1))
                       (if (= (point) 1)
                           1
                           (get-text-property (1- (point)) 'mpv-index))
                       (get-text-property (point) 'mpv-index))))
        (setq music--refresh-next-index (+ current count)))
    (setq music--refresh-next-column (current-column)))
(defface music-current-entry-face '((t :foreground "#80FFFF")) "")
(defface music-current-playing-entry-face
    '((t :inherit music-current-entry-face :weight bold :foreground "#FF4040"))
    "")
(defun music--insert-playlist (socket)
    (let ((count   (mpv-ipc-expand-integer socket "${playlist-count}"))
          (current (mpv-ipc-expand-integer socket "${playlist-pos}"))
          (face    (if (equal (mpv-ipc-expand socket "${pause}") "yes")
                       'music-current-entry-face
                       'music-current-playing-entry-face)))
        (dotimes (index count)
            (insert (music--playlist-entry socket index current face)))))
(defun music--propertize (index path text)
    (propertize text 'mpv-index index 'full-path path))
(defun music--playlist-entry (socket index current face)
    (let* ((format (format "${playlist/%d/filename}" index))
           (path (mpv-ipc-expand socket format))
           (file (file-name-nondirectory path))
           (file-line (format "%d. %s\n" (1+ index) file)))
        (music--propertize index path
            (if (= index current)
                (music--playlist-current-entry socket file-line face)
                file-line))))
(defun music--playlist-current-entry (socket file-line face)
    (let ((playing-line (music--playing-line socket))
          (seek-lines   (music--seek-lines socket)))
        (setq file-line (propertize file-line 'face face))
        (when (evil-motion-state-p)
            (unless (next-single-property-change
                        0 'mpv--position seek-lines)
                (put-text-property 0 1 'mpv--position t file-line)))
        (concat file-line playing-line seek-lines)))
(defconst music--seek-bar
    "%04dm---- 10sss---- 20sss---- 30sss---- 40sss---- 50sss----")
(defun music--playing-line (socket)
    (let ((loop (mpv-ipc-expand socket "${loop}")))
        (cond
            ((equal loop "no")
                (setq loop " "))
            ((equal loop "inf")
                (setq loop "∞"))
            ((> (length loop) 1)
                (setq loop "+")))
        (format "  %s %s"
            loop
            (mpv-ipc-expand socket "${time-pos} / ${duration}\n"))))
(defun music--get-seconds (socket format)
     (let* ((raw (mpv-ipc-expand socket format))
            (parts (split-string raw "\\.")))
         (string-to-number (car parts))))
(defun music--seek-lines (socket)
    (when (evil-motion-state-p)
        (let* ((duration (music--get-seconds socket "${=duration}"))
               (duration-minutes (floor duration 60))
               (duration-seconds (mod duration 60))
               (position (music--get-seconds socket "${=time-pos}"))
               (lines ()))
            (push "" lines)
            (let ((line (format music--seek-bar duration-minutes)))
                (push (substring line 0 duration-seconds) lines))
            (while (> duration-minutes 0)
                (setq duration-minutes (1- duration-minutes))
                (let ((line (format music--seek-bar duration-minutes)))
                    (push line lines)))
            (let ((seek-bar (string-join lines "\n"))
                  (start position))
                (put-text-property start (1+ start) 'mpv--position t seek-bar)
                (propertize seek-bar
                    'field 'seek-bar
                    'front-sticky '(field))))))
(defun music--post-command-seek ()
    (when (and (evil-motion-state-p)
               (not (get-text-property (point) 'mpv--position))
               (eq (get-text-property (point) 'field) 'seek-bar))
        (let ((seconds-from-start (- (point) (field-beginning)))
              (inhibit-quit nil))
            (mpv-ipc music--socket `("seek" ,seconds-from-start "absolute")))))
(define-key music-mode-map "q" 'quit-window)
(defun music-eval ()
    (interactive)
    (let* ((input   (read-string "mpv IPC: "))
           (command (split-string input)))
        (princ (mpv-ipc music--socket command))))
(defun music-playlist-position ()
    (mpv-ipc-expand-integer music--socket "${playlist-pos}"))
(defun music-playlist-count ()
    (mpv-ipc-expand-integer music--socket "${playlist-count}"))
(defun music-playlist-add (path target &optional play)
    (setq path (expand-file-name path))
    (let ((action)
          (command))
        (cond
            ((eq target 'last)
                (setq action "append"))
            ((eq target 'next)
                (setq action "insert-next"))
            (t
                (setq action "insert-at")))
        (when play
            (setq action (concat action "-play")))
        (if (memq target '(last next))
            (setq command (list "loadfile" path action))
            (setq command (list "loadfile" path action target)))
        (mpv-ipc music--socket command)))
(defun music-playlist-remove (target)
    (cond
        ((eq target 'all)
            (mpv-ipc music--socket (list "playlist-clear"))
            (mpv-ipc music--socket (list "playlist-remove" "current")))
        ((eq target 'all-except-current)
            (mpv-ipc music--socket (list "playlist-clear")))
        (t
            (mpv-ipc music--socket (list "playlist-remove" target)))))
(defun music-normal-state ()
    (interactive)
    (evil-normal-state)
    (let ((index (music-playlist-position)))
        (when (>= index 0)
            (goto-char (point-max))
            (text-property-search-backward 'mpv-index index 'equal))))
(evil-define-operator music-delete (start end type register yank-handler)
    :move-point nil
    :type line
    (interactive "<R><x><y>")
    (let ((evil-was-yanked-without-register nil))
        (evil-yank start end type register yank-handler))
    (let* ((text    (evil-paste-to-string 1 register))
           (indexes (text-property-values nil nil 'mpv-index text)))
        (dolist (index (nreverse indexes))
            (music-playlist-remove index))))
(music-define-key 'normal "d" 'music-delete)
(evil-define-operator music-delete-line (register yank-handler)
    :move-point nil
    :motion nil
    (interactive "<x><y>")
    (let ((start (line-beginning-position))
          (end   (line-end-position))
          (evil-was-yanked-without-register nil))
        (evil-yank start end 'line register yank-handler))
    (let* ((text (evil-paste-to-string 1 register))
           (index (car (text-property-values nil nil 'mpv-index text))))
        (music-playlist-remove index)))
(music-define-key 'normal "D" 'music-delete-line)
(defun music--index-for-point (offset)
    (if-let (index (get-text-property (point) 'mpv-index))
        (+ index offset)
        (music-playlist-count)))
(defun music--paths-for-paste (register)
    (full-path-property-split nil nil (evil-paste-to-string 1 register)))
(defun music--open (count paths target)
    (setq paths (nreverse paths))
    (dotimes (_ count)
        (dolist (path paths)
            (music-playlist-add path target)))
    (* count (length paths)))
(defun music--paste (count register offset)
    (let* ((paths (music--paths-for-paste register))
           (index (music--index-for-point offset)))
        (music--open count paths index)))
(evil-define-command music-paste-after (count register)
    :suppress-operator t
    (interactive "p<x>")
    (music--index-move-after-refresh
        (music--paste count register 1)))
(music-define-key 'normal "p" 'music-paste-after)
(evil-define-command music-paste-before (count register)
    :suppress-operator t
    (interactive "p<x>")
    (music--paste count register 0))
(music-define-key 'normal "P" 'music-paste-before)
(evil-define-command music-replacing-paste-before (count register)
    :suppress-operator t
    (interactive "p<x>")
    (let* ((paths (music--paths-for-paste register))
           (start (point))
           (end   (line-end-position (* count (length paths))))
           (indexes (text-property-values start end 'mpv-index)))
        (dolist (index (nreverse indexes))
            (music-playlist-remove index))
        (let ((index (music--index-for-point 0)))
            (music--open count paths index))))
(evil-define-command music-replacing-paste-after (count register)
    :suppress-operator t
    (interactive "p<x>")
    (music--index-move-after-refresh
        (music-replacing-paste-before count register)))
(music-define-key 'normal "gp" 'music-replacing-paste-after)
(music-define-key 'normal "gP" 'music-replacing-paste-before)
(defun music-open-after (count)
    (interactive "p")
    (music--index-move-after-refresh
        (music--open count (music-select) (music--index-for-point 1))))
(music-define-key 'normal "o" 'music-open-after)
(defun music-open-before (count)
    (interactive "p")
    (music--open count (music-select) (music--index-for-point 0)))
(music-define-key 'normal "O" 'music-open-before)
(evil-define-operator music-change (start end type register yank-handler)
    :move-point nil
    :type line
    (interactive "<R><x><y>")
    (music-delete start end type register yank-handler)
    (music-open-before 1))
(music-define-key 'normal "c" 'music-change)
(evil-define-operator music-change-line (register yank-handler)
    :move-point nil
    :motion nil
    (interactive "<x><y>")
    (music-delete-line register yank-handler)
    (music-open-before 1))
(music-define-key 'normal "C" 'music-change-line)
(defun music-open (path prefix-argument)
    (interactive (list (car (music-select)) current-prefix-arg))
    (music)
    (if prefix-argument
        (if (integerp prefix-argument)
            (if (< prefix-argument 0)
                (let* ((count (music-playlist-count))
                       (target (+ count prefix-argument)))
                    (music-playlist-add path target t))
                (music-playlist-add path prefix-argument t))
            (if (> (prefix-numeric-value prefix-argument) 4)
                (mpv-ipc music--socket `("loadfile" ,path "replace"))
                (music-playlist-add path 'next t)))
        (music-playlist-add path 'last t)))
(defun music-pause-toggle ()
    (interactive)
    (let ((paused (mpv-ipc-cycle music--socket "pause")))
        (message "mpv pause: %s" paused)))
(music-define-key 'normal "x" 'music-pause-toggle)
(music-define-key 'motion "x" 'music-pause-toggle)
(defun music-pause ()
    (interactive)
    (let ((paused (mpv-ipc-set music--socket "pause" "yes")))
        (message "mpv pause: %s" paused)))
(music-define-key 'normal "X" 'music-pause)
(music-define-key 'motion "X" 'music-pause)
(defun music-next (count)
    (interactive "p")
    (let ((command "playlist-next"))
        (when (< count 0)
            (setq command "playlist-prev")
            (setq count (- count)))
        (dotimes (_ count)
            (mpv-ipc music--socket (list command)))))
(music-define-key 'normal "L" 'music-next)
(music-define-key 'motion "L" 'music-next)
(defun music-previous (count)
    (interactive "p")
    (music-next (- count)))
(music-define-key 'normal "H" 'music-previous)
(music-define-key 'motion "H" 'music-previous)
(evil-define-command music-volume (&optional volume)
    :suppress-operator t
    (interactive "<c>")
    (if volume
        (setq volume (mpv-ipc-set music--socket "volume" volume))
        (setq volume (mpv-ipc-get music--socket "volume")))
    (message "mpv volume: %s" volume))
(music-define-key 'normal "=" 'music-volume)
(music-define-key 'motion "=" 'music-volume)
(defun music-volume-up (count)
    (interactive "p")
    (let ((volume (mpv-ipc-add music--socket "volume" count)))
        (message "mpv volume: %s" volume)))
(music-define-key 'motion "+" 'music-volume-up)
(defun music-volume-down (count)
    (interactive "p")
    (music-volume-up (- count)))
(music-define-key 'motion "-" 'music-volume-down)
(defun music--loop (property count)
    (if count
        (mpv-ipc-set music--socket property count)
        (mpv-ipc-cycle music--socket property '("no" "inf"))))
(evil-define-command music-loop (&optional count)
    :suppress-operator t
    (interactive "<c>")
    (message "mpv loop current: %s" (music--loop "loop" count)))
(evil-define-command music-loop-all (&optional count)
    :suppress-operator t
    (interactive "<c>")
    (message "mpv loop playlist: %s" (music--loop "loop-playlist" count)))
(music-define-key 'normal "&" 'music-loop)
(music-define-key 'motion "&" 'music-loop)
(music-define-key 'normal "g&" 'music-loop-all)
(music-define-key 'motion "g&" 'music-loop-all)
(defun music-play (target)
    (interactive (list
                     (if current-prefix-arg
                         (1- (prefix-numeric-value current-prefix-arg))
                         (get-text-property (point) 'mpv-index))))
    (let ((current (mpv-ipc-expand-integer music--socket "${playlist-pos}")))
        (if (equal target current)
            (music-pause-toggle)
            (mpv-ipc music--socket (list "playlist-play-index" target)))))
(define-key music-mode-map "\C-m" 'music-play)
(music-define-key 'normal "i" 'evil-motion-state)
(music-define-key 'normal "a" 'evil-motion-state)
(music-define-key 'normal "A" 'evil-motion-state)
(music-define-key 'normal "I" 'evil-motion-state)
(music-define-key 'normal [escape] 'quit-window)
(music-define-key 'motion [escape] 'music-normal-state)
(add-to-list 'evil-normal-state-modes 'music-mode)

(define-key space-search-map "m" 'music-open)
(define-key space-map "m" 'music)
(define-key space-map ">" 'music-next)
(define-key space-map "<" 'music-previous)
(define-key space-map "&" 'music-loop)
(define-key space-map "p" 'music-pause-toggle)

(defconst tumblr--python (expand-file-name "~/.tumblr/venv/bin/python"))
(defconst tumblr--script (expand-file-name "~/.tumblr/tumblr.py"))
(defvar tumblr-blogs '(
    "mentalisttraceur"
    "mentalisttraceur-humor"
    "mentalisttraceur-long"
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
             (error "tumblr error: %s" output))))
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
(defun tumblr-pull ()
    (interactive)
    (let-unpack ((status output) (funcall-process "p"))
        (if (equal status 0)
            (let* ((default-directory denote-directory)
                   (file (tumblr "pull" output)))
                (when-let ((buffer (find-buffer-visiting file)))
                    (refresh-modified-state buffer))
                (find-file file))
            (error "paste error: %s" output))))
(defun tumblr-link (path)
    (let ((blog (tumblr "get" "blog" path))
          (post (tumblr "get" "post" path)))
        (when (or (equal blog "") (equal post ""))
            (user-error "%s is not a Tumblr post" path))
        (format "https://%s.tumblr.com/post/%s" blog post)))
(evil-define-command tumblr-open (prefix-argument register)
    (interactive "P<x>")
    (denoted-try
        (lambda-let (prefix-argument register) (path)
            (let ((link (tumblr-link path)))
                (if prefix-argument
                    (evil-yank-string link register)
                    (browse-url link)))
            nil)))
(define-key space-misc-map "w" 'tumblr-publish)
(define-key space-misc-map "d" 'tumblr-delete)
(define-key space-misc-map "u" 'tumblr-pull)
(define-key space-misc-map "o" 'tumblr-open)

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
(defun russian-vi-bind--1 (keymap russian-key english-key)
    (when-let (binding (lookup-key keymap english-key))
        (define-key keymap russian-key binding)
        (define-key keymap english-key nil t)
        (define-key keymap english-key binding)))
(defun russian-vi-bind (map)
    (dolist (pair russian-vi-symbol-pairs)
        (let-unpack ((russian english) pair)
            (russian-vi-bind--1 map russian english)))
    (dolist (pair russian-vi-letter-pairs)
        (let-unpack ((russian english) pair)
            (russian-vi-bind--1 map russian english)
            (let ((russian (upcase russian))
                  (english (upcase english)))
                (russian-vi-bind--1 map russian english))
            (let ((russian (kbd (concat "C-" russian)))
                  (english (kbd (concat "C-" english))))
                (russian-vi-bind--1 map russian english))))
    (map-keymap
        (lambda (_event binding)
            (when (symbolp binding)
                (setq binding (symbol-function binding)))
            (when (keymapp binding)
                (russian-vi-bind binding)))
        map))
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
                       window-state-map histdir-repl-mode-map music-mode-map
                       undo-tree-visualizer-mode-map
                       calendar-mode-map global-map))
        (russian-vi-bind map))
    (dolist (cell evil-minor-mode-keymaps-alist)
        (let ((inner-alist (cdr cell)))
            (dolist (cell inner-alist)
                (let ((map (cdr cell)))
                    (russian-vi-bind map))))))


(setq gc-cons-threshold init-gc-cons-threshold
      file-name-handler-alist init-file-name-handler-alist)


(setq initial-buffer-choice 'eshell)
