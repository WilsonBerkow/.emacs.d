;; Wilson Berkow init.el

;; Generic input options:
;; ~ means consider removing
(column-number-mode)
(show-paren-mode 1)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(set-buffer-file-coding-system 'utf-8-unix)
(setq inhibit-startup-screen t)
(setq echo-keystrokes 0.1)
(setq delete-by-moving-to-trash t) ;; ~
(setq shift-select-mode nil)
(delete-selection-mode 1) ;; Delete selected text after start typing
(global-linum-mode 1)
(subword-mode 1)
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; Consider: (require 'undo-tree)...? Just to try.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Melpa:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
    If NO-REFRESH is non-nil, the available package lists will not be
     re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun init--install-packages ()
  (mapcar 'require-package
          '(;; magit
            visible-mark
            god-mode ;; experimental ATM
            haskell-mode
            async
            rust-mode
            nodejs-repl
            markdown-mode
            ;; paredit
            clojure-mode)))

;; Because Windows Emacs has problems with SSL connections:
(defvar gnutls-trustfiles '("C:\\Users\\Wilson\\Downloads\\gnutls\\certificate-also-necessary\\cacert.pem"))


(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; REVISIT w3m LATER:
;; (setq browse-url-browser-function 'w3m-browse-url)
;; (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; (global-set-key "\M-~" 'browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consider another approach
;; ;; visible-mark -- show where the mark is, not where the region is
;; (defface visible-mark-active
;;   '((t (:background "orange"
;;         :weight ultra-bold)))
;;   "")
;; (transient-mark-mode 0)
;; (global-visible-mark-mode 1)


;; god-mode
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-meta-mode)
(define-key god-local-mode-map (kbd "<backspace>") 'god-mode-backspace)
(define-key god-local-mode-map (kbd "/") 'god-mode-all)
(setq god-exempt-predicates nil)
(setq god-exempt-major-modes nil)

(defun god-update-cursor ()
  (setq cursor-type
        (if god-local-mode
            'box
          'bar)))
(add-hook 'god-mode-enabled-hook 'god-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-update-cursor)

(defun god-meta-mode ()
  (interactive)
  (if god-local-mode
      (god-mode-all)
    (progn
      (setq god-mod-alist
            '((nil . "M-")
              ("g" . "C-")
              ("G" . "C-M-")))
      (god-mode-all))))

;; (defun god-mode-ret ()
;;   (interactive)
;;   (forward-line 1)
;;   (move-beginning-of-line 1))

(defun god-mode-backspace ()
  (interactive)
  (delete-backward-char 1)
  (god-mode-all))

(god-meta-mode)

;; ELisp Editing
(put 'add-hook 'lisp-indent-function 1)

;; HTML:
(add-hook 'html-mode-hook
  (lambda ()
    ;; Indent with 4 spaces rather than 2-spaces default
    (set (make-local-variable 'sgml-basic-offset) 4)))

;; Haskell:
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

;;(add-to-list 'load-path "~/haskell-mode/")
;;(require 'haskell-mode-autoloads)
;;(add-to-list 'Info-default-directory-list "~/haskell-mode/")

;; To modify word movement and deletion:
;;(require 'subword-mode)
;; (defun char-stx-type (ch) (char-to-string (char-syntax ch)))
;; (defun point-stx () (char-stx-type (char-after (point))))
;; (defun fwd-word ()
;;   (interactive)
;;   ;; Skip whitespace:
;;   (while (/= (point-stx) "-") (forward-char))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern matching macro stuff:

;; (this is mostly for fun, considering a significant
;;  amt of pattern matching is built in)

;; VARIABLE:
(defun var-matchp (spec)
  (symbolp spec))
(defun fits-varp (_) t)

;; CONS CELL:
(defun cons-matchp (spec)
  (and (listp spec)
       (= (length spec) 3)
       (= (car spec) 'cons)))
(defun fits-consp (x) (consp x))

;; NIL:
(defun nil-matchp (spec)
  (null spec))
(defun fits-nilp (x) (null x))

;; VALUE: -- can include nil
(defun value-matchp (spec)
  (or (stringp spec)
      (numberp spec)
      (null spec)
      (charp spec)))
(defun fits-valuep (x) (value-matchp x))

(defun M_vconcat (&rest args)
  ;; TODO: a monadic vconcat. if any args is nil (or non-vector), then
  ;; return nil. Otherwise, concatenate
  )

(defun parse-match (x match)
  ;; Returns a list of (symbol . value) pairs if `x' matches
  ;; the structure of `match', and otherwise nil
  ;; TODO: distinguish between no-match-found and no-vars-bound
  (let ((match-spec (car match))
        (match-body (cdr match)))
    (cond ((var-matchp match-spec)
           (when (fits-varp x) [(match-spec . x)]))
          ((cons-matchp match-spec)
           (when (fits-consp x)
             (M_vconcat (parse-match (car x) (nth 1 match-spec))
                        (parse-match (cdr x) (nth 2 match-spec)))))
          ((value-matchp match-spec)
           (when (fits-valuep x) []))
          ;;
          )
    ))
;; TODO ^



(defmacro match-list (to-match nilmatch consmatch)
  ;; milmatch structure is (nil body...)
  ;; consmatch structure is ((cons carvarname consvarname) body...)
  (letrec ((cons-match-expr (car consmatch))
           (cons-match-body (cdr consmatch))
           (cons-match-car-name (car (cdr cons-match-expr)))
           (cons-match-cdr-name (car (cdr (cdr cons-match-expr))))
           (nil-match-body (cdr nilmatch)))
    `(if ,to-match
         (let ((,cons-match-car-name (car ,to-match))
               (,cons-match-cdr-name (cdr ,to-match)))
           ,(cons 'progn cons-match-body))
       ,(cons 'progn nil-match-body))))
(put 'match-list 'lisp-indent-function 1)

;; Example:
;; (defun printall (L)
;;   (match-list L
;;     (nil)
;;     ((cons x xs)
;;       (insert (string x))
;;       (printall xs))))
;; (printall '(?H ?e ?l ?l ?o ?!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS

;; Rationale behind rebinding of so many keys (below):
;; I have been getting used to Emacs's default mvmnt
;; commands (e.g. C- f,b,n,p for movement), but even though
;; it's fairly simple to get these into muscle memory, they,
;; and some other movment patterns (e.g. M-f for move by word,
;; C-f by char. Slow and awkward to jump btwn M and C so much).
;; There are a few sets of commands that provide really nice
;; versatile and work best with a full layout redo:
;;  scrolling (M-h, M-y, M-H, M-Y, M-I, M-K)
;;  movement (M-i,j,k,l,u,o,9,0)
;;  editing (M-w,e,d,f,F)
;;  window switching (M-t M-i,j,k,l)
;;  buffer switching (M-b M-j,l)
;; With the C-f/M-f example, the setup with M-i,j,k,l for character
;; movement and M-u,o for word movement allows for much faster
;; and more precise movement. Additionally, not having to jump
;; around on the keyboard, and being able to do most movement
;; with one hand, is pretty nice too. I'll be seeing if it's nice
;; enough to make up for the hassle of dealing with minor and major
;; modes shadowing my keybindings.

;; Helpers:
(defun cadar (L) (car (cdr (car L))))

(defun caar (L) (car (car L)))

(defun remap-kbds (km remaps)
  (when remaps
    (define-key km [remap (caar remaps)] (cadar remaps))
    (remap-kbds km (cdr remaps))))
(put 'remap-kbds 'lisp-indent-function 1)

(defun attach-kbds (km bindings)
  (when bindings
    (define-key km (kbd (caar bindings)) (cadar bindings))
    (attach-kbds km (cdr bindings))))
(put 'attach-kbds 'lisp-indent-function 1)



;; Org-mode:
;; TODO: THE FOLLOWING CUSTOMIZATIONS DONT WORK
(require 'org)

(defvar org-keys
  '(("C-a" org-mark-element
     "M-h" scroll-up-line ;; To overwrite default M-h org-mark-element binding
     "C-(" outline-previous-visible-heading
     "C-)" outline-next-visible-heading
     ;; Overwrite M-9,0?
     "C-i" org-shiftup
     "C-k" org-shiftdown
     "C-j" org-backward-sentence
     "C-l" org-forward-sentence
     "M-e" kill-word-backward)))

(attach-kbds org-mode-map org-keys)

;; Allow alphabetic list-labels (e.g. A) ... B) ... etc.)
(setq org-mode-list-allow-alphabetical t)

;; Set number of newlines allowed in a markup expression (default is just 1)
(setcar (nthcdr 4 org-emphasis-regexp-components) 10)



;; Some commands:
(defun previous-line-with-scroll ()
  (interactive)
  (previous-line)
  (scroll-down-line))

(defun next-line-with-scroll ()
  (interactive)
  (next-line)
  (scroll-up-line))

(defun back-to-indentation-or-beginning ()
  (interactive)
  (when (= (point) (progn (back-to-indentation) (point)))
    (beginning-of-line)))

(defun kill-word-backward (&optional arg)
  (interactive "P")
  (kill-word (if arg (- arg) -1)))

(defun kill-and-join-line-forward (&optional arg)
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

;; Functions to maximize in Windows with Emacs version < 24.4:
(defun restore-frame-with-w32 ()
  "Restore a maximized frame with w32-send-sys-command"
  (interactive)
  (w32-send-sys-command 61728))

(defun maximize-frame-with-w32 ()
  "Maximize the frame with w32-send-sys-command"
  (interactive)
  (w32-send-sys-command #xf030))

(defun get-frame-width ()
  (cdr (assoc 'width (frame-parameters))))
(defun get-frame-height ()
  (cdr (assoc 'height (frame-parameters))))

(defvar frame-maximized-width 150)
(defvar frame-maximized-height 40)

(defun frame-is-maximized ()
  (and (>= (get-frame-width) frame-maximized-width)
       (>= (get-frame-height) frame-maximized-height)))

;; Use either (toggle-frame-fullscreen) or the above fallback system:
(defun toggle-maximization ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      (if (frame-is-maximized)
          (restore-frame-with-w32)
        (maximize-frame-with-w32))
    (when (fboundp 'toggle-frame-fullscreen)
      (toggle-frame-fullscreen))))

(if (eq system-type 'windows-nt)
    ;; On Windows, set maximization as the default
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; On all else, just fullscreen it
  (toggle-maximization))

;; Global keybindings:
(defvar char-movement
  '(("M-j" backward-char)
    ("M-l" forward-char)
    ("M-i" previous-line)
    ("M-k" next-line)
    ("M-u" backward-word)
    ("M-o" forward-word)
    ("M-\S-i" previous-line-with-scroll)
    ("M-\S-k" next-line-with-scroll)))

(defvar sexp-movement
  '(("C-M-j" backward-sexp)
    ("C-M-l" forward-sexp)))
;; Figure out what the following do...
;;("C-M-i" forward-list)
;;("C-M-k" backward-list)))

(defvar other-movement
  '(("M-<" beginning-of-buffer)
    ("M->" end-of-buffer)
    ("M-(" backward-paragraph)
    ("M-)" forward-paragraph)
    ("M-n" back-to-indentation-or-beginning)
    ("M-m" move-end-of-line)))

(define-key (current-global-map)
  [remap move-beginning-of-line]
  'back-to-indentation-or-beginning)

(defvar edit-char
  '(("M-d" delete-char)
    ("M-s" delete-backward-char)
    ("M-w" kill-word-backward)
    ("M-e" kill-word)))

(defun kill-whole-line-maintain-column (&optional n)
  (interactive "p")
  (let ((column (current-column)))
    (if (= (line-number-at-pos (point)) (line-number-at-pos (buffer-end 1)))
        (kill-whole-line (- (or n 1)))
      (kill-whole-line n))
    (move-to-column column)))

(defvar edit-other
  '(("M-f" kill-and-join-line-forward)
    ("M-F" kill-whole-line-maintain-column)
    ("M-_" undo)
    ("M-'" comment-dwim)
    ("<M-S-backspace>" 'delete-trailing-whitespace)))

(defvar clipboard
  '(("M-x" kill-region)
    ("M-c" copy-region-as-kill)
    ("M-v" yank)
    ("M-\S-v" yank-pop)))

(defvar view-movement
  '(("M-y" scroll-down-line)
    ("M-h" scroll-up-line)
    ("M-Y" scroll-down)
    ("M-H" scroll-up)
    ("M-p" recenter-top-bottom)))

(defun marker-is-point-p (marker)
  "test if marker is at point in the current buffer"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))
(defun point-is-car-mark-ring ()
  "test if `point' is at the first marker in the mark-ring"
  (and mark-ring
       (marker-is-point-p (car mark-ring))))
(defun point-is-last-mark-ring ()
  "test if `point' is at the last marker in the mark-ring"
  (and mark-ring
       (marker-is-point-p (last mark-ring))))

;; Much of the following mark commands can be credited to www.masteringemacs.org
(defun lastcar (L) (car (last L)))

(defun marker-to-string (marker)
  (concat (number-to-string (marker-position marker)) ", "))

(defun concat-list-elems (L)
  (match-list L
    (nil "")
    ((cons x xs) (concat x (concat-list-elems xs)))))

(defun jump-to-mark (&optional arg)
  "Jumps to the local mark, respecting the `mark-ring' order.
  When called with a prefix argument, reverses the order of the `mark-ring'
  first, then jumps to local mark respecting the new order."
  (interactive "P")
  (when arg
    (message "going bkwds")
    (letrec ((cur-mark (mark-marker))
             (new-mark (lastcar mark-ring))
             (new-mark-ring
              (append (reverse (butlast mark-ring))
                      (list cur-mark))))
      (set-mark new-mark)
      (setq mark-ring new-mark-ring)))
  ;;(setq mark-ring (cons (lastcar mark-ring) (butlast mark-ring))))
  ;;(set-mark-command 1))
  (when (mark)
    (set-window-point (get-buffer-window) (mark))
    ;;(pop-mark)
    (setq mark-ring (append mark-ring (list (mark-marker))))
    (when mark-ring
      (set-mark (car mark-ring))
      (setq mark-ring (cdr mark-ring))))
  (deactivate-mark)
  (message "Mark: %d\tmark-ring: %s" (mark) (concat (mapcar 'marker-to-string mark-ring))))
;; (when mark-ring
;;   (when (marker-is-point-p (car mark-ring))
;;     (set-mark-command 1))))
;;(call-interactively 'pop-global-mark))
;;(call-interactively 'pop-global-mark))

(defun backward-mark-ring ()
  (interactive)
  (jump-to-mark 1))

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

;; TODO: Complete mark things started above

(defvar navigation ;; More complex movement
  '(("M-SPC" set-mark-command)
    ("M-:" isearch-backward)
    ("M-;" isearch-forward)))

(defvar isearch-keys
  '(("M-:" isearch-repeat-backward)
    ("M-;" isearch-repeat-forward)
    ;; TODO: C-: C-; for replace
    ))

(attach-kbds isearch-mode-map isearch-keys)

(defvar execution
  '(("M-a" execute-extended-command)
    ("M-A" eval-expression)
    ("M-]" async-shell-command)
    ("M-}" shell-command) ;; Synchronous
    ("M-|" shell)
    ("M-r" eval-last-sexp)))

(defvar all-global
  (append char-movement
          sexp-movement
          other-movement
          edit-char
          edit-other
          clipboard
          view-movement
          navigation
          execution))

(attach-kbds (current-global-map) all-global)
(global-set-key (kbd "\S-RET") 'newline-and-indent)

;; Prefix keymaps:
(defun switch-to-next-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) -1)))

(defun open-init-dot-el-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defvar buffer-keymap (make-sparse-keymap))
(attach-kbds buffer-keymap
  '(("M-j" switch-to-prev-buffer)
    ("M-l" switch-to-next-buffer)
    ("M-s" save-buffer)
    ("M-a" mark-whole-buffer)
    ("M-w" write-file)
    ("M-o" find-file)
    ("M-e" open-init-dot-el-file)
    ("M-d" kill-buffer)
    ("M-b" switch-to-buffer)))

;;(require 'frame-cmds)

(defvar window-keymap (make-sparse-keymap))
(attach-kbds window-keymap
  '(("M-j" windmove-left)
    ("M-l" windmove-right)
    ("M-i" windmove-up)
    ("M-k" windmove-down)
    ("M-o" other-window)
    ;;("M-J" split window right, then switch to left)
    ("M-L" split-window-right)
    ;;("M-I" split window below, then switch up)
    ("M-K" split-window-below)
    ("M-a" toggle-maximization)
    ("M-d" delete-window)
    ("M-1" delete-other-windows)
    ("M-y" enlarge-window)
    ("M-h" enlarge-window-horizontally)))
(global-set-key (kbd "M-b") buffer-keymap)
(global-set-key (kbd "M-t") window-keymap)

(defvar help-keymap (make-sparse-keymap))
(attach-kbds help-keymap
  '(("M-k" describe-key)
    ("M-b" describe-bindings)))
(global-set-key (kbd "M-?") help-keymap)

(global-set-key (kbd "M-SPC") 'set-mark-command)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Todo:
;; - Cycle thru kill ring
;; - Cycle thru mark ring
;; - Cycle thru buffers (make better)
;; - Better window resizing
;; - find and replace
;; ? Support prefix arguments with C-<number>? E.g. C-12 M-k to move down 12 lines
;; - UTILIZE prefix arguments!
;; ? Workspaces?
;; ? S-TAB or M-TAB for removing the indent, like M-S-f except without killing it
;; - go to line number (just M-S-< with prefix arg for number?)
;; ? shortcut to run `git pull' in .emacs.d folder
;; - HideShow seems very useful. If I can mess with it to make it remember what
;;   internal blocks have been minimized when minimizing a containing one, so
;;   that when the enclosing one is shown again, the internal hidden blocks
;;   remain hidden, that would greatly increase its usefulness to me. Also,
;;   perhaps making the hide/show commands only trigger iff the start of the
;;   block is on the current line, to prevent accidents and avoid frequent
;;   use of `move-end-of-line'.

;; Weird braindropping shit
;; - newline-with-continuity?
;;   - when in comment, continue comment and indent after comment
;;   - when not in comment, indent properly
;;   - major modes extend to work with specific language structures
;; - paste-without-context? for, e.g., you paste onto a line with a comment and
;;    all successive lines are also commented. Like paste-without-formatting in
;;    word processors
