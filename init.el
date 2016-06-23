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
(global-subword-mode 1)
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))
(tool-bar-mode -1)
(menu-bar-mode 1) ;; Still feels weird without it...

;; Set font to Consolas
(set-face-attribute 'default nil :font "Consolas")
(set-frame-font "Consolas" nil t)

;; Indent java argument lists with 4 spaces on next line
(add-hook 'java-mode-hook '(lambda () (c-set-offset 'arglist-intro '+)))

;; Consider: (require 'undo-tree)...? Just to try.

;; Processing
(setq processing-location "~/processing-3.1.1/processing-java.exe")
(setq processing-application-dir "~/processing-3.1.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Melpa:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives
;;             '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

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
            clojure-mode
            cider-mode)))

;; Because Windows Emacs has problems with SSL connections:
(defvar gnutls-trustfiles '("C:\\Users\\Wilson\\Downloads\\gnutls\\certificate-also-necessary\\cacert.pem"))

(defun my-update-packages ()
  (condition-case nil
      (init--install-packages)
    (error
     (condition-case nil
         (progn (package-refresh-contents)
                (init--install-packages))
       (error
        (message "Error installing packages"))))))

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

;; Org-mode:
(require 'org)

;; (defvar org-keys
;;   '(;; Overlay my bindings over org's:
;;     ("M-h" scroll-up-line)
;;     ("M-a" execute-extended-command)
;;     ("M-e" kill-word)
;;     ;;("C-a" org-mark-element)
;;     ;;("C-(" outline-previous-visible-heading)
;;     ;;("C-)" outline-next-visible-heading)
;;     ;;("C-i" org-shiftup)
;;     ;;("C-k" org-shiftdown)
;;     ;;("C-j" org-backward-sentence)
;;     ;;("C-l" org-forward-sentence)
;;     ))

;; (add-hook 'org-mode-hook
;;   (lambda ()
;;     (attach-kbds org-mode-map org-keys)))
;; Allow alphabetic list-labels (e.g. A) ... B) ... etc.)
(setq org-mode-list-allow-alphabetical t)

;; Set number of newlines allowed in a markup expression (default is just 1)
(setcar (nthcdr 4 org-emphasis-regexp-components) 10)

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


(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(require 'evil)
(evil-mode)

;; To modify word movement and deletion:
;;(require 'subword-mode)
;; (defun char-stx-type (ch) (char-to-string (char-syntax ch)))
;; (defun point-stx () (char-stx-type (char-after (point))))
;; (defun fwd-word ()
;;   (interactive)
;;   ;; Skip whitespace:
;;   (while (/= (point-stx) "-") (forward-char))
