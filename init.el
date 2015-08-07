;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      My .emacs.d file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package multiple-cursors
  :ensure multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(use-package avy
  :ensure avy)
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)


(use-package zenburn-theme
  :ensure zenburn-theme)
(load-theme 'zenburn t)

(use-package powerline
  :ensure powerline)
(powerline-default-theme)


;; Evil mode
;; (require 'evil)
;; (evil-mode 1)

;; Add directory of file in the mode line
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))

(add-hook 'find-file-hook 'add-mode-line-dirtrack)


(defun move-to-beginning-of-line (arg)
  "Move to first character of line. Again to move to beginning of line."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'move-to-beginning-of-line)


;; Move around quicker when using the bfnp keys with Shift
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))


;; SQL MODE
(add-hook 'sql-mode-hook
         (lambda()
           (auto-complete-mode)
           ))


;; Change some Emacs default behaviour
(setq default-directory "~/" )
(setq frame-title-format "%b - Emacs")  ; Show file name in title bar

(column-number-mode 1)                  ; Col number in mode line
(line-number-mode 1)                    ; Row number
(blink-cursor-mode 0)                   ; Cursor blink

(menu-bar-mode 0)                       ; Remove menu bar
(scroll-bar-mode 0)                     ; Remove scroll bar
(tool-bar-mode 0)                       ; Remove tool bar

(global-unset-key "\C-xf")              ; Default is to set fill-column

(global-hl-line-mode 1)                 ; Highlight current line
(global-auto-revert-mode 1)             ; Refreshes buffer if file changes 
(global-font-lock-mode 1)               ; syntax highlighting
(transient-mark-mode 1)                 ; sane select (mark) mode
(delete-selection-mode 1)               ; Entry deletes marked text
(show-paren-mode 1)                     ; Highlights matching parenthesis
(setq show-paren-delay 0)               ; highlight parentheses immediately

(setq-default fill-column 80)           ; Set default fill-column to 80
(setq-default indent-tabs-mode nil)     ; Uses spaces as tabs
(setq-default tab-width 4)              ; Default tab width
(setq undo-limit 100000)                ; Increase number of undo
(setq initial-scratch-message "")       ; Empty scratch buffer
(setq inhibit-startup-message t)        ; No splash screen


(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ; one line at a time for mouse scrolling
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq scroll-conservatively most-positive-fixnum)   ; Scroll more smoothly with keyboard


;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))



;; Ido Mode
(use-package ido-vertical-mode
  :ensure ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode t)
(setq ido-enable-flex-matching 1)
(setq ido-show-dot-for-dired 1)
(icomplete-mode)                        ; Shows autocomplete in minibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer) ; Better buffer switcher


(setq ibuffer-use-other-window t)         ; open ibuffer in another window



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
