;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      My .emacs.d file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)




;; Python - ELPY
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(use-package elpy
  :ensure elpy)
(elpy-enable)

(add-hook 'python-mode-hook
          (lambda()
            (hs-minor-mode t)
            (local-set-key [C-tab] 'hs-toggle-hiding)
            ))
          




;; Theme to use
(use-package zenburn-theme
  :ensure zenburn-theme)
(load-theme 'zenburn t)


(use-package auto-complete
  :ensure auto-complete)


(use-package rainbow-delimiters
  :ensure rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Evil mode
;; (use-package eviil
;;   :ensure evil)
;; (evil-mode 1)


;; Multiple cursors like in Sublime text
(use-package multiple-cursors
  :ensure multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Jump to any word in buffer quickly
(use-package ace-jump-mode
  :ensure ace-jump-mode)
(global-set-key (kbd "C-c j") 'ace-jump-word-mode)


;; display current and total match when in search mode
(use-package anzu
  :ensure anzu)
(global-anzu-mode 1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package powerline
  :ensure powerline)
(powerline-default-theme)

(defface powerline-custom1
  '((t (:background "#EEAD0E" :foreground "black" :weight bold)))
  "Custom face for bright sections"
  :group 'powerline)

(defface powerline-custom2
  '((t (:foreground "#EEAD0E" :weight bold)))
  "Custom face for text"
  :group 'powerline)

(defun my/anzu-update-func (here total)
  "Customizing how anzu displays HERE & TOTAL on the mode line."
  (propertize (format " <%d/%d>" here total)
              'face 'powerline-custom1))
(setq anzu-mode-line-update-function 'my/anzu-update-func)


(defun powerline-spacemacs-imitation-theme ()
  "An attempt to imitate the spacemacs powerline theme."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face3 (if active 'powerline-custom1 mode-line))
                          (face4 (if active 'powerline-custom2 mode-line))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" face3 'l)
                                     (powerline-raw " " face3)
                                     (funcall separator-left face3 mode-line)
                                     
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info face4 'l))
                                     (powerline-buffer-id face4 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-right face1 mode-line)
                                     
                                     (powerline-minor-modes mode-line 'l)
                                     (powerline-narrow mode-line 'l)
                                     (powerline-raw " " mode-line)
                                     (funcall separator-left mode-line face1)
                                     
                                     (powerline-vc face1 'r)
                                     (powerline-raw " " face1)
                                     (funcall separator-right face1 face2)
                                     
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face1 'l))
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-spacemacs-imitation-theme)


;; Add directory of file in the mode line
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))

(add-hook 'find-file-hook 'add-mode-line-dirtrack)



;; -----------------------------------------------------------------------------
;; Navigation tweaks
;; -----------------------------------------------------------------------------

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
(fset 'copy-sql-table-name
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item (quote ([67108896 18 32 6 134217847] 0 "%d")) arg)))

(add-hook 'sql-mode-hook
          (lambda()
            (auto-complete-mode)
            (sql-highlight-oracle-keywords)
            (local-set-key (kbd "C-c t") 'copy-sql-table-name)
            ))


;; Change some Emacs default behaviour
(setq default-directory "~/" )
(setq frame-title-format "%b")  ; Show file name in title bar

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
(setq undo-limit 10000)                 ; Increase number of undo
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
(use-package ido-ubiquitous
  :ensure ido-ubiquitous)

(ido-mode t)

(setq ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-show-dot-for-dired 1)

(ido-vertical-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(add-hook 'ido-setup-hook
          '(lambda()
             (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
             (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
             (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match)
             ))

(icomplete-mode)                        ; Shows autocomplete in minibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer) ; Better buffer switcher
(setq ibuffer-use-other-window t)         ; open ibuffer in another window



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("a444b2e10bedc64e4c7f312a737271f9a2f2542c67caa13b04d525196562bf38" "d1dbb3c37e11ae8f986ca2d4b6a9d78bb1915fe66f3a6ffab1397cc746c18cba" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "b06aaf5cefc4043ba018ca497a9414141341cb5a2152db84a9a80020d35644d1" default)))
 '(fci-rule-color "#424242")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
