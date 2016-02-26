;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My .emacs.d file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; This package lets you auto install a package if you dont have it already
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)


;; Theme to use
;; (use-package zenburn-theme
;;   :ensure zenburn-theme)
;; (load-theme 'zenburn t)

(use-package color-theme-sanityinc-tomorrow
  :ensure color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)


;; Autocomplete in programming languages
(use-package auto-complete
  :ensure auto-complete)


;; Jump to any word in buffer quickly
(use-package ace-jump-mode
  :ensure ace-jump-mode)
(global-set-key (kbd "C-c j") 'ace-jump-word-mode)

;; Multiple cursors like in Sublime Text
(use-package multiple-cursors
  :ensure multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(use-package git-gutter+
  :ensure git-gutter+)
(global-git-gutter+-mode)


;; Font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10:bold"))

;; -----------------------------------------------------------------------------
;; Change some Emacs default behaviour
;; -----------------------------------------------------------------------------

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

(setq undo-limit 1000)                  ; Increase number of undo

;; Start up
(setq initial-scratch-message "")       ; Empty scratch buffer
(setq inhibit-startup-message t)        ; No splash screen

;; Tab behaviour
(setq-default indent-tabs-mode nil)     ; Uses spaces as tabs
(setq-default tab-width 4)              ; Default tab width

(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ; one line at a time for mouse scrolling
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq scroll-conservatively most-positive-fixnum)   ; Scroll more smoothly with keyboard


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




;; -----------------------------------------------------------------------------
;; SQL Mode
;; -----------------------------------------------------------------------------
(add-hook 'sql-mode-hook
          (lambda()
            (auto-complete-mode)
            (sql-highlight-oracle-keywords)
            (local-set-key (kbd "C-c t") 'copy-sql-table-name)
            ))





(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(fci-rule-color "#515151")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
