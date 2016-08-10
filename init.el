;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My .emacs.d file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme
    ess
    elpy
    auto-complete
    ace-jump-mode
    multiple-cursors
    git-gutter
    neotree))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)


(add-to-list 'auto-mode-alist
             '("\\.sas\\'" . (lambda ()
                               (require 'ess-site)
                               (sas-mode))))

(require 'better-defaults)


;; Jump to any word in buffer quickly
(global-set-key (kbd "C-c j") 'ace-jump-word-mode)


;; Multiple cursors like in Sublime Text
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Tree directory
(global-set-key [f8] 'neotree-toggle)

;; Font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))

;; Python
(elpy-enable)


;; yafolding
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))


(add-hook 'prog-mode-hook
          (lambda ()
            (auto-complete-mode)
            (yafolding-mode)
            (git-gutter-mode)
            ;; (git-gutter:linum-setup)
            ;; (linum-mode)
            ))


(setq
 backup-directory-alist '(("." . "~/.saves")) 
 backup-by-copying t      ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq auto-save-default nil)

;; -----------------------------------------------------------------------------
;; Change some Emacs default behaviour
;; -----------------------------------------------------------------------------

(setq frame-title-format "%b")         ; Show file name in title bar
(setq initial-scratch-message "")      ; Empty scratch buffer
(setq inhibit-startup-message t)       ; No splash screen
(setq default-directory "~/" )         ; Set default directory to HOME
(setq undo-limit 10000)                ; Increase number of undo

(blink-cursor-mode 0)                  ; Remove cursor blink
(column-number-mode 1)                 ; Col number in mode line
(line-number-mode 1)                   ; Row number
(global-hl-line-mode 0)                ; Highlight current line
(global-auto-revert-mode 1)            ; Refreshes buffer if file changes
(delete-selection-mode 1)               ; Entry deletes marked text

;; Tab behaviour
(setq-default indent-tabs-mode nil)     ; Uses spaces as tabs
(setq-default tab-width 4)              ; Default tab width

;; Scrolling behaviour
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
(ido-mode t)
(ido-vertical-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; (setq ido-enable-flex-matching t
;;       ido-enable-prefix nil
;;       ido-use-filename-at-point nil
;;       ido-max-prospects 10
;;       ido-show-dot-for-dired 1)

(add-hook 'ido-setup-hook
          '(lambda()
             (define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
             (define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match)
             ))



(icomplete-mode)                        ; Shows autocomplete in minibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer) ; Better buffer switcher
;; (setq ibuffer-use-other-window t)         ; open ibuffer in another window


;; -----------------------------------------------------------------------------
;; SQL Mode
;; -----------------------------------------------------------------------------
(add-hook 'sql-mode-hook
          (lambda()
            (auto-complete-mode)
            (sql-highlight-oracle-keywords)
            ))


(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)
;; (add-hook 'before-save-hook 'cleanup-buffer-safe) ; Cleans up whitespace when saving


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
 '(custom-enabled-themes (quote (material)))
 '(custom-safe-themes
   (quote
    ("e56ee322c8907feab796a1fb808ceadaab5caba5494a50ee83a13091d5b1a10c" "b0ab5c9172ea02fba36b974bbd93bc26e9d26f379c9a29b84903c666a5fde837" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-yasnippet elpy-module-sane-defaults)))
 '(fci-rule-color "#515151")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (material-theme fill-column-indicator git-gutter neotree fringe-helper web-mode zenburn-theme yafolding use-package nlinum multiple-cursors markdown-mode magit ido-vertical-mode ido-ubiquitous evil ess elpy ein color-theme-sanityinc-tomorrow auto-complete ace-jump-mode)))
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
