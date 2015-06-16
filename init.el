;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;      My .emacs.d file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "* --[ Loading my Emacs init file ]--")

(setq default-directory "~/" )


(set-face-background 'mode-line "black")
(set-face-background 'modeline-inactive "grey20")

(rainbow-delimiters-mode t)

;; Elisp
;; Replace last elisp expression with result
(defun replace-last-sexp ()
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(global-set-key (kbd "C-c C-x C-e") 'replace-last-sexp)


;; Packages
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Evil mode
;; (require 'evil)
;; (evil-mode 1)

;; OS specific settings
(cond
 ;; Windows Settings
 ((string-equal system-type "windows-nt")
  (progn
    (message "Microsoft Windows")
    (menu-bar-mode 0)                 ; Remove menu bar
    ))

 ;; Mac OS X Settings
 ((string-equal system-type "darwin")
  (progn
    (message "Mac OS X")

    ;; Switches command and alt keys
    (setq mac-option-modifier 'meta)
    (setq mac-command-modifier 'meta)

    ;; Loads the shell variables from $PATH
    (exec-path-from-shell-initialize)

    ;; Magit status
    (global-set-key (kbd "C-c C-m") 'magit-status)

    (menu-bar-mode 1)                 ; Menu bar is okay in OSX.
    (setq ring-bell-function 'ignore) ; No ringing bell on error))

    ;; Default font
    (set-face-attribute 'default nil :font  "DejaVu Sans Mono-12")
    ))
 )

;; Python mode
(elpy-enable)
(add-hook 'python-mode-hook
          (lambda ()
            'jedi:setup
            (setq hs-minor-mode t)
            (auto-complete-mode t)
            ))

;; Code Folding
(global-set-key (kbd "C-M--") 'hs-hide-block)
(global-set-key (kbd "C-M-=") 'hs-show-block)


(require 'ess-site)


;; Color parenthesis
(require 'rainbow-delimiters)
(rainbow-delimiters-mode t)


;; Add directory of file in the mode line
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
               '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)


;; -------------------------------------------------------------------
;;    Operating System Tweaks
;; -------------------------------------------------------------------



(global-set-key (kbd "C-c s") 'eshell)

(require 'ace-jump-mode)
(global-set-key (kbd "C-c u") 'ace-jump-mode)


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


;; -------------------------------------------------------------------
;;    Visual Tweaks
;; -------------------------------------------------------------------
(setq frame-title-format "%b - Emacs")  ; Show file name in title bar

(column-number-mode 1)                  ; Col number in mode line
(line-number-mode 1)                    ; Row number
(blink-cursor-mode 0)                   ; Cursor blink

(setq visible-bell t)                   ; Stop audio ding when error

(scroll-bar-mode 0)                     ; remove scroll bar
(tool-bar-mode 0)                       ; Remove tool bar




;; Ido mode for better selection UI when switching buffers, opening files, etc
;; (require 'ido)
;; (ido-mode t)
;; (ido-vertical-mode 1)
;; (setq ido-enable-flex-matching 1)
;; (setq ido-show-dot-for-dired 1)
;; (icomplete-mode)                        ; Shows autocomplete in minibuffer

;; Helm
;;(add-to-list 'load-path "~/.emacs.d/stuff")
;;(require 'my-helm-stuff)                ; Helm customizations
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


(global-set-key (kbd "C-x C-b") 'ibuffer) ; Better buffer switcher
;; ;;(setq ibuffer-use-other-window t)         ; open ibuffer in another window


;; Multiple cursors like in sublime text
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Search and replace with regular expressions.
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)  ;; multiple-cursors?


;; When yanking (pasting) something, region will be highlighted
(require 'volatile-highlights)
(volatile-highlights-mode t)


;; -------------------------------------------------------------------
;;  Changing Emacs defaults
;; -------------------------------------------------------------------

;; Unbind some default keys
(global-unset-key "\C-xf")                ; Default is to set fill-column
(define-key global-map (kbd "C-z") 'undo) ;  Default to minimize. Change to undo

(global-hl-line-mode 1)               ; Highlight current line
(global-auto-revert-mode 1)           ; Refreshes buffer if file changes on disk
(global-font-lock-mode 1)             ; syntax highlighting
(transient-mark-mode 1)               ; sane select (mark) mode
(delete-selection-mode 1)             ; Entry deletes marked text
(show-paren-mode 1)                   ; Highlights matching parenthesis
(setq show-paren-delay 0)             ; highlight parentheses immediately

(setq-default fill-column 80)           ; Set default fill-column to 80
(setq-default indent-tabs-mode nil)     ; Uses spaces as tabs
(setq-default tab-width 4)              ; Default tab width
(setq undo-limit 100000)                ; Increase number of undo
(setq initial-scratch-message "")       ; Empty scratch buffer
(setq inhibit-startup-message t)        ; No splash screen


(setq scroll-conservatively most-positive-fixnum) ; Scroll more smoothly with keyboard
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ; one line at a time for mouse scrolling
(setq mouse-wheel-progressive-speed nil)            ; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse



;; C-a moves to first character of line. Then C-a again will move to beginning of line.
(defun prelude-move-beginning-of-line (arg)
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

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)


;; -------------------------------------------------------------------
;;    Buffer and Window stuff
;; -------------------------------------------------------------------
;; Cycles open buffers through the opened windows
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows) 1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

;; Toggle horizontal/vertical window split
(defun toggle-window-split ()
  "If only 2 windows, then toggles between horizontal and vertical split"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-|") 'toggle-window-split) ; Hotkey for window split



;; -------------------------------------------------------------------
;;    Navigation
;; -------------------------------------------------------------------
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


;; Better way to switch between buffers when there are > 2 windows
(require 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)

;; Define a function to move backwards
(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value n))))

(global-set-key "\C-x\C-n" 'other-window)
(global-set-key "\C-x\C-p" 'other-window-backward)



;; -------------------------------------------------------------------
;;    Misc. Tweaks
;; -------------------------------------------------------------------

;; Line numbers in programming modes
;; (require 'nlinum)
;; (add-hook 'prog-mode-hook 'nlinum-mode)


;; Cleans up whitespace only. Will not indent buffer
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))


;; Cleans up whitesapce and will fix indentions
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

;; Cleans up whitespace when saving
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; Cleans up whitespace and fix indentation
(global-set-key (kbd "C-c n") 'cleanup-buffer)


;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; -------------------------------------------------------------------
;;    Org Mode
;; -------------------------------------------------------------------
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (auto-fill-mode t)          ; Line wrap
            (nlinum-mode 0)             ; No line numbers
            (local-set-key (kbd "M-n") 'outline-next-visible-heading)
            (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
            ))

;(setq org-startup-indented t)
;(setq org-hide-leading-stars t)         ; Hide leading stars
;(setq org-adapt-indentation nil)        ; Fix indentation

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-log-done t)

(setq org-agenda-files (list "~/Dropbox/Org/home.org"
                             "~/Dropbox/Org/work2.org"))

(setq org-default-notes-file "~/Dropbox/Org/notes.org")



;; Enable R and LaTeX code
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (latex . t)))

(setq org-confirm-babel-evaluate nil)   ; Disable asking for confirm each time


;; R Stuff
;;(require 'ess-site)                     ; emacs speaks statistics
(setq ess-ask-for-ess-directory nil)

(add-hook 'ess-mode-hook
          '(lambda()
             (auto-complete-mode 1)     ; Autocomplete R commands
             ))

;; SQL Stuff
(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-oracle-keywords)
            (electric-indent-mode 1)
            (auto-complete-mode 1)
            ))

;; Magit status hotkey
;; (require 'magit)
;; (global-set-key (kbd "C-c C-m") 'magit-status)



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit t :background "#3E4444")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6a9606327ecca6e772fba6ef46137d129e6d1888dcfc65d0b9b27a7a00a4af20" "da7fa7211dd96fcf77398451e3f43052558f01b20eb8bee9ac0fd88627e11e22" "ee0bd3866d6eeaf2799531e10d5a0d6006a8c917eee75d769427a79ffdacfe4d" default)))
 '(fci-rule-color "#383838")
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
