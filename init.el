;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customised emacs config                          ;;
;; Author: Andrew Higginson <azhigginson@gmail.com> ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add directories to load path ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory
				(convert-standard-filename "elpa/")))
(add-to-list 'load-path (concat user-emacs-directory
				(convert-standard-filename "elpa-to-submit/")))

;;;;;;;;;;;;;;;;;;;;
;; package system ;;
;;;;;;;;;;;;;;;;;;;;
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;;;;;;;;;;;;;;;;;
;; color theme ;;
;;;;;;;;;;;;;;;;;
(when window-system
  (load "color-theme")
  (load "color-theme-twilight")
  (setq color-theme-is-global t)
  (color-theme-twilight))

;;;;;;;;;;;;;;;;;;;
;; auto complete ;;
;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; centralised backup location ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;;;;;;;;;;;;;;;;;;;;;;;
;; language specific ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; enable generic modes
;require 'generic-x)

;; enable php mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; show python whitespace
(require 'python)
(add-hook 'python-mode-hook
          (lambda () (interactive) (setq show-trailing-whitespace t)))

;; enable javascript js mode
;(autoload 'js2-mode "js2" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;;;;;;;;;;;;;;;
;; misc tweaks ;;
;;;;;;;;;;;;;;;;;
;; remove top bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; inhibit splash screen
(setq inhibit-splash-screen t)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; setup y/n shortcut
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight parentheses
(show-paren-mode 1)

;; always show line numbers
(global-linum-mode 1)

;; show column number
(column-number-mode 1)

;; scroll line by line
(setq scroll-step 1)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#330")

;; enter compressed archives transparently
(auto-compression-mode 1)

;; automatic restore of desktop
(desktop-save-mode 1)

;; Set this to whatever browser you use
;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium-browser")

;; enable cua mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; enable ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; ctrl+tab buffer switch
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'previous-buffer)

;; easy window switching
(windmove-default-keybindings 'meta)