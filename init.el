;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customised emacs config                  ;;
;; Author: Andrew Higginson <me@drewzh.com> ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn on debugging (comment this out for normal use)
;(setq debug-on-error t)

;; remove top bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add directories to load path ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path user-emacs-directory)

(let ((default-directory "~/.emacs.d/elpa/"))
  (setq load-path
	(append
	 (let ((load-path (copy-sequence load-path))) ;; Shadow
	   (append 
	    (copy-sequence (normal-top-level-add-to-load-path '(".")))
	    (normal-top-level-add-subdirs-to-load-path)))
	 load-path)))

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
  ;;  (load "color-theme-twilight")
  (require 'color-theme-solarized)
  (setq color-theme-is-global t)
  (color-theme-solarized-dark))

;;;;;;;;;;;;;;;;;;;
;; auto complete ;;
;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-auto-start t)
(setq ac-delay 0)
(setq ac-auto-show-menu t)
(setq ac-show-menu-immediately-on-auto-complete t)
(setq ac-trigger-key nil)
;; fix auto completion
(ac-flyspell-workaround)

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;;;;;;;;;;;;;;
;; anything ;;
;;;;;;;;;;;;;;
(require 'anything)
(require 'anything-config)
(setq anything-sources
      (list anything-c-source-buffers
            anything-c-source-file-name-history
            anything-c-source-info-pages
            anything-c-source-man-pages
            anything-c-source-file-cache
            anything-c-source-emacs-commands))
(global-set-key (kbd "M-X") 'anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; centralised backup location ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically save and restore sessions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq desktop-path                '("~/.emacs.d/desktop")
      desktop-dirname             "~/.emacs.d/desktop"
      desktop-base-file-name      "emacs-desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;
;; language specific ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; enable php mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; show python whitespace
(require 'python)
(add-hook 'python-mode-hook
          (lambda () (interactive) (setq show-trailing-whitespace t)))

;; Colourise CSS colour literals
(autoload 'rainbow-turn-on "rainbow-mode" "Enable rainbow mode colour literal overlays")
(add-hook 'css-mode-hook 'rainbow-turn-on)
(add-hook 'html-mode-hook 'rainbow-turn-on)
(add-hook 'sass-mode-hook 'rainbow-turn-on)

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;;;;;;;;;;
;; flymake ;;
;;;;;;;;;;;;;
;; install pyflakes to check python code                                                     
(require 'flymake-cursor)
(global-set-key [f4] 'flymake-goto-next-error)

(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))

  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init))

  (defun flymake-html-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "tidy" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.html$\\|\\.ctp" flymake-html-init))
  
  (add-to-list 'flymake-err-line-patterns
               '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
                 nil 1 2 4))
  )

;;;;;;;;;;;;;;;;;;;;
;; spell checking ;;
;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p "/usr/bin/hunspell")                                         
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))


(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive arg.  For use in hooks."
  (interactive)
  (flyspell-mode 1))

;; enable for text modes
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'fundamental-mode 'turn-on-flyspell)

;; fix warning message
(setq flyspell-issue-welcome-flag nil)

;; Enable for all languages
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                clojure-mode-hook
                ruby-mode-hook
                yaml-mode
                python-mode-hook
                shell-mode-hook
                php-mode-hook
                css-mode-hook
                haskell-mode-hook
                caml-mode-hook
                nxml-mode-hook
                crontab-mode-hook
                perl-mode-hook
                tcl-mode-hook
                javascript-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

;;;;;;;;;;;;;;;;;
;; misc tweaks ;;
;;;;;;;;;;;;;;;;;
;; font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; syntax highlighting everywhere
(global-font-lock-mode t)

;; higlight changes in documents
;;(global-highlight-changes-mode t)
;;(set-face-foreground 'highlight-changes nil)
;;(set-face-background 'highlight-changes "#001B1C")
;;(set-face-foreground 'highlight-changes-delete nil)
;;(set-face-background 'highlight-changes-delete "#261515")

;; get rid of the blinking cursor
(blink-cursor-mode 0)

;; inhibit splash screen
(setq inhibit-splash-screen t)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; setup y/n shortcut
(defalias 'yes-or-no-p 'y-or-n-p)

;; tab settings
(setq c-basic-indent 2)
(setq tab-width 4)
(setq indent-tabs-mode nil)

;; highlight parentheses
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; make bell visible
(setq visible-bell t)

;; always show line numbers
(global-linum-mode 1)

;; show column number
(column-number-mode 1)

;; scroll line by line
(setq scroll-step 1)

;; highlight current line
(global-hl-line-mode 1)
;;(set-face-background 'hl-line "#330")

;; enter compressed archives transparently
(auto-compression-mode 1)

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

;; select all
(global-set-key "\C-a" 'mark-whole-buffer)

;; enable ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point nil)
(ido-mode 1)

;; ctrl+tab buffer switch
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'previous-buffer)

;; easy window switching
(windmove-default-keybindings 'meta)

;; suppress gtk dialogues
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;; Kills live buffers, leaves some emacs work buffers
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-buffers (&optional list)
  "For each buffer in LIST, kill it silently if unmodified. Otherwise ask.
LIST defaults to all existing live buffers."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (and (not (string-equal name ""))
	   (not (string-equal name "*Messages*"))
	  ;; (not (string-equal name "*Buffer List*"))
	   (not (string-equal name "*buffer-selection*"))
	   (not (string-equal name "*Shell Command Output*"))
	   (not (string-equal name "*scratch*"))
	   (/= (aref name 0) ? )
	   (if (buffer-modified-p buffer)
	       (if (yes-or-no-p
		    (format "Buffer %s has been edited. Kill? " name))
		   (kill-buffer buffer))
	     (kill-buffer buffer))))
    (setq list (cdr list))))