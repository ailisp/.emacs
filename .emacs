;;;; Add popkit package archive.
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; *** Add new packages here ***
(defvar my-packages
  '(material-theme
    page-break-lines
    paredit
    undo-tree
    auto-complete
    ac-slime
    tabbar-ruler
    ecb
    yasnippet))
;; Install package which is not installed
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      my-packages)

;;;; Miscellaneous for better defaults
(tool-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")
(global-auto-revert-mode 1)
(require 'saveplace)
(setq-default save-place t)
(defalias 'list-buffers 'ibuffer)
(ido-mode 1)
(setq ido-separator "\n")
(setq ido-enable-flex-matching t)
(recentf-mode 1)
(setq ring-bell-function 'ignore)
;(setq browse-url-browser-function 'eww-browse-url)
(setq backup-directory-alist (quote (("." . "~/.backups"))))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(defun unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.
URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))
;; (defadvice show-paren-function (after blink activate)
;;   (when (= ?\) (char-before (point)))
;;     (blink-matching-open)))

;;;; Package settings
;;; Set theme
(load-theme 'material t)
(require 'page-break-lines)
(global-page-break-lines-mode)

;;; The path to lisp and slime
(setq inferior-lisp-program "~/common-lisp/ccl/ccl.bat")
;(setq inferior-lisp-program "~/common-lisp/sbcl/1.3.9/sbcl")
;(setq inferior-lisp-program "~/common-lisp/clisp-2.49/clisp")
(load "~/quicklisp/slime-helper.el")
(load "~/quicklisp/clhs-use-local.el" t)

;;; Auto completion
(require 'ac-slime)
(ac-config-default)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;;; slime
(add-hook 'slime-mode-hook
	  (lambda ()
	    (unless (slime-connected-p)
	      (save-excursion (slime)))))
(setq slime-net-coding-system 'utf-8-unix)
(eval-after-load 'slime
  `(define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))

;;; Tabbar
(setq tabbar-ruler-use-mode-icons nil)
(setq tabbar-ruler-popup-scrollbar t)
(require 'tabbar-ruler)

;;; Undo and Redo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;;; Arrange buffers and navigate files and code
(require 'ecb)
(set-face-attribute 'ecb-default-highlight-face nil
                    :background "#464646")
(set-face-attribute 'ecb-tag-header-face nil
		    :background  "#008b8b")

;;; Paredit in lisp source
(require 'paredit)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

;;; Snippets
(require 'yasnippet)
(yas-global-mode 1)

;;;; For convinience
;;; Aliases
(defalias 'rr 'query-replace-regexp)

;;; Global Keybindings
(global-set-key (kbd "<f7>") 'recentf-open-files)
(global-set-key (kbd "<f8>") 'paredit-mode)
(global-set-key (kbd "C-c s") 'slime-selector)
(defun up-slightly ()
  (interactive)
  (scroll-up 1))
(defun down-slightly ()
  (interactive)
  (scroll-down 1))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "<M-right>") 'tabbar-forward-tab)
(global-set-key (kbd "<M-left>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-backspace>") 'delete-indentation)
(global-set-key (kbd "C-c m") 'mark-sexp)
(defun kill-backward-sexp ()
  (interactive)
  (backward-sexp)
  (kill-sexp))
(global-set-key (kbd "<C-M-backspace>")'kill-backward-sexp) 
;; TODO: kill/mark/copy/delete/move backward/forward sexp/char/word/line/inner-sexp

;;; Local Keybindings
;(define-key paredit-mode-map (kbd "C-]") 'paredit-close-round-and-newline)


;;;; Custiomize by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-auto-update-methods-after-save t)
 '(ecb-compilation-buffer-names
   (quote
    (("*Calculator*")
     ("*Apropos*")
     ("*Occur*")
     ("*shell*")
     ("\\*[cC]ompilation.*\\*" . t)
     ("\\*i?grep.*\\*" . t)
     ("*Help*")
     ("*eww*")
     ("\\.*w3\\.*")
     ("*Completions*")
     ("*Backtrace*")
     ("*Compile-Log*")
     ("*Messages*")
     ("*slime-events*")
     ("*inferior-lisp*"))))
 '(ecb-compilation-major-modes (quote (compilation-mode slime-repl-mode)))
 '(ecb-compile-window-height 0.3)
 '(ecb-compile-window-width (quote edit-window))
 '(ecb-display-default-dir-after-start t)
 '(ecb-enlarged-compilation-window-max-height 1.0)
 '(ecb-layout-always-operate-in-edit-window (quote (delete-other-windows switch-to-buffer)))
 '(ecb-layout-window-sizes
   (quote
    (("left8"
      (ecb-directories-buffer-name 0.1927710843373494 . 0.41379310344827586)
      (ecb-sources-buffer-name 0.1927710843373494 . 0.3448275862068966)
      (ecb-methods-buffer-name 0.1927710843373494 . 0.4482758620689655)
      (ecb-history-buffer-name 0.1927710843373494 . 0.2413793103448276)))))
 '(ecb-options-version "2.50")
 '(ecb-other-window-behavior (quote edit-and-compile))
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-select-edit-window-on-redraw t)
 '(ecb-source-path (quote ("~")))
 '(ecb-tip-of-the-day nil)
 '(ecb-windows-width 0.2)
 '(imenu-auto-rescan t)
 '(package-selected-packages
   (quote
    (flymd markdown-mode paredit undo-tree tabbar-ruler page-break-lines material-theme ac-slime)))
 '(tabbar-ruler-swap-faces t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(secondary-selection ((t (:background "steel blue"))))
 '(slime-repl-inputed-output-face ((t (:foreground "chocolate"))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :foreground "deep sky blue" :box nil :height 98 :width normal :family "Sans Serif")))))


;;;; Start up ecb and slime
(ecb-activate)
(slime)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
