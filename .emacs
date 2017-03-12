(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (or (file-exists-p package-user-dir) (package-refresh-contents)) 
  (package-refresh-contents))

(setq package-list
      '(auto-complete
	ac-slime
	ace-jump-mode
	ace-jump-buffer
	ace-window
	material-theme
	page-break-lines
	paredit
	rainbow-delimiters
	undo-tree))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;; Miscellaneous for better defaults
(show-paren-mode 1)
(tool-bar-mode -1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq initial-scratch-message "")
(global-auto-revert-mode 1)
(require 'saveplace)
(setq-default save-place t)
;(defalias 'list-buffers 'ibuffer)
(ido-mode 1)
(setq ido-separator "\n")
(setq ido-enable-flex-matching t)
(recentf-mode 1)
(setq ring-bell-function 'ignore)
(setq browse-url-browser-function 'eww-browse-url)
(setq backup-directory-alist (quote (("." . "~/.backups"))))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil))) 
;;;; Package settings
;;; Set theme
(load-theme 'material t)
(require 'page-break-lines)
(global-page-break-lines-mode)

;;; ace-jump
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;;; ace-jump-buffer
(require 'ace-jump-buffer)
(global-set-key (kbd "C-x b") 'ace-jump-buffer)

;;; The path to sbcl and slime
;(setq inferior-lisp-program "/usr/bin/sbcl")

(load "~/quicklisp/slime-helper.el")
(load "~/quicklisp/clhs-use-local.el" t)
(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl"))
	(ecl ("/usr/bin/ecl"))
	(ccl ("/home/myfyb/bin/ccl/lx86cl64"))
	(abcl ("/home/myfyb/bin/abcl-bin-1.4.0/abcl.jar"))))
(setq slime-contribs '(slime-fancy)) ; almost everything

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

;;; Undo and Redo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;;; Paredit in lisp source
(require 'paredit)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

;;; Rainbow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;; For convinience
;;; Aliases
(defalias 'rr 'query-replace-regexp)

;;; Global Keybindings
(global-set-key (kbd "<f7>") 'recentf-open-files)
(global-set-key (kbd "<f8>") 'paredit-mode)
(global-set-key (kbd "C-c s") 'slime-selector)
(defun up-slightly ()
  (interactive)
  (scroll-up 3))
(defun down-slightly ()
  (interactive)
  (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "<C-backspace>") 'delete-indentation)
(global-set-key (kbd "C-c m") 'mark-sexp)
(defun kill-backward-sexp ()
  (interactive)
  (backward-sexp)
  (kill-sexp))
(global-set-key (kbd "<C-M-backspace>")'kill-backward-sexp) 


;;; Local Keybindings
;(define-key paredit-mode-map (kbd "C-]") 'paredit-close-round-and-newline)


(put 'downcase-region 'disabled nil)


;;;; Custiomize by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(imenu-auto-rescan t)
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-8-face ((t (:foreground "yellow"))))
 '(secondary-selection ((t (:background "steel blue"))))
 '(slime-repl-inputed-output-face ((t (:foreground "chocolate")))))
