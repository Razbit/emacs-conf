;; Package manager
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
				("elpa" . "http://tromey.com/elpa/")
			      	("melpa" . "http://melpa.milkbox.net/packages/")
))
(add-to-list 'package-archives source t))
(package-initialize)

(require 'cc-mode)

;; Auto-indent
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4)
(add-hook 'c-mode-common-hook (lambda()(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))

(setq-default indent-tabs-mode nil)

;; a vertical line at col 80
(add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 72)))

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Autopair
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'autopair)
(autopair-global-mode)

(load "auctex.el" nil t t)

(require 'yasnippet)
(yas-global-mode 1)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(global-auto-complete-mode t)

;; Emacs Code Browser
(require 'ecb)
(require 'ecb-autoloads)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("769bb56fb9fd7e73459dcdbbfbae1f13e734cdde3cf82f06a067439568cdaa95" default)))
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'darkburn)

;; Always visible compile window
(setq ecb-compile-window-height 12)

;; layout
(setq ecb-layout-name "left3")

;;; activate and deactivate ecb 
(global-set-key (kbd "C-x ;") 'ecb-activate)
(global-set-key (kbd "C-x '") 'ecb-deactivate) 

;;; show/hide ecb window
(global-set-key (kbd "C-c ;") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-c '") 'ecb-hide-ecb-windows)

;;; quick navigation between ecb windows 
(global-set-key (kbd "C-c 1") 'ecb-goto-window-edit1) 
(global-set-key (kbd "C-c 2") 'ecb-goto-window-directories)
(global-set-key (kbd "C-c 3") 'ecb-goto-window-sources)
(global-set-key (kbd "C-c 4") 'ecb-goto-window-methods)
(global-set-key (kbd "C-c 5") 'ecb-goto-window-compilation)

;; Disassemble C
(require 'disaster)
(define-key c-mode-base-map (kbd "C-c d") 'disaster)
