;; Package manager
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
				("elpa" . "http://tromey.com/elpa/")
			      	("melpa" . "http://melpa.milkbox.net/packages/")
))
(add-to-list 'package-archives source t))
(package-initialize)

(load-theme 'zenburn)

;; headers in c++ mode..
; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Auto-indent
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4)
(add-hook 'c-mode-common-hook (lambda()(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)))

(setq-default indent-tabs-mode nil)

;; a vertical line at col 80
(add-hook 'c-mode-hook (lambda () (interactive) (column-marker-1 80)))

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

(require 'ac-math) 
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

 (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
   (setq ac-sources
         (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                 ac-sources))
   )
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(global-auto-complete-mode t)
 
(setq ac-math-unicode-in-math-p t)

;; Emacs Code Browser
(require 'ecb)
(require 'ecb-autoloads)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "a53714de04cd4fdb92ed711ae479f6a1d7d5f093880bfd161467c3f589725453" default)))
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; Webdev things
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
