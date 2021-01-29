;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ;;("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq required-package-list '(use-package))

(dolist (package required-package-list)
   (unless (package-installed-p package)
     (package-install package)))    

;; use-package
(require 'use-package)
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A E S T H E T I C S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn off the splash
(setq inhibit-startup-message t)

;; get rid of UI elements
(menu-bar-mode -1)
(tool-bar-mode -1) 
(scroll-bar-mode -1)

;; fringe?
(set-fringe-mode 10)

;; induce seizures
(setq visible-bell t)

;; color-theme 🧛
(use-package dracula-theme)

;; matched delimiters become rainbows
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ;; add line numbers, but disable for various shells
;; (global-display-line-numbers-mode t)
;; (dolist (mode '(org-mode-hook
;; 		term-mode-hook
;; 		eshell-mode-hook))
;;   (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add column numbers to lower buffer
(column-number-mode)

;; switch between buffers quickly
(global-set-key (kbd "C-c C-<left>")  'windmove-left)
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<up>")    'windmove-up)
(global-set-key (kbd "C-c C-<down>")  'windmove-down)

;; use multi-term
(use-package multi-term)

;; follow symlinks to version-controlled files
(setq vc-follow-symlinks t)

;; open Ibuffer in current buffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; what are keys even (in the current mode)?
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idel-delay 1))

;; counsel
(use-package counsel)

;; magit
(use-package magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'latex-mode-hook  'flyspell-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)

;; Change default TeX compiler to pdflatex
(setq latex-run-command "pdflatex")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set elpy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elpy)
(elpy-enable)
(setenv "CONDA_PREFIX" (concat (getenv "HOME") "/miniconda3"))
(setenv "WORKON_HOME"  (concat (getenv "CONDA_PREFIX") "/envs"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Julia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package julia-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indentation
(setq js-indent-level 2)

;; (use-package js2-mode
;;   :hook (js-mode . js2-minor-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arduino
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck)
(use-package arduino-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Org-Mode Bullets
;;(require 'org-bullets)
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

(setq org-ellipsis " ▾" ;;▼
      org-hide-emphasis-markers t)

;; custom org-mode todo-keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")
	(sequence "CANCELED(c)")
	(sequence "BLOCKED(b)")))

;; custom font-faces for org-mode todo-keywords
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("IN-PROGRESS" . "#1589FF")
	("BLOCKED"  . (:foreground "yellow" : weight bold))
        ("CANCELED" . (:foreground "#778899" :weight bold))))

;; languages that org-babel understands
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (js . t)
   (haskell . t)
   (R . t)))		

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Chef
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-chef
  :ensure t)

(setq org-capture-templates
      '(("c" "Cookbook" entry (file "~/org/cookbook.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
        ("m" "Manual Cookbook" entry (file "~/org/cookbook.org")
         "* %^{Recipe title: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n :author:\n :END:\n** Description\n   %?\n\n** Ingredients\n   %?\n** Directions\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use evil mode ?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'evil)
;; (evil-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Added by Custom...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit flycheck arduino-mode counsel which-key js2-mode elpy markdown-mode use-package rainbow-delimiters multi-term dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
