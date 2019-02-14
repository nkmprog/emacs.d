(require 'package)

(add-to-list 'package-archives
             '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/" ) t)

(package-initialize)

(setq
 backup-by-copying t                             ; don't clobber symlinks
 backup-directory-alist '(("." . "~/.saves"))    ; don't litter my fs tree
 version-control t
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 5
 doc-view-continuous t)                          ; doc-view continuous scroll

(line-number-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(require 'use-package)

(use-package erlang
  :ensure t
  :config
  (add-hook 'erlang-mode-hook '(lambda()
                                 (setq indent-tabs-mode nil)
                                 (add-to-list 'ac-modes 'erlang-mode))))

(use-package projectile
  :ensure t)

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-format-hook (lambda ()
                                 (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                   (setq elixir-format-arguments nil))))
  (add-hook 'elixir-mode-hook
           '(lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package alchemist
  :ensure t)

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :init
  (customize-set-variable 'git-gutter:added-sign "++")
  (customize-set-variable 'git-gutter:deleted-sign "--")
  (customize-set-variable 'git-gutter:modified-sign "  ")
  :config
  (global-git-gutter-mode t)
  (global-git-gutter-mode +1))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package prolog
  :config
  ;; setup files ending with .pl to open in prolog-mode
  (setq auto-mode-alist (cons '("\\.pl$" . prolog-mode) auto-mode-alist)))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'erlang-mode-hook 'flycheck-mode)
  (add-hook 'elixir-mode-hook 'flycheck-mode))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:modified-sign "  ")
 '(package-selected-packages
   (quote
    (projectile zenburn-theme use-package solarized-theme smartparens scheme-here racket-mode markdown-preview-mode magit ledger-mode git-gutter flycheck erlang drag-stuff diminish atom-one-dark-theme alchemist))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
