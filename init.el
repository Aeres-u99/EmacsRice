;; Package Setup and things.
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://stable.melpa.org/packages/")
                         ("melpa_untable" . "http://melpa.org/packages/")))

(set-face-attribute 'default nil :height 80)
;; for clipboard, this allows contro shift C and control shift v to copy paste stuffs
(setq select-enable-clipboard t)
;; -- Appearance Setting
(setq-default cursor-type 'bar)
(set-cursor-color "#ffffff") 
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)
;; Setting up font
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font Mono-9" ))
(set-face-attribute 'default t :font "FiraCode Nerd Font Mono-9" )

;;for line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "C-x \\") 'delete-window)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8
;; (setq sml/no-confirm-load-theme t)
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

(setq c-default-style "linux"
          c-basic-offset 4)
(global-hl-line-mode t) ;; To enable
(set-face-background 'hl-line "black") ;; change with the color that you like
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    )
  )
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(package-initialize)
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(selectrum-mode +1)
;; -- Smart parens --
;; For automatic completion of brackets
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)
;; -- C / C++ things
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(require 'yasnippet)
(yas-global-mode 1)
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/lib/gcc/x86_64-pc-linux-gnu/10.2.0/include-fixed"))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)

;; -- Done -- 
(elpy-enable)
(use-package all-the-icons)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-window-width 25)
(use-package lsp-ui)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional
(message "juicing eaf ...")
(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
    (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
    (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
    (eaf-bind-key take_photo "p" eaf-camera-keybinding))
(message "loading secrets ...")
(load-file "~/.emacs.d/secret.el")
(message "loading theme ...")


;; -- Python 3 VirtualEnv
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place

;; (load-theme 'darkmine t)
(load-theme 'modus-vivendi t)
(message "juicing golang ...")
;;-- GOLANG pewpew!
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "/home/akuma/go/src/")
(add-to-list 'exec-path "/home/akuma/go/src/bin")
(add-hook 'before-save-hook 'gofmt-before-save)
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)
(with-eval-after-load 'go-mode
   (require 'go-autocomplete))
;;--
;; -- Scheme PewPew --
(require 'geiser)

(setq geiser-active-implementations '(mit))

(defun geiser-save ()
  (interactive)
  (geiser-repl--write-input-ring))
;; --- *** ---

;; -- Space line -- 
;;(use-package spaceline
;;  :demand t
;;  :init
;;  (setq powerline-default-separator 'arrow-fade)
;;  (setq powerline-height 20)
;;  :config
;;  (require 'spaceline-config)
;;  (spaceline-spacemacs-theme))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Or use this
;; Use `window-setup-hook' if the right segment is displayed incorrectly
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
;; Neotree
(global-set-key (kbd "C-c C-t") 'neotree-toggle)
;; EAF related Settings
(eaf-setq eaf-browser-dark-mode "false")
(eaf-setq eaf-terminal-dark-mode "follow")
(eaf-setq eaf-mindmap-dark-mode "follow") ; default option
(eaf-setq eaf-pdf-dark-mode "false") 
;; -- ORG Mode --
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(require 'pomodoro) 
(pomodoro-add-to-mode-line)
(setq org-agenda-files (list "~/orgwiki/work/work.org"
                             "~/orgwiki/hacking/hacking.org" 
                             "~/orgwiki/programming/programming.org"
							 "~/orgwiki/studies/studies.org"
							 "~/orgwiki/research/research.org"
							 "~/orgwiki/DailyTodo.org"))
(use-package org-bullets
:ensure t
:init
(setq org-bullets-bullet-list
'("◉" "◎" "⚫" "○" "►" "◇"))
:config
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
(sequence "⚑ WAITING(w)" "|")
(sequence "|" "✘ CANCELED(c)")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d2db4af7153c5d44cb7a67318891e2692b8bf5ddd70f47ee7a1b2d03ad25fcd9" "a10ca93d065921865932b9d7afae98362ce3c347f43cb0266d025d70bec57af1" default))
 '(package-selected-packages
   '(ivy-posframe modus-themes pomodoro yasnippet-classic-snippets windresize vterm use-package spacemacs-theme spaceline-all-the-icons sml-modeline smartparens selectrum restart-emacs posframe nov neotree lsp-ui lsp-jedi jedi ivy ir-black-theme iedit helm go-complete go-autocomplete geiser fontawesome flycheck exec-path-from-shell elpy doom-themes doom-modeline dired-k diminish dashboard darkmine-theme dark-mint-theme company-lua company-lsp company-jedi company-go bm avy autopair auto-virtualenvwrapper auto-virtualenv auto-complete-c-headers atom-one-dark-theme atom-dark-theme amx ample-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
