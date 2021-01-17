;;Straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(toggle-scroll-bar -1)
;; Package Setup and things.
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://stable.melpa.org/packages/")
                         ("melpa_untable" . "http://melpa.org/packages/")))

(set-face-attribute 'default nil :height 80)
;; for clipboard, this allows contro shift C and control shift v to copy paste stuffs
(setq select-enable-clipboard t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;for line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x _") 'split-window-below)
(global-set-key (kbd "C-x \\") 'delete-window)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1 to 26 default to 8
(setq sml/no-confirm-load-theme t)
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

(setq c-default-style "linux"
          c-basic-offset 4)
;;(global-hl-line-mode t) ;; To enable
;;(set-face-background 'hl-line "black") ;; change with the color that you like
                                       
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    )
  )
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(package-initialize)
(selectrum-mode +1)
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
(setq neo-window-width 30)
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

(load-theme 'doom-tomorrow-night t)
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
;; -- Space line -- 
(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  (setq powerline-height 20)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))
;; Neotree
(global-set-key (kbd "C-c C-t") 'neotree-toggle)
(eaf-setq eaf-browser-dark-mode "false")
(eaf-setq eaf-terminal-dark-mode "follow")
(eaf-setq eaf-mindmap-dark-mode "follow") ; default option
(eaf-setq eaf-pdf-dark-mode "false") 

