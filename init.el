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
(set-face-background 'hl-line "white") ;; change with the color that you like                                    
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
(which-key-setup-minibuffer)
(selectrum-mode +1)
;; -- Smart parens --
;; For automatic completion of brackets
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(amx-mode t)
 '(custom-safe-themes
   '("660376e0336bb04fae2dcf73ab6a1fe946ccea82b25f6800d51977e3a16de1b9" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "9e39a8334e0e476157bfdb8e42e1cea43fad02c9ec7c0dbd5498cf02b9adeaf1" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "45e76a1b1e3bd74adb03192bf8d6eea2e469a1cf6f60088b99d57f1374d77a04" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" default))
 '(eaf-find-alternate-file-in-dired t nil nil "Customized with use-package eaf")
 '(fci-rule-color "#888395")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
	 ("NEXT" . "#dc752f")
	 ("THEM" . "#2d9574")
	 ("PROG" . "#4f97d7")
	 ("OKAY" . "#4f97d7")
	 ("DONT" . "#f2241f")
	 ("FAIL" . "#f2241f")
	 ("DONE" . "#86dc2f")
	 ("NOTE" . "#b1951d")
	 ("KLUDGE" . "#b1951d")
	 ("HACK" . "#b1951d")
	 ("TEMP" . "#b1951d")
	 ("FIXME" . "#dc752f")
	 ("XXX+" . "#dc752f")
	 ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#222228" "#819cd6"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#222228" "#5b94ab"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#222228" "#515462"))
 '(line-number-mode nil)
 '(objed-cursor-color "#e1c1ee")
 '(package-selected-packages
   '(doom-modeline sml-modeline @ smartparens nov yasnippet-classic-snippets windresize which-key vterm use-package spacemacs-theme spaceline-all-the-icons selectrum restart-emacs neotree lsp-ui lsp-jedi jedi ivy ir-black-theme iedit go-complete go-autocomplete flycheck exec-path-from-shell elpy doom-themes dired-k diminish dashboard dark-mint-theme company-lua company-lsp company-jedi company-go autopair auto-virtualenvwrapper auto-virtualenv auto-complete-c-headers atom-one-dark-theme atom-dark-theme amx ample-theme))
 '(pdf-view-midnight-colors (cons "#c6c6c6" "#282b33"))
 '(rustic-ansi-faces
   ["#282b33" "#e1c1ee" "#5b94ab" "#cfcf9c" "#819cd6" "#a6c1e0" "#7289bc" "#c6c6c6"])
 '(tetris-x-colors
   [[229 192 123]
	[97 175 239]
	[209 154 102]
	[224 108 117]
	[152 195 121]
	[198 120 221]
	[86 182 194]])
 '(vc-annotate-background "#282b33")
 '(vc-annotate-color-map
   (list
	(cons 20 "#5b94ab")
	(cons 40 "#81a7a6")
	(cons 60 "#a8bba1")
	(cons 80 "#cfcf9c")
	(cons 100 "#c1cab2")
	(cons 120 "#b3c5c9")
	(cons 140 "#a6c1e0")
	(cons 160 "#a6c1e0")
	(cons 180 "#a6c1e0")
	(cons 200 "#a6c1e0")
	(cons 220 "#b9c1e4")
	(cons 240 "#cdc1e9")
	(cons 260 "#e1c1ee")
	(cons 280 "#bda5cb")
	(cons 300 "#998aa8")
	(cons 320 "#756f85")
	(cons 340 "#888395")
	(cons 360 "#888395")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
