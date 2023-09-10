(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar

(setq backup-directory-alist  '((".*" . "~/.Trash"))) ;; disable ~ files

(show-paren-mode)
(electric-pair-mode)

(global-hl-line-mode t)

(setq visible-bell t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil												
  :init													
  (setq evil-want-integration t)										
  (setq evil-want-keybinding nil)										
  (setq evil-want-C-u-scroll t)										
  (setq evil-want-C-i-jump nil)										
  (setq evil-vsplit-window-right t)										
  (setq evil-split-window-below t)										
  :config													
  (evil-mode 1)												
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)						
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)				

  ;; Use visual line motions even outside of visual-line-mode buffers					        
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)							
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)						        

  (evil-set-initial-state 'messages-buffer-mode 'normal)							
  (evil-set-initial-state 'dashboard-mode 'normal))								

(use-package evil-collection											
  :after evil												        
  :config													
  (evil-collection-init))

(use-package general
        :config
        (general-evil-setup)

   ;;setup 'SPC' as the leader key
    (general-create-definer cd/leader-keys
        :states '(normal insert visual emacs)
        :keymaps 'override
        :prefix "SPC"
        :global-prefix "M-SPC")
    (cd/leader-keys
        "b" '(:ignore t :wk "buffer")
        "SPC" '(counsel-M-x :wk "M-x")
        "f c" '((lambda () (interactive) (find-file "~/.emacs.d/config.org")) :wk "Open Emacs Config")
        "TAB TAB" '(comment-line :wk "Comment line"))
    (cd/leader-keys
        "b" '(:ignore t :wk "buffer")
        "b b" '(switch-to-buffer :wk "Switch buffer")
        "b k" '(kill-this-buffer :wk "Kill this buffer")
        "b n" '(next-buffer :wk "Next-buffer")
        "b p" '(previous-buffer :wk "Previous-buffer")
        "b i" '(ibuffer :wk "Ibuffer")
        "b r" '(revert-buffer :wk "Reload buffer"))
    (cd/leader-keys
        "f" '(:ignore t :wk "files")
        "f f" '(find-file :wk "Find file")
        "f r" '(recentf :wk "Find recent files")
        "." '(find-file :wk "Find files"))
    (cd/leader-keys
      "e" '(:ignore t :wk "Evaluate")    
      "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
      "e d" '(eval-defun :wk "Evaluate defun containing or after point")
      "e e" '(eval-expression :wk "Evaluate and elisp expression")
      "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
      "e r" '(eval-region :wk "Evaluate elisp in region")) 
    (cd/leader-keys
        "t" '(:ignore t :wk "theme")
        "t t" '(visual-line-mode :wk "Toggle truncated lines")
        "t l" '(display-line-numbers-mode :wk "Toggle line number"))
    (cd/leader-keys
        "h" '(:ignore t :wk "Help")
        "h f" '(describe-function :wk "Describe function")
        "h v" '(describe-variable :wk "Describe variable")
        "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :wk "Reload emacs config"))
      ;;"h r r" '(reload-init-file :wk "Reload emacs config"))
 )

(set-face-attribute 'default nil
  :font "JetBrains Mono"
  :height 90
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 100
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 90
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-10"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(column-number-mode)
(display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(global-visual-line-mode t)

(use-package doom-themes
  :init (load-theme 'doom-tokyo-night t))

(use-package all-the-icons)

;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))
;;(setq doom-modeline-major-mode-icon nil)

(use-package which-key
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit t
	  which-key-separator " → " ))

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(use-package counsel
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
