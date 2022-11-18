;; Initialize package sources
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

(setq inhibit-startup-message t)
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 5)        ; Give some breathing room
  (setq backup-directory-alist  '((".*" . "~/.Trash"))) ;; disable ~ files
  (menu-bar-mode -1)            ; Disable the menu bar
  (show-paren-mode)
  (electric-pair-mode)
  (global-hl-line-mode t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq visible-bell t)

;; Hide advertisement from minibuffer
(defun display-startup-echo-area-message ()
  (message ""))

(set-face-attribute 'default nil :font "Hack Nerd Font" :height 100)

(use-package general												
    :config													
    (general-evil-setup t))											

(nvmap :keymaps 'override :prefix "SPC"									
       "SPC"   '(counsel-M-x :which-key "M-x")								
       "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config")
       "."     '(find-file :which-key "Find file")								
       "f r"   '(counsel-recentf :which-key "Recent files")							
       "f f"   '(find-file :which-key "Find file")								
       "t"  '(:ignore t :which-key "toggles")								
       "tt" '(counsel-load-theme :which-key "choose theme")							
       )

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

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package doom-modeline											
   :ensure t													
   :init (doom-modeline-mode 1))										
  (setq doom-modeline-major-mode-icon nil)									

(use-package all-the-icons)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package counsel												
   :bind (("M-x" . counsel-M-x)										        
	  ("C-x b" . counsel-ibuffer)										
	  ("C-x C-f" . counsel-find-file)									
	  :map minibuffer-local-map										
	  ("C-r" . 'counsel-minibuffer-history)))								

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

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package page-break-lines)

(use-package dashboard											
  :init      ;; tweak dashboard config before loading it							
  (setq dashboard-set-heading-icons t)									
  (setq dashboard-set-file-icons t)										
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")					
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner				
  (setq dashboard-startup-banner "~/.emacs.d/logo.png")  ;; use custom image as banner			
  (setq dashboard-center-content nil) ;; set to 't' for centered content					
  (setq dashboard-items '((recents . 5)									
                          (agenda . 5 )									
                          (bookmarks . 3)									
                          (projects . 3)									
                          (registers . 3)))									
  (setq dashboard-page-separator "\n\f\n")    ;; <-----							
  :config													
  (dashboard-setup-startup-hook)										
  (dashboard-modify-heading-icons '((recents . "file-text")							
 			     (bookmarks . "book"))))

(use-package org
  :config
  (setq org-ellipsis " "))

(use-package org-bullets											
  :after org													
  :hook (org-mode . org-bullets-mode)									
  :custom													
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(defun cec/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . cec/org-mode-visual-fill))

;; Automatically tangle our Emacs.org config file when we save it
(defun cec/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cec/org-babel-tangle-config)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
