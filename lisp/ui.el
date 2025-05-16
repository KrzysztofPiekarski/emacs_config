;; === WIZUALNE ULEPSZENIA ===
(use-package doom-themes
  :config
  (load-theme 'doom-nord t))
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(set-face-attribute 'default nil :family "FiraCode Nerd Font" :height 120)

;; === DASHBOARD ===
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Witaj w Emacsie!")
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-footer nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))

(add-hook 'dashboard-mode-hook
          (lambda ()
            (face-remap-add-relative 'default :height 0.9))) 

(provide 'ui)