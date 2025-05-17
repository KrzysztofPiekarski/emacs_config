;; === LSP Mode: językowy serwer podpowiedzi ===
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (js-mode . lsp)
         (java-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (yaml-mode . lsp)
         (bash-mode . lsp)
         (markdown-mode . lsp)
         (web-mode . lsp))
  :commands lsp
  :config
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil))

;; === LSP UI: dokumentacja, tooltippy, boczne podpowiedzi ===
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t))

;; === LSP Pyright: serwer języka Python ===
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

;; === Flycheck – lintowanie ===
(use-package flycheck
  :init (global-flycheck-mode))

;; === Wsparcie dla języków ===
(use-package go-mode :hook (before-save . gofmt-before-save))
(use-package web-mode :mode "\\.html?\\'")
(use-package yaml-mode :mode "\\.ya?ml\\'")
(use-package bash-completion)
(use-package haskell-mode)
(use-package clojure-mode)
(add-hook 'python-mode-hook #'font-lock-mode)
(add-hook 'python-mode-hook #'display-line-numbers-mode)

;; === Markdown ===
(use-package markdown-mode
  :mode "\\.md\\'")

(use-package markdown-preview-mode
  :after markdown-mode)

(use-package grip-mode
  :after markdown-mode)

;; === Snippety i składnia ===
(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; === POETRY, CONDA, MAMBA ===
(use-package conda
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (conda-env-autoactivate-mode t)) ; automatycznie aktywuje środowisko Conda przy otwarciu folderu projektu

;; === JUPYTER (EIN) ===
(use-package ein :defer t)

;; === dap-mode – debugowanie z LSP (Python) ===
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (setq dap-python-executable "python3"))

(provide 'editing)

