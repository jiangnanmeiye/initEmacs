(package-initialize)

(global-company-mode t)
(global-linum-mode t)
(scroll-bar-mode -1)
(global-eldoc-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode t)
(delete-selection-mode t)

(require 'org)
(setq org-src-fontify-natively t)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(setq-default cursor-type 'bar)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(defun open-config-file ()
  (interactive)
  (find-file "c:/users/admin/AppData/Roaming/.emacs.d/init.el"))

(global-set-key (kbd "<f1>") 'open-config-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages (quote (company)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant italic :weight normal :height 98 :width normal)))))
