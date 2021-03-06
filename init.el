(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"."http://elpa.emacs-china.org/gnu/")
		      ("melpa"."http://elpa.emacs-china.org/melpa/"))))

;; cl - Common Lisp Extension
(require 'cl)

;; Add Packages
(defvar my/packages '(
		;; --- Auto-completion ---
		company
		;; --- Better Editor ---
		hungry-delete		
		swiper
		counsel
		smartparens
		paredit
		sr-speedbar
		ecb
		;; --- Major Mode ---
		;; After install Haskell Platform
		;; intero
		irony
		js2-mode
		;; --- Minor Mode ---
		nodejs-repl
		;; solarized-theme
		) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

(unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

;;other-window
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

(global-set-key (kbd "<f1>") 'open-config-file)

;;ecb speedbar
(require 'ecb)
(require 'speedbar)
(setq ecb-windows-width 30)
(setq speedbar-show-unknown-files t)
(setq speedbar-use-images nil)
(setq sr-speedbar-width 30)
(setq sr-speedbar-auto-refresh t)
(setq sr-speedbar-right-side t)
(setq speedbar-tag-hierarchy-method nil)
;;active or deactivate ecb
(global-set-key (kbd "<f7>") 'ecb-activate)
(global-set-key (kbd "<f8>") 'ecb-deactivate)
(global-set-key (kbd "<f3>") 'sr-speedbar-toggle)
;;show or hide ecb window
(define-key global-map [(control f1)] 'ecb-show-ecb-windows)
(define-key global-map [(control f2)] 'ecb-hide-ecb-windows)

(require 'org)
(require 'hungry-delete)
(require 'smartparens-config)
(require 'nodejs-repl)
(require 'recentf)
(setq org-src-fontify-natively t)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq initial-frame-alist (quote ((fullscreen . maximized))))
(setq-default cursor-type 'bar)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;Auto-company brackets
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;;Always start smartparens mode in emacs-mode
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

;;Always start smartparens mode in haskell-mode
;;(add-hook 'haskell-mode-hook 'smartparens-mode)

;;hook mode for C/C++
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun open-config-file ()
  (interactive)
  (find-file "c:/users/koal/AppData/Roaming/.emacs.d/init.el"))

(global-company-mode t)
(global-linum-mode t)
(global-hungry-delete-mode t)
(scroll-bar-mode -1)
(global-eldoc-mode -1)
(tool-bar-mode -1)
(delete-selection-mode t)
;;(intero-global-mode 1)

;;Magit
(global-set-key (kbd "C-x g") 'magit-status)

;;Swiper binding
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

;;config js2-mode for js files
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

;;;;;;;;;;;;
;;Copy the configuration about Scheme from 'http://yinwang.org'
;;;;;;;;;;;;
(require 'cmuscheme)
(setq scheme-program-name "scheme") ;;If use Petite, change the variable scheme-program-name to "petite"

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name)))
  (or (scheme-get-process)
      (error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    (split-window-horizontally (floor (* 0.68 (window-width))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (find "*scheme*"
               (mapcar (lambda (w) (buffer-name (window-buffer w)))
                       (window-list))
               :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f5>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-definition-split-window)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1)
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
    (sr-speedbar ecb rainbow-delimiters rainbow-blocks intero scala-mode paredit company hungry-delete swiper counsel smartparens js2-mode nodejs-repl)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier New" :foundry "outline" :slant italic :weight bold :height 98 :width normal))))
 '(cursor ((t (:background "snow"))))
 '(ecb-analyse-face ((t (:inherit gray :background "magenta"))))
 '(js2-external-variable ((t (:foreground "dim gray")))))
