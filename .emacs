(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install packages, if necessary
(setq package-list '(magit yaml-mode flycheck))

(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Set font to Input Mono (Narrow for OS X)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Input Mono Narrow")
  (set-face-attribute 'default nil :height 150))
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Input Mono")
  (set-face-attribute 'default nil :height 120))

;; Only set Solarized in GUI mode. It looks awful in terminal
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(when window-system
  (load-theme 'solarized t))

;; No splash screen
(setq inhibit-splash-screen t)

;; Navigate using words in multi-word identifiers
(add-hook 'c-mode-common-hook '(lambda() (subword-mode)))

;; For GUI Emacs, use Command as meta and Optional as super
(when (and (eq system-type 'darwin) window-system)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

;; Shorter goto line shortcut, we use this often
(global-set-key "\M-g" 'goto-line)

;; Rows aren't enough
(column-number-mode)

;; Nobody likes tabs
(setq-default indent-tabs-mode nil)

;; Or bells
(setq ring-bell-function 'ignore)

;; 80 character limit for C++, 100 for Java
(dolist (hook '(c++-mode-hook python-mode-hook))
  (add-hook hook '(lambda () (font-lock-set-up-width-warning 80))))
(dolist (hook '(java-mode-hook))
  (add-hook hook '(lambda () (font-lock-set-up-width-warning 100))))

;; Indent continuations four spaces rather than aligning
(add-hook 'java-mode-hook
          (lambda () (c-set-offset 'arglist-cont-nonempty '++)))

;; Centralize file backups so we don't have ~ files line around everywhere
(setq backup-by-copying t  ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.saves"))  ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)  ; use versioned backups

;; Prevent GUI from dying if ctrl-z is accidentally pressed
(if window-system (global-unset-key (kbd "C-z")))

;; Set GUI frame to 100x48 for Java files
(add-hook 'java-mode-hook
          (lambda () (if window-system
                         (set-frame-size (selected-frame) 100 48))))

;; Helpful mode for Git commits
(require 'git-commit)

;; Enable Flycheck for everything possible
(add-hook 'after-init-hook #'global-flycheck-mode)

;; We don't care about running processes when quitting (sadly deprecated)
;;(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;  (noflet ((process-list ())) ad-do-it))
