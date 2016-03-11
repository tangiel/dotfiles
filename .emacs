(require 'package)
;; SSL doesn't work for Emacs on Windows
(if (eq system-type 'windows-nt)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'local-pre-hooks nil t)

;; Install packages, if necessary
(defvar package-list
  '(company
    company-go
    company-jedi
    flycheck
    flycheck-google-cpplint
    go-mode
    google-c-style
    magit
    protobuf-mode
    py-autopep8
    yaml-mode))

(defun packages-installed-p (package-list)
  (if (not package-list) t
    (and (package-installed-p (car package-list))
         (packages-installed-p (cdr package-list)))))

(unless (packages-installed-p package-list)
  (condition-case err
      (progn
        (package-refresh-contents)
        (dolist (package package-list)
          (unless (package-installed-p package)
            (package-install package))))
    (error (message "Package install failed: %s" (error-message-string err)))))

;; Set font to Input Mono (Narrow for OS X)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Input Mono Narrow")
  (set-face-attribute 'default nil :height 150))
(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :family "Input Mono")
  (set-face-attribute 'default nil :height 120))
(when (eq system-type 'windows-nt)
  (set-face-attribute 'default nil :family "InputMono")
  (set-face-attribute 'default nil :height 120))

;; Set Solarized to light in the GUI and dark in the terminal. The terminal
;; needs to have ANSI colors set correctly, or this will look awful. For some
;; reason, the frame hook doesn't set the initial frame, so we do an initial
;; set ourselves. For Windows, use dark as well, since it looks "better."
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(defun solarized-light-p (&optional frame)
  (and (display-graphic-p frame) (not (eq system-type 'windows-nt))))

(when (member 'solarized (custom-available-themes))
  (let ((mode (if (solarized-light-p) 'light 'dark)))
    (add-to-list 'default-frame-alist '(background-mode . mode))
    (set-frame-parameter nil 'background-mode mode)
    (set-terminal-parameter nil 'background-mode mode))
  (load-theme 'solarized t))

;; No splash screen
(setq inhibit-splash-screen t)

;; Navigate using words in multi-word identifiers
(add-hook 'prog-mode-hook '(lambda() (subword-mode)))

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
(defun font-lock-set-up-width-warning (width)
  "Make text beyond column WIDTH appear in `font-lock-warning-face'."
  (require 'font-lock)
  (font-lock-mode 1)
  (make-local-variable 'font-lock-keywords)
  (font-lock-add-keywords
   nil
   `((,(format "^.\\{%d\\}\\(.+\\)" width)
      1 font-lock-warning-face t))))

(dolist (hook '(c++-mode-hook python-mode-hook))
  (add-hook hook '(lambda () (font-lock-set-up-width-warning 80))))
(dolist (hook '(java-mode-hook))
  (add-hook hook '(lambda () (font-lock-set-up-width-warning 100))))

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
(require 'git-commit nil t)

;; Enable Flycheck for everything possible
(when (require 'flycheck nil t)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc python-flake8))

  (when (require 'flycheck-google-cpplint nil t)
    (flycheck-add-next-checker 'c/c++-clang
                               'c/c++-googlelint 'append)))

;; Lilypond mode when installed (not in MELPA, as of now)
(when (require 'lilypond-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode)))

;; Google C style
(when (require 'google-c-style nil t)
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent))

;; Go support
(require 'go-mode-load nil t)

;; Enable company for all programming modes
(when (require 'company nil t)
  (add-hook 'prog-mode-hook 'global-company-mode)
  (setq-default company-idle-delay 0.2)
  (when (require 'company-jedi nil t)
    (add-to-list 'company-backends 'company-jedi))
  (when (require 'company-go nil t)
    (add-to-list 'company-backends 'company-go)))

;; To be like OS X, bind a hotkey to switch frames
(global-set-key "\M-`" 'other-frame)

;; Set auto-fill-mode at 80 columns when editing Markdown
(add-hook 'markdown-mode-hook
          (lambda()
            (progn
              (setq fill-column 80)
              (auto-fill-mode))))

(require 'local-post-hooks nil t)

;; We don't care about running processes when quitting (sadly deprecated)
;;(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;;  (noflet ((process-list ())) ad-do-it))
