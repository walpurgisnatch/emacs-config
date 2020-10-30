(defun system-is-linux()
    (string-equal system-type "gnu/linux"))
(defun system-is-windows()
    (string-equal system-type "windows-nt"))

;; MS Windows path-variable
(when (system-is-windows)
    (setq win-sbcl-exe          "C:/sbcl/sbcl.exe")
    (setq win-init-path         "C:/.emacs.d")
    (setq win-init-ct-path      "C:/.emacs.d/plugins/color-theme")
    (setq win-init-ac-path      "C:/.emacs.d/plugins/auto-complete")
    (setq win-init-ac-dict-path "C:/.emacs.d/plugins/auto-complete/dict")
    (add-to-list 'custom-theme-load-path win-init-ct-path)
    (add-to-list 'load-path win-init-pl-path))

;; Unix path-variable
(when (system-is-linux)
    (setq unix-sbcl-bin          "/usr/bin/sbcl")
    (setq unix-init-path         "~/.emacs.d")
    (setq unix-init-ct-path      "~/.emacs.d/plugins/color-theme")
    (setq unix-init-ac-path      "~/.emacs.d/plugins/auto-complete")
    (setq unix-init-slime-path   "/usr/share/common-lisp/source/slime/")
    (setq unix-init-ac-dict-path "~/.emacs.d/plugins/auto-complete/dict")
    (add-to-list 'custom-theme-load-path unix-init-ct-path))

(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(require 'cl)
(setq-default inferior-lisp-program "sbcl")
;; Package manager:
;; Initialise package and add Melpa repository
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(defvar required-packages '(slime
                            smartparens
                            auto-complete
                            web-mode
						    js2-mode
                            lsp-mode
                            flymake
                            company-lsp
                            use-package
							omnisharp
							csharp-mode
                            vue-mode
                            emmet-mode))

(defun packages-installed-p ()
  (loop for package in required-packages
        unless (package-installed-p package)
          do (return nil)
        finally (return t)))

(unless (packages-installed-p)
  (package-refresh-contents)
  (dolist (package required-packages)
    (unless (package-installed-p package)
      (package-install package))))

(when (packages-installed-p)
  (require 'smartparens-config)
  (smartparens-global-mode)

  (require 'auto-complete-config)
  (ac-config-default)
  (global-auto-complete-mode t)
  (setq-default ac-auto-start t)
  (setq-default ac-auto-show-menu t)
  (defvar *sources* (list
                     'lisp-mode
                     'ac-source-semantic
                     'ac-source-functions
                     'ac-source-variables
                     'ac-source-dictionary
                     'ac-source-words-in-all-buffer
                     'ac-source-files-in-current-dir))
  (let (source)
    (dolist (source *sources*)
      (add-to-list 'ac-sources source)))
  (add-to-list 'ac-modes 'lisp-mode)

  (require 'slime)
  (require 'slime-autoloads)
  (slime-setup '(slime-asdf
                 slime-fancy
                 slime-indentation))
  (setq-default slime-net-coding-system 'utf-8-unix))

(setq-default lisp-body-indent 2)
(setq-default lisp-indent-function 'common-lisp-indent-function)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (cyberpunk)))
 '(custom-safe-themes
   (quote
    ("687e997f50a47c647c5132f0671df27b8a3ff4f18e31210dc53abeaa7ea8cde3" "86e74c4c42677b593d1fab0a548606e7ef740433529b40232774fbb6bc22c048" "94146ac747852749e9444b184eb1e958f0e546072f66743929a05c3af62de473" "4e839b24f87c529e837535d0a7880f40ac3867b6e3e73a2cf2bb40bab53d4658" "5adf7ad078568675387aac96e142c1300006531721bca35b941e4ed3e3b59000" default)))
 '(fci-rule-character-color "#202020")
 '(fci-rule-color "#383838")
 '(fringe-mode 4 nil (fringe))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (neotree ## skewer-mode mmm-mode company-lsp python-mode use-package vue-mode tern-auto-complete tern ac-js2 jsonnet-mode yasnippet lsp-mode auto-complete smartparens slime)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-warning ((t nil))))
 
;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top)

;;; C#

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

(defun my-csharp-mode-fn ()
  (turn-on-auto-revert-mode)
  (setq indent-tabs-mode nil)
  )

(add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "\c-n") 'company-select-next)
     (define-key company-active-map [control n] 'company-select-next)
     (define-key company-active-map (kbd "\c-p") 'company-select-previous)
     (define-key company-active-map [control p] 'company-select-previous)
     )
  )

(eval-after-load 'company
  '(add-to-list 'company-backends #'company-omnisharp))

(require 'csharp-mode)
(add-hook 'csharp-mode-hook
	  '(lambda()
	     (setq comment-column 40)
	     (setq c-basic-offset 4)
	    (flycheck-mode)
        (omnisharp-mode)
        (company-mode)
        (omnisharp-start-omnisharp-server)
	     )
	      )

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp)
  )
 
 ;; Web-mode
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(require 'web-mode)
	
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0)
  (setq web-mode-enable-current-element-highlight t)
;;  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  (set-face-attribute 'web-mode-css-rule-face nil :foreground "Pink3")
  (setq web-mode-extra-snippets
      '(("erb" . (("toto" . "<% toto | %>\n\n<% end %>")))
        ("php" . (("dowhile" . "<?php do { ?>\n\n<?php } while (|); ?>")
                  ("debug" . "<?php error_log(__LINE__); ?>")))
       ))
(setq web-mode-extra-auto-pairs '(("erb" . (("open" "close")))
                                  ))
  (setq web-mode-content-types-alist
  '(("json" . "/some/path/.*\\.api\\'")
    ("xml"  . "/other/path/.*\\.api\\'")
    ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
    )

(setq web-mode-enable-current-element-highlight t)

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook  'emmet-mode)

;;; JS

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
;(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))

;;; LSP-MODE

(require 'lsp-mode)
(use-package lsp-mode
  :commands lsp)

;; for completions
(use-package company-lsp
  :after lsp-mode
  :config (push 'company-lsp company-backends))

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))
  
(add-hook 'mmm-mode-hook
		  (lambda () 
		    (set-face-background 'mmm-default-submode-face nil)))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; (show-paren-mode t) ;; включить выделение выражений между {},[],()
;; (setq show-paren-style 'expression)

;; Electric-modes settings
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок

;; Delete selection
(delete-selection-mode t)

;; Disable GUI components
(tooltip-mode      -1)
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
;;(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
;;(blink-cursor-mode -1) ;; курсор не мигает
(setq use-dialog-box     nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; Coding-system settings
(set-language-environment 'UTF-8)
(progn
        (setq default-buffer-file-coding-system 'utf-8)
        (setq-default coding-system-for-read    'utf-8)
        (setq file-name-coding-system           'utf-8)
        (set-selection-coding-system            'utf-8)
        (set-keyboard-coding-system        'utf-8-unix)
        (set-terminal-coding-system             'utf-8)
        (prefer-coding-system                   'utf-8))
		
;; Linum plugin
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк

;; Fringe settings
(fringe-mode '(8 . 0)) ;; ограничиталь текста только слева
(setq-default indicate-empty-lines t) ;; отсутствие строки выделить глифами рядом с полосой с номером строки
(setq-default indicate-buffer-boundaries 'left) ;; индикация только слева

;; Display file size/time in mode-line
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах

;; Line wrapping
(setq word-wrap          t) ;; переносить по словам
(global-visual-line-mode t)
	
;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; отдельный список буферов при нажатии C-x C-b
(global-set-key (kbd "<f2>") 'bs-show) ;; запуск buffer selection кнопкой F2
		
;; Indent settings
(setq-default indent-tabs-mode nil) ;; возможность ставить отступы TAB'ом
(setq-default tab-width          4) ;; ширина табуляции - 4 пробельных символа
(setq-default c-basic-offset     4)
(setq-default standart-indent    4) ;; стандартная ширина отступа - 4 пробельных символа
(setq-default lisp-body-indent   4) ;; сдвигать Lisp-выражения на 4 пробельных символа
(global-set-key (kbd "RET") 'newline-and-indent) ;; при нажатии Enter перевести каретку и сделать отступ
(setq lisp-indent-function  'common-lisp-indent-function)

;; Scrolling settings
(setq scroll-margin          5) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы  
(setq scroll-step            1
      scroll-conservatively  10000
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't)

;; Short messages
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; End of file newlines
(setq require-final-newline    t) ;; добавить новую пустую строку в конец файла при сохранении
(setq next-line-add-newlines nil) ;; не добавлять новую строку в конец при смещении 
						            ;; курсора  стрелками
									
;; Highlight search resaults
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Easy transition between buffers: M-arrow-keys
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hot keys

(defvar cfg-mode-map (make-sparse-keymap))

(define-key cfg-mode-map (kbd "C-k") 'next-line)
(define-key cfg-mode-map (kbd "C-i") 'previous-line)
(define-key cfg-mode-map (kbd "C-l") 'forward-char)
(define-key cfg-mode-map (kbd "M-l") 'forward-word)
(define-key cfg-mode-map (kbd "M-j") 'backward-word)
(define-key cfg-mode-map (kbd "C-j") 'backward-char)
(define-key cfg-mode-map (kbd "C-u") 'kill-line)
(define-key cfg-mode-map (kbd "C-o") 'beginning-of-defun)
(define-key cfg-mode-map (kbd "C-p") 'end-of-defun)
(define-key cfg-mode-map (kbd "C-b") 'kill-buffer)
;(define-key cfg-mode-map (kbd "TAB") 'insert-tab-char)

(defun insert-tab-char ()
"Insert a tab char. (ASCII 9, \t)"
(interactive)
(insert "\t"))

(define-minor-mode cfg-mode
"cfg-mode"
t
" cfg"
cfg-mode-map)

(defadvice load (after cfg-keybindings-priority)
(if (not (eq (car (car minor-mode-map-alist)) 'cfg-mode))
(let ((mykeys (assq 'cfg-mode minor-mode-map-alist)))
(assq-delete-all 'cfg-mode minor-mode-map-alist)
(add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(defun turn-on-cfg-mode ()
(interactive)
(cfg-mode t))

(defun turn-off-cfg-mode ()
(interactive)
(cfg-mode -1))

(define-globalized-minor-mode global-cfg-mode cfg-mode turn-on-cfg-mode)

(defun lcl:get-hotkeys ()
(list
(list "C-x u" 'undo-tree-visualize)
...
(list "M-Z" 'undo-tree-redo)
(list "M-z" 'undo-tree-undo)))

(defun cfg:cfg-hotheys (map)
(dolist (k (lcl:get-hotkeys))
(when k
(let ((key (kbd (car k)))
(func (car (cdr k))))
(define-key map key func)
(global-set-key key func)))))

(defun cfg:cfg ()
(add-hook 'minibuffer-setup-hook 'turn-off-cfg-mode)
(cfg:cfg-hotheys cfg-mode-map)
(global-cfg-mode))