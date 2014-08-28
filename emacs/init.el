; -*- mode: Lisp;-*-

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Interactively do things
(require 'ido)

;; Force load of .el to list
(add-to-list 'auto-mode-alist '("\\.el\\'" . lisp-mode))

;; Configure before loading org mode (package-initialize)  
(package-initialize)

  ;; Setup MELPA package repo
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Org-mode code block delimiters 
;; Before loading org-mode
;(defface org-block-begin-line
;;;;;  '((t (:underline "#efef8f" :foreground "#efef8f" :background "#efef8f")))
;  '((t (:underline "#A7A6AA" :foreground "#cc9393" :background "#383838")))
;  "Face used for the line delimiting the begin of source blocks.")
;
;(defface org-block-background
;  '((t (:background "#383838")))
;  "Face used for the source block background.")
;
;(defface org-block-end-line
;  '((t (:overline "#A7A6AA" :foreground "#cc9393" :background "#383838")))
;  "Face used for the line delimiting the end of source blocks.")

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ledger . t)
   (octave . t)
   (latex . t)
   (sh . t)
   (gnuplot . t)
   (emacs-lisp . t)   
   ))

;; Fix path to work with Anaconda
(defun set-exec-path-from-shell-PATH ()
        (interactive)
        (let ((path-from-shell (replace-regexp-in-string "^.*\n.*shell\n" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


  ;; Setup latex export
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))

;; Activate agenda key
(define-key global-map "\C-ca" 'org-agenda)

;;(setq org-agenda-files (list "~/org/TODO.org"
;;                             "~/org/PhDPlanner.org" 
;;                             "~/org/Article2.org" 
;;                             "~/org/Article3.org"))

  ;; Allow images to imbed in Org-mode
(defun do-org-show-all-inline-images ()
  (interactive)
  (org-display-inline-images t t))
(global-set-key (kbd "C-c C-x C v")
                'do-org-show-all-inline-images)


;; Enforce 80 line column mode
(add-hook 'c-mode-hook 'column-enforce-mode)
(add-hook 'python-mode-hook 'column-enforce-mode)

;; taskjuggler plugin
(require 'ox-taskjuggler)

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; Emacs setup
(let ((default-directory "~/ownCloud/.configs/.emacs/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Enable PDFLaTeX as default
(setq TeX-PDF-mode t)

;; Load auctex
(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)

;; Load RefTeX
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

;; Load Color Theme
(require 'color-theme)
(load-theme 'zenburn t)
;;(color-theme-initialize)
;;(color-theme-charcoal-black)


;; Load AUCTeX
;;setq reftex-default-bibliography (quote ("/home/christian/PhD/Library.bib")))
;;(setq reftex-bibpath-environment-variables '("/home/christian/PhD/Library.bib"))

;; Load Zotexo (Zotero-minor-mode)
;; Requies MozRelp installed
(require 'zotexo)
(add-hook 'TeX-mode-hook 'zotexo-minor-mode)

;; Autoload
(autoload 'cc-mode "cc-mode" "C/C++ Mode." t)
(autoload 'matlab-mode "matlab-mode" "Matlab Editing Mode" t)

;; Setup modes
(setq auto-mode-alist 
      (append
       '(("\\.c"   . c-mode)
	 ("\\.cpp" . c-mode)
	 ("\\.h"   . c-mode)
	 ("\\.cu"  . cuda-mode)
	 ("\\.m"  . matlab-mode)) 
       auto-mode-alist)
)

;; Moz-repl Integration
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;; matlab-mode
(setq matlab-indent-function t)
(setq matlab-shell-command "matlab")

(setq org-todo-keywords
       '((sequence "TODO" "IN PROGRESS" "|" "DONE" "CANCELED")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "/usr/local/texlive/2014/bin/x86_64-linux/latex")
 '(custom-safe-themes (quote ("75c9f0b0499ecdd0c856939a5de052742d85af81814e84faa666522c2bba7e85" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" default)))
 '(epg-gpg-home-directory nil)
 '(exec-path (quote ("/usr/lib/lightdm/lightdm" "/usr/local/sbin" "/usr/local/bin" "/usr/sbin" "/usr/bin" "/sbin" "/bin" "/usr/games" "/usr/local/Anaconda/2.0.1/bin/")))
 '(latex-run-command "/usr/local/texlive/2014/bin/x86_64-linux/latex")
 '(org-agenda-files (quote ("~/PhD/Org/Artikle4.org" "~/Documents/org/Server-notes.org" "~/PhD/Org/Home.org" "~/PhD/Org/Teaching.org" "~/PhD/Org/Article2.org" "~/PhD/Org/Article3.org" "~/PhD/Org/TODO.org" "~/PhD/Org/Notes.org")))
 '(org-babel-python-command "/usr/local/Anaconda/2.0.1/bin/python")
 '(org-babel-python-mode (quote /usr/local/Anaconda/2\.0\.1/bin/ipython))
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(org-taskjuggler-default-project-duration 600)
 '(org-taskjuggler-default-reports (quote ("include \"reports.tji\"")))
 '(python-shell-exec-path (quote ("nil")))
 '(tramp-default-method "ssh"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

