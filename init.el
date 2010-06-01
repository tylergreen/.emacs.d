;; My .emacs file

;; to find unbalanced parens -- go to the end of the file and type C-u C-M-u.
;; This will move you to the beginning of the first defun that is unbalanced. 

;; run multiple subshells -- rename shell buffer using m-x rename-uniquely, then 
;; M-j to start new shell as normal

; /opt/local/share/22.3/lisp

;*****************
; Elisp Utils

(defun group (n source)
  (if (endp source)
      nil
    (let ((rest (nthcdr n source)))
      (cons (if (consp rest) (subseq source 0 n ) source)
	    (group n rest)))))

(defun mkassoc (binds)
  (if (endp binds)
      nil
    (cons (cons (car binds) (cadr binds))
	  (mkassoc (nthcdr 2 binds)))))

(defmacro fn (params &rest body)
  `(lambda ,params ,@body))

; the need for this highlights a big weakness of lisp macros 
(defmacro mapm (macro &rest defs)
  "apply a macro to each list in DEFN"
  `(progn ,@(mapcar (fn (x) (cons macro x)) defs)))

; REMEMBER: nconc-- last cdr of each of the lists is changed to
; refer to the following list.
; The last of the lists is not altered

;*******************
; Environments

(defmacro disable-if-bound (fn)
  `(when (fboundp ',fn) (,fn -1)))

(mapm disable-if-bound
      (toggle-scroll-bar)
;       (menu-bar-mode) 
       (tool-bar-mode)
    )

; Windowing Config 

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook
	  'ansi-color-for-comint-mode-on)

(defun mac-setup ()
  (setq HOME "/Users/jorge/"
	CS "/Users/jorge/cs/"
	;setq  mac-command-modifier 'meta
	;ispell-program-name "aspell"
	)
  )

(defun linux-setup ()
  (disable-if-bound menu-bar-mode)
  )

(cond ((eq system-type 'darwin)
       (mac-setup))
      ((member system-type '(gnu/linux linux))
       (linux-setup)))

(defun in-cs (extension) (concat CS extension))

;*****************
; Libraries

;(require 'magit)
; paredit-- matching delimiter tool   
(add-to-list 'load-path "/Users/jorge/.emacs.d/")

;(autoload 'paredit-mode "paredit"
;  "Minor mode for pseudo-structurally editing lisp code"
;  t)

;(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))

(mapc 'require
      '(cl
	paren
	))

;; My emacs commands

(add-to-list 'load-path "~/cs/mython/sandbox")
(require 'mython-mode)

;**************
; Shortcuts

(defun mylog ()
  (interactive)
  (find-file (in-cs "/log/log.txt")))

;****************
; Emacs Config

; don't make me type yes and no
(fset 'yes-or-no-p 'y-or-n-p)

(setq auto-mode-alist
      (append
       (mkassoc '("\\.pl\\'" prolog-mode
 		  "\\.txt\\'" auto-fill-mode
		  "\\.my\\'" mython-mode
		  "\\.py\\'" python-mode
		  ))
       auto-mode-alist))

; the mapc approach has many weakness...
; (mapc (fn (bind) (global-set-key (car bind) (cadr bind)))
;                   (mkasso ...))
(mapm global-set-key
      ("\C-x\C-m" 'execute-extended-command)
      ("\C-c\C-m" 'execute-extended-command)
      ("\C-xm" 'execute-extended-command)
      ("\C-w" 'kill-word)
      ("\C-q" 'backward-kill-word)
      ("\C-x\C-k" 'kill-region)
      ("\C-xk" 'kill-region)
      ("\C-x\C-j" 'kill-this-buffer)
      ((kbd "C-.") 'other-frame)
      ((kbd "C-,") 'previous-multiframe-window)
      ("\C-x\C-u" 'undo)
      ("\C-x\C-n" 'next-line)
      ("\M-g" 'goto-line)
      ("\M-j" 'shell)
      ("\C-cf" 'run-factor)
      ("\C-c\C-q" 'quote-prev) 
      ("\M-u" 'upcase-prev)
      )

(defun disable (commands)
  (mapc (fn (x) (put x 'disabled t))
	commands))

(disable '(upcase-region
	   downcase-region))

; *********
; Custom Commands

(defun reload-emacs ()
  (interactive)
  (load-file (concat HOME ".emacs.d/init.el")))

(defun quote-prev ()
  (interactive)
  (save-excursion
    (insert "\"")
    (backward-word)
    (insert "\""))
  (forward-char))

(defun upcase-prev ()
  (interactive)
  (backward-word)
  (upcase-word 1))

(defun my-html-mode-hook ()
  (define-key html-mode-map (kbd "C-c C-;") 'sgml-close-tag))

(add-hook 'html-mode-hook 'my-html-mode-hook)

; don't work
(defun untabify ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;**************
; Factor Setting

(defun use-factor ()
  (setq fbase (in-cs "factor/"))
					; fuel
  (load-file (concat fbase "misc/fuel/fu.el"))
  (setq fuel-listener-factor-binary (concat fbase "factor")
	fuel-listener-factor-image (concat fbase "factor.image"))

  ;; custom fuel keys
;  (define-key fuel-mode-map (kbd "C-c p") 'fuel-eval-definition)
;  (define-key fuel-mode-map (kbd "C-c u") 'fuel-show-callers)
;  (define-key fuel-mode-map (kbd "C-c o") 'fuel-show-callees)
;  (define-key fuel-mode-map (kbd "C-c i") 'fuel-refactor-inline-word)
  )

(use-factor)

;----------------
; gnu smalltalk

(defun use-smalltalk ()
  (autoload 'smalltalk-mode
    (in-cs "st-src/smalltalk-mode.el")
    "" t)

  (push '("\\.st\\'" . smalltalk-mode)
	auto-mode-alist))

(use-smalltalk)

; *********
; slime

(defun use-slime ()
  (global-set-key "\C-cd" 'slime-eval-defun)

  ; load slime:
  (nconc load-path
	 (mapcar 'in-cs
		 '("cl/clbuild/source/slime/contrib"
		   "cl/clbuild/source/slime")))

  (setq slime-backend
	(in-cs "cl/clbuild/.swank-loader.lisp") ;;file contains swank-loader
	inhibit-splash-screen t
	inferior-lisp-program (in-cs "cl/clbuild/clbuild --implementation sbcl lisp")
	slime-use-autodoc-mode nil)

  (load (in-cs "cl/clbuild/source/slime/slime"))
 
  (slime-require :swank-listener-hooks)
  (slime-setup '(slime-fancy slime-tramp slime-asdf))
  )

(use-slime)

;***************
; clojure-slime

(defun use-clojure ()
; clojure-mode
  (add-to-list 'load-path "~/cs/clojure/clojure-mode")

  (require 'clojure-mode)

; swank-clojure
  (add-to-list 'load-path "~/cs/clojure/swank-clojure/src/emacs")

  (setq swank-clojure-jar-path "~/cs/clojure/clojure-core/clojure.jar"
	swank-clojure-extra-classpaths
	(list "~/cs/clojure/swank-clojure/src/main/clojure"
	      "~/cs/clojure/clojure-contrib/clojure-contrib.jar"))

  (require 'swank-clojure-autoload)

  (add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))
  )

(use-clojure)

; M-- M-x slime ;; prompts which lisp to use, sbcl or clojure
; slime-quit-lisp to close

;**************
; Zen Coding

(defun use-zen ()
  (add-to-list 'load-path "~/cs/emacs/zencoding/")
  (require 'zencoding-mode)
  (add-hook 'sgml-mode-hook 'zencoding-mode)
;  (define-key zencoding-mode-keymap  "\C-c\C-e" 'zencoding-expand-line)
)

;(use-zen)
  

;; ;; Erlang Section
(defun use-erlang ()
  (setq erlang-root-dir "/opt/local/lib/erlang")
  (add-to-list 'load-path "/opt/local/lib/erlang/lib/tools-2.6.5.1/emacs")
  (add-to-list 'exec-path "/opt/local/lib/erlang/bin")
  (require 'erlang-start) 
  )

(use-erlang)

(defun use-distel ()
;; This is needed for Distel setup
  (let ((distel-dir "/Users/jorge/cs/erlang/distel/elisp"))
    (unless (member distel-dir load-path)
    ;; Add distel-dir to the end of load-path
      (add-to-list 'load-path distel-dir)))

  (require 'distel)
  (distel-setup)
  
  (add-hook 'erlang-mode-hook
	    (lambda ()
	      ;; when starting an Erlang shell in Emacs, default in the node name
	      (setq inferior-erlang-machine-options '("-sname" "emacs"))
	      ;; add Erlang functions to an imenu menu
	      (imenu-add-to-menubar "imenu")))

  ;; A number of the erlang-extended-mode key bindings are useful in the shell too
  (defconst distel-shell-keys
    '(("\C-\M-i"   erl-complete)
      ("\M-?"      erl-complete)	
      ("\M-."      erl-find-source-under-point)
      ("\M-,"      erl-find-source-unwind) 
      ("\M-*"      erl-find-source-unwind) 
      )
    "Additional keys to bind when in Erlang shell.")

  (add-hook 'erlang-shell-mode-hook
	    (lambda ()
      ;; add some Distel bindings to the Erlang shell
	      (dolist (spec distel-shell-keys)
		(define-key erlang-shell-mode-map (car spec) (cadr spec)))))
  )

(use-distel)

;; End Erlang
