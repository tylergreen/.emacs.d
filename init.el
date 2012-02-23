;; My .emacs file for EMACS 24
;; to find unbalanced parens -- go to the end of the file and type C-u C-M-u.
;; This will move you to the beginning of the first defun that is unbalanced. 

;; See local-config.el for local configurations

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

(defun load-if-exists (filename)
  (if (file-exists-p filename)
      (load-file filename)))

;*******************
; Parenthesis Matching

(electric-pair-mode t)
(show-paren-mode t)
(setq show-paren-delay 2) ; delay in seconds

;*******
; Shell Mode

(dirtrack-mode t)

;***************
; Customizations

(setq make-backup-files nil)

(defmacro disable-if-bound (fn)
  `(when (fboundp ',fn) (,fn -1)))

(mapm disable-if-bound
      (menu-bar-mode)
      (toggle-scroll-bar)
      (tool-bar-mode)
      (osx-key-mode)
      )

(defun mac-setup ()
  )

(defun linux-setup ()
  (setq x-select-enable-clipboard t)
  (disable-if-bound menu-bar-mode)
  )

(cond ((eq system-type 'darwin)
       (mac-setup))
      ((member system-type '(gnu/linux linux))
       (linux-setup)))

; put a file name '.nosearch' in directories you do not want to be loaded
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))


;;;;;;;;;;;;;;;;;
; Windowing Config 

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook
	  'ansi-color-for-comint-mode-on)

(defun make-pretty ()
;  (add-lib "color-theme-6.6.0/")
;  (add-lib "color-theme-6.6.0/themes/")
  (require 'color-theme)
  (color-theme-initialize)
  (if window-system 
      (color-theme-gnome2)
      (color-theme-calm-forest)))

(make-pretty)

(unless (transient-mark-mode)
  (transient-mark-mode))

;*****************
; Libraries

(mapc 'require
      '(cl
	ibuffer 
	ido
	tramp
	sql
	coffee-mode
	))

(ido-mode)
;****************
; Emacs Config

; don't make me type yes and no
(fset 'yes-or-no-p 'y-or-n-p)

(mapc (fn (pair) (add-to-list 'auto-mode-alist pair))
      (mkassoc '(
 		  "\\.cljs\\'" clojure-mode
		  "\\.pl\\'" prolog-mode
 		  "\\.txt\\'" auto-fill-mode
		  "\\.py\\'" python-mode
		  "\\.clj\\''" clojure-mode
		  "\\.el\\'" emacs-lisp-mode
		  "\\.yml\\'" yaml-mode
		  )))

;; classic lisp macro example
(defmacro global-keymap (&rest bindings)
  `(progn ,@(mapcar (fn (pair)
		    `(global-set-key (kbd ,(car pair)) ',(cdr pair)))
		(mkassoc bindings))))

(mapc 'global-unset-key '( "\C-_"
			  ))

(global-keymap 
 "C-w" kill-word
 "C-q" backward-kill-word
 "C-x C-k" kill-region
 "C-x k" kill-region
 "C-x C-j" kill-this-buffer
 "C-x j" kill-this-buffer
 "C-." other-frame
 "C-," previous-multiframe-window
 "C-x C-u" undo
 "C-x C-n" next-line
 "M-g" goto-line
 "M-j" shell
 "C-c C-q" quote-prev
 "M-u" upcase-prev
 "M-c" cap-prev
 "C-x C-b" ibuffer
 "M-u" windmove-up
 "M-m" windmove-down
 "M-h" windmove-left
 "M-'" windmove-right
 "M-k" zap-to-char
 "C-z" kill-ring-save
 )

(defun datahand ()
    (global-keymap
	"M-SPC" set-mark-command
	"C-;" rename-buffer
	))

(datahand)

(defun disable (commands)
  (mapc (fn (x) (put x 'disabled t))
	commands))

(disable '(upcase-region
	   downcase-region
	   ))

; *********
; Custom Commands

(defmacro defi (name &rest body)
  "define standard interactive function"
  `(defun ,name () 
     (interactive)
     ,@body))

(defi dot
  (find-file "~/.emacs.d/init.el"))

(defi bash
  (find-file "~/.bashrc"))

(defi quote-prev
  (save-excursion
    (insert "\"")
    (backward-word)
    (insert "\""))
  (forward-char))

(defi upcase-prev
  (backward-word)
  (upcase-word 1))

(defi cap-prev 
  (backward-word)
  (capitalize-word 1))

;; Customize this for you own use -- straight from emacs-fu
(setq ibuffer-saved-filter-groups
  '((("default"      
            ("Org" ;; all org-related buffers
              (mode . org-mode))  
            ("Mail"
              (or  ;; mail-related buffers
               (mode . message-mode)
               (mode . mail-mode)
               ;; etc.; all your mail related modes
               ))
            ("iovation"
              (filename . "~/clojure/iovation/"))
            ("erlang"
              (filename . "~/erlang/"))
            ("Programming" ;; prog stuff not already in MyProjectX
              (or
                (mode . c-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)
                ;; etc
                )) 
            ("ERC"   (mode . erc-mode))))))

(add-hook 'ibuffer-mode-hook
  (fn () (ibuffer-switch-to-saved-filter-groups "default")))


