;; Temporary hack until I find a way to get .Xresources to work again
(set-default-font "Dejavu Sans Mono 10")
;; the command M-x top-level aborts all levels of recursive edits,
;; returning immediately to the top-level command reader.

;; M-x find-name-dired: prompt for a root directory and a filename pattern.
;; Press t to "toggle mark" for all files found.
;; Press Q for "Query-Replace in Files...":
;; prompted for query/substitution regexps.

;; C-x ^      -- Enlarge window
;; C-x }      -- Enlarge horizontally
;; C-x {      -- Shrink horizontally
;; C-x -      -- Shrink to fit buffer

;; (global-set-key "\C-x\C-q" 'toggle-read-only)
;; C-x C-e    -- eval-last-sexp
;; (setq ring-bell-function (lambda ()
;;                        (call-process "ratpoison" nil nil nil "-c"
;;                        "echo ding")))
;;
;; unicode, utf-8, character encoding:
;; You can change the encoding to use for the file when saving using
;; 'C-x C-m f'.  You can also force this immediately by using 'C-x C-m
;; c <encoding> RET C-x C-w RET'.  You can force Emacs to read a file
;; in a specific encoding with 'C-x RET c C-x C-f'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load default.el first.
(load "default" t t)
(setq inhibit-default-init t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-path
(add-to-list 'load-path "~/lib/emacs/")
(add-to-list 'load-path "~/usr/src/distel/elisp/")
(add-to-list 'load-path "~/usr/src/magit/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name and E-mail
(setq user-full-name "Daniel Luna")
(setq user-mail-address "daniel@lunas.se")
(setq message-user-organization nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global keybindings
(setq ctrl-z-map (make-keymap))
(global-set-key "\C-z" ctrl-z-map)
(global-set-key "\C-x\C-z" 'suspend-emacs)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\M-g" 'goto-line)  ; Goto line number
(global-set-key "\C-h" 'backward-delete-char-untabify)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)
(global-set-key "\C-zl" 'load-file)
(global-set-key "\C-zh" 'help)
(global-set-key "\C-xO" 'previous-multiframe-window)
(global-set-key "\C-zc" 'compile)
;; (setq compile-command "cd ../../../ && ./rebar compile")
(setq compile-command "cd ../../../ && make")
(global-set-key "\C-xw" 'tex-buffer)
(global-set-key "\C-xg" 'magit-status)
(global-set-key "\C-zg" 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annoying stuff
(setq visible-bell t)
(setq inhibit-startup-echo-area-message "luna"
      inhibit-startup-message           t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)
(if (fboundp 'scroll-bar-mode) (tool-bar-mode -1))
;; xemacs equivalent
;(set-specifier top-toolbar-visible-p nil)
;; Scroll line by line.
(setq scroll-conservatively 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
(setq completion-ignore-case t
      pcomplete-ignore-case t
      read-file-name-completion-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
(setq-default mouse-yank-at-point t)
(put 'overwrite-mode 'disabled t)
(setq next-line-add-newlines nil) ; only needed for version < 21
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
(add-to-list 'load-path "~/.emacs.d/vendor/candy-chain")
(when (locate-library "color-theme")
  (require 'color-theme)
  (require 'candy-chain)
  (color-theme-initialize)
  (color-theme-candy-chain)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language environment
;; (set-terminal-coding-system 'iso-8859-1)
;; (setq default-buffer-file-coding-system 'iso-8859-1)
;; (prefer-coding-system 'iso-8859-1)
;; (set-language-environment "Latin-1")
;; (setq file-buffer-coding 'iso-8859-1)
;; (let ((mode (current-input-mode)))
;;   (setcar (cdr (cdr mode)) 8)
;;   (apply 'set-input-mode mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving

;; START Hunter:
;; ;; Auto-save every 300 characters typed
;; (setq auto-save-interval 300)

;; ;; Don't clutter up directories with files~
;; (setq backup-directory-alist `(("." . ,(expand-file-name
;; (concat dotfiles-dir "backups")))))

;; ;; Tramp as well
;; (setq tramp-backup-directory-alist backup-directory-alist)
;; (setq tramp-auto-save-directory (expand-file-name (concat dotfiles-dir "autosaves")))

;; ;; More backup configuration
;; (setq
;; backup-by-copying t	 ; Don't clobber symlinks
;; backup-directory-alist '(("." . "~/.emacs.d/backups/"))	; Don't litter FS
;; auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t))
;; delete-old-versions t
;; kept-new-versions 10
;; kept-old-versions 10
;; version-control t)	 ; Use versioned backups
;; END Hunter

(setq make-backup-files t)
;; Put backups in ~/.emacs.d/backup
(if (file-directory-p "~/.emacs.d/backup")
    (setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
  (message "Directory does not exist: ~/.emacs.d/backup"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Passwords
(let ((f "~/.emacs.authinfo"))
  (when (file-exists-p f)
    (load-file f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Modes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global modes
(column-number-mode t)
(line-number-mode t)
(transient-mark-mode t)
(setq iswitchb-regexp t)
(if (>= emacs-major-version 21) (iswitchb-mode t))
;;(if (> emacs-major-version 21) (iswitchb-default-keybindings))
(if (> emacs-major-version 20) (mouse-wheel-mode t))
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
(setq case-fold-search t) ; make searches case insensitive
(set-cursor-color "white")
(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ace jump mode
(add-to-list 'load-path "~/.emacs.d/vendor/ace-jump-mode")
(when (locate-library "ace-jump-mode")
  (require 'ace-jump-mode)
  (define-key global-map (kbd "C-0") 'ace-jump-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock mode
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
;; (if (window-system)
;;     (global-whitespace-mode t))
;; make whitespace-mode use just basic coloring
(setq whitespace-style
      '( spaces tabs newline space-mark tab-mark newline-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erlang
(add-to-list 'load-path "~/devel/otp/lib/tools/emacs")
(autoload 'erlang-mode "erlang.el" "" t)
(add-to-list 'auto-mode-alist '("\\.[eh]rl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.app$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.conf$" . erlang-mode))
(add-to-list 'auto-mode-alist '("reltool\\.config$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.compilation$" . compilation-minor-mode))
;(setq erlang-indent-level 2)
(add-hook 'erlang-new-file-hook 'tempo-template-erlang-normal-header)
(add-hook 'erlang-mode-hook (lambda () (setq parens-require-spaces nil)))
(condition-case nil (load "erlang_templates.el") (file-error))
(when (locate-library "distel")
  (require 'distel)
  (distel-setup)
  (setq erl-nodename-cache 'smcore\@spawn)
  ;; (defun vc-git-inferior-erlang ()
  ;;   (interactive)
  ;;   ;; TODO - make it automatically set up the node name and cookie, too
  ;;   (inferior-erlang (format "cd %s && make run"
  ;;                            (or (vc-git-root default-directory)
  ;;                                "~/code/ubiquity"))))
  ;; (setq erlang-shell-display-function 'vc-git-inferior-erlang-run-or-select)
  ;; (defun vc-git-inferior-erlang-run-or-select ()
  ;;   (interactive)
  ;;   (if (null (inferior-erlang-running-p))
  ;;       (vc-git-inferior-erlang)
  ;;     (inferior-erlang-display-buffer t)))
  ;; (setq erlang-shell-function 'vc-git-inferior-erlang)
  )
(when (locate-library "magit")
  (require 'magit)
)
(defcustom git-grep-switches "-E -I -nH -i --no-color"
  "Switches to pass to `git grep'."
  :type 'string)
(defcustom git-grep-default-work-tree (expand-file-name "~/code/mything")
  "Top of your favorite git working tree. \\[git-grep] will search from here if it cannot figure out where else to look."
  :type 'directory)
(when (require 'vc-git nil t)
  (defun git-grep (regexp)
    (interactive
     (list (read-from-minibuffer
            "Search git repo: "
            (let ((thing (and buffer-file-name
                              (thing-at-point 'symbol))))
              (or (and thing
                       (progn
                         (set-text-properties 0 (length thing) nil thing)
                         (shell-quote-argument (regexp-quote thing))))
                  ""))
            nil nil 'git-grep-history)))
    (let ((grep-use-null-device nil)
          (root (or (vc-git-root default-directory)
                    (prog1 git-grep-default-work-tree
                      (message "git-grep: %s doesn't look like a git working tree; searching from %s instead" default-directory root)))))
      (grep (format "GIT_PAGER='' git grep %s -e %s -- %s" git-grep-switches regexp root)))))
(global-set-key (kbd "C-x ?") 'git-grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kreditor
(defun kfind-at (path word)
  (grep-find (concat "find " path " -name '.svn' -prune -o "
		     "-name '*~' -prune -o "
		     "-name '*html' -prune -o "
		     "-type f -print0 | "
		     "xargs -0 -e grep -n -e " word)))

(defun kfind (word)
  (interactive "MFind: ")
  (kfind-at 
   (concat 
    (car (split-string (buffer-file-name) "lib"))
    "lib/")
   word)
  )
(global-set-key (kbd "C-.") 'next-error)
(global-set-key (kbd "C-,") 'previous-error)
(global-set-key "\C-zf" 'kfind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Muse
(let ((f "~/luna/www/muse.el"))
  (when (file-exists-p f)
    (load-file f)))
(let ((f "~/luna/jci/nio/nyhetsbrev/muse.el"))
  (when (file-exists-p f)
    (load-file f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; promela
(autoload 'promela-mode "promela.el" "" t)
(add-to-list 'auto-mode-alist '("\\.pr$" . promela-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smarkets sql
(add-to-list 'auto-mode-alist '("\\.migration$" . sql-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jabber
(when (locate-library "jabber")
  (require 'jabber-autoloads)
  (add-hook 'jabber-chat-mode-hook 'goto-address)
  (global-set-key (kbd "C-c j") 'jabber-connect-all)
  (global-set-key (kbd "C-c J") 'jabber-send-presence)
  (global-set-key (kbd "C-c M-j") 'jabber-disconnect)
  (global-set-key (kbd "C-c C-SPC") 'jabber-activity-switch-to)
  ;; sudo apt-get install libnotify-bin
  (defun jabber-message-notify (from buffer text propsed-alert)
    "Displays MESSAGE through notify-send (libnotify-bin)"
    (shell-command (concat "notify-send \"New message: " text "\"")))
  (add-hook 'jabber-alert-message-hooks 'jabber-message-notify)
  ;;(add-to-list 'jabber-alert-message-hooks 'jabber-message-notify)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rcirc
;; C-h v rcirc-authinfo
;; /keyword thing ; will boldify the thing
(when (locate-library "rcirc")
  (require 'rcirc)
  (setq rcirc-log-flag t)
  (setq rcirc-default-nick "dluna")
  (setq real-rcirc-nick "dluna")
  (setq rcirc-nick real-rcirc-nick)
  (global-set-key "\C-c\C-@" 'rcirc-next-active-buffer)
  (rcirc-track-minor-mode 1)
  ;; M-- C-l fixes stuff
  (add-hook 'rcirc-mode-hook
	    (lambda ()
	      (set (make-local-variable 'scroll-conservatively)
		   1000000)))
  ;; Include date in time stamp.
  (setq rcirc-time-format "%Y-%m-%d %H:%M ")
  (let ((f "~/.rcirc-authinfo"))
    (when (file-exists-p f)
      (load-file f)))
  (setq rcirc-server-alist
    '(("irc.freenode.net" :channels (
                                     "#2040"
                                     "#erlang"
                                     "#erlounge"
                                     "#hcoop"
                                     "#ratpoison"
                                     "#rcirc"
                                     "#rebar"
                                     ;; "#riak"
                                     "#riak_core"
                                     "#spawnfest"
                                     )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; psvn
(when (locate-library "psvn")
  (require 'psvn)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jinja
(autoload 'jinja-mode "jinja.el" "" t)
(add-to-list 'auto-mode-alist '("\\.jinja$" . jinja-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;; C-x C-t/t -> toggle TODO/DONE/nothing
;; C-ENTER -> new item at the same level as current item
;; M-leftarrow/M-rightarrow -> move current line/marked area up/down one level
(global-set-key (kbd "C-c t") 'org-todo)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; SQL-stuff: M-x sql-postgres
;; \d TABLENAME;  -- types and names and stuff
;; \df oplog*
;; \sf oplog_get
;; \sf oplog_get_from(uuid, integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 '(jabber-account-list `(("daniel.luna@smarkets.com/emacs" (:password . ,jabber-passwd) (:connection-type . starttls))))
 '(starttls-extra-arguments (quote ("--insecure")))
 '(jabber-auto-reconnect t)
 '(jabber-avatar-cache-directory "~/.emacs.d/.jabber-avatars/")
 '(jabber-chat-buffer-show-avatar nil)
 '(jabber-debug-keep-process-buffers t)
 '(jabber-global-history-filename "~/.emacs.d/.jabber_global_message_log")
 '(jabber-history-dir "~/.emacs.d/.jabber-history")
 '(jabber-history-enable-rotation t)
 '(jabber-history-enabled t)
 '(jabber-roster-line-format "%c %-25n %u %-8s  %S")
 '(jabber-show-offline-contacts nil)
 '(jabber-vcard-avatars-publish nil)
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((c-hanging-comment-ender-p) (erlang-indent-level . 4) (erlang-indent-level . 2)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(smerge-refined-change ((t (:background "blue")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session handling
;; --no-desktop
(if (fboundp 'desktop-save-mode)
    (progn
      (desktop-save-mode t)
      (global-set-key "\C-zd" 'desktop-change-dir)
      (setq desktop-clear-preserve-buffers
	    (append '("\\*.*\\..*\\*" "#.*" ".*@.*")
                    '("\\*-jabber.*\\*")
		    desktop-clear-preserve-buffers))
      (defun my-desktop-save ()
        (interactive)
        ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
        (if (eq (desktop-owner) (emacs-pid))
            (desktop-save desktop-dirname)))
      (add-hook 'auto-save-hook 'my-desktop-save)))
