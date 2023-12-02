;;; init.el --- Init File -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022 by Stephen Fromm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(message "Loading up Emacs...")
(defvar init-core-start-time (current-time))

(defun init-report-startup-time ()
  "Report startup time."
  (interactive)
  (message "Emacs is ready, finished loading after %.03fs."
           (float-time (time-subtract after-init-time before-init-time))))

(add-hook 'after-init-hook #'init-report-startup-time)


;;; Platform specific details.
(defun forge/system-type-darwin-p ()
  "Return non-nil if system is Darwin/MacOS."
  (string-equal system-type "darwin"))

(defun forge/system-type-windows-p ()
  "Return non-nil if system is Windows."
  (string-equal system-type "windows-nt"))

(defun forge/system-type-linux-p ()
  "Return non-nil if system is GNU/Linux."
  (string-equal system-type "gnu/linux"))

(defun forge/native-comp-p ()
  "Return non-nil native compilation is available."
  (if (fboundp 'native-comp-available-p) (native-comp-available-p)))

(defvar forge-site-dir (expand-file-name "site-lisp/" user-emacs-directory)
  "Path to user's site configuration.")

(defvar forge-personal-dir (expand-file-name "user/" user-emacs-directory)
  "Path to user's personal configuration.")

(defvar forge-themes-dir (expand-file-name "themes/" user-emacs-directory)
  "Path to user themes.")

(defvar forge-state-dir (expand-file-name "var/" user-emacs-directory)
  "Path to Emacs' persistent data files.")

(defvar forge-backup-dir (expand-file-name "backup/" forge-state-dir)
  "Path to Emacs' backup and autosave files.")

(defvar forge-log-dir (expand-file-name "log/" forge-state-dir)
  "Path to Emacs packages' log files.")


;; Load custom and then do basic initialization.
(setq custom-file (expand-file-name "custom.el" forge-personal-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-elpa)


(defun init-mkdirs-user-emacs-directory ()
  "Create emacs.d directories environment."
  (dolist (dir (list forge-site-dir forge-personal-dir forge-state-dir forge-backup-dir forge-log-dir))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun init-clean-user-emacs-directory ()
  "Set appropriate paths to keep `user-emacs-directory' clean."
  (interactive)
  (with-no-warnings
    (setq gamegrid-user-score-file-directory (expand-file-name "games" forge-state-dir)
          async-byte-compile-log-file (expand-file-name "async-bytecomp.log" forge-state-dir))
    (customize-set-variable 'bookmark-default-file (expand-file-name "bookmarks" forge-state-dir))
    (customize-set-variable 'calc-settings-file (expand-file-name "calc-settings.el" forge-state-dir))
    (customize-set-variable 'transient-history-file (expand-file-name "transient/history.el" forge-state-dir))
    (customize-set-variable 'transient-levels-file (expand-file-name "transient/levels.el" forge-personal-dir))
    (customize-set-variable 'transient-values-file (expand-file-name "transient/values.el" forge-personal-dir))
    (customize-set-variable 'message-auto-save-directory (expand-file-name "messages" forge-state-dir))
    (customize-set-variable 'package-quickstart-file (expand-file-name "package-quickstart.el" forge-state-dir))
    (customize-set-variable 'project-list-file (expand-file-name "project-list.el" forge-state-dir))
    (customize-set-variable 'tramp-auto-save-directory (expand-file-name "tramp/auto-save" forge-state-dir))
    (customize-set-variable 'tramp-persistency-file-name (expand-file-name "tramp/persistency.el" forge-state-dir))
    (customize-set-variable 'url-cache-directory (expand-file-name "url/cache/" forge-state-dir))
    (customize-set-variable 'url-configuration-directory (expand-file-name "url/configuration/" forge-state-dir))))

(defun my-initialize ()
  "Initialize paths and session for this Emacs instance."
  (init-mkdirs-user-emacs-directory)
  (init-clean-user-emacs-directory)
  ;; when native compilation is available ...
  (when (forge/native-comp-p)
    (setq native-comp-deferred-compilation nil
          native-comp-async-report-warnings-errors 'silent
          package-native-compile nil))
  ;;
  (add-to-list 'load-path forge-site-dir)
  (add-to-list 'custom-theme-load-path forge-themes-dir)
  (setq inhibit-splash-screen t
        ;; always load the newer version of a file
        load-prefer-newer t
        ;; warn when opening files bigger than 50MB
        large-file-warning-threshold 50000000))

(my-initialize)

(defgroup forge nil
  "Forge custom settings."
  :group 'environment)


(defun forge/message-module-load (mod time)
  "Log message on how long it took to load module MOD from TIME."
  (message "Loaded %s (%0.2fs)" mod (float-time (time-subtract (current-time) time))))

(defun forge/load-directory-modules (path)
  "Load Lisp files in PATH directory."
  (let ((t1 (current-time)))
    (when (file-exists-p path)
      (message "Loading lisp files in %s..." path)
      (mapc 'load (directory-files path 't "^[^#\.].*el$"))
      (forge/message-module-load path t1))))

(defun forge/load-modules (&rest modules)
  "Load forge modules MODULES."
  (interactive)
  (dolist (module (cons '() modules ))
    (when module
      (let ((t1 (current-time)))
        (unless (featurep module)
          (require module nil t)
          (forge/message-module-load module t1))))))

(forge/load-directory-modules forge-site-dir)


;; dbus is a linux thing -- only load on that platform
(when (forge/system-type-linux-p)
  (require 'dbus)

  (defun forge/network-online-p ()
    "Check if we have a working network connection"
    (interactive)
    (let ((nm-service "org.freedesktop.NetworkManager")
          (nm-path "/org/freedesktop/NetworkManager")
          (nm-interface "org.freedesktop.NetworkManager")
          (nm-state-connected-global 70))
      (eq nm-state-connected-global
          (dbus-get-property :system nm-service nm-path nm-interface "State"))))
  )


;;; exec-path-from-shell
;;; Set exec-path based on shell PATH.
;;; Some platforms, such as MacOSX, do not get this done correctly.
(with-eval-after-load 'exec-path-from-shell
  (exec-path-from-shell-initialize))

(when (forge/system-type-darwin-p)
  (dolist (path (list "/usr/local/bin" (expand-file-name "~/bin")))
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (setenv "PATH" (concat path ":" (getenv "PATH")))
      (add-to-list 'exec-path path))))

(when (forge/system-type-darwin-p)
  (defun forge/network-online-p ()
    "Check if online."
    (interactive)
    (let* ((output (shell-command-to-string "networksetup -listnetworkserviceorder | grep 'Hardware Port'"))
           (netsetup (split-string output "\n")))
      (catch 'found
        (dolist (elt netsetup)
          (when (> (length elt) 0)
            (let* ((netifseq (string-match "Device: \\([a-z0-9]+\\))" elt))
                   (netif (match-string 1 elt)))
              (when (string-match "status: active" (shell-command-to-string (concat "ifconfig " netif " | grep status")))
                (throw 'found netif)))))))))

(defvar forge/vpn-config ""
  "Name of the OpenVPN VPN configuration to use.")

(when (forge/system-type-darwin-p)
  (defun vpn-connect ()
    "Connect to VPN configuration CFG.
Assumes you are are on MacOS and using Wireguard to connect."
    (interactive)
    (require 'em-glob)
    (let ((cfg (completing-read "Config: "
                                (mapcar #'file-name-sans-extension
                                        (directory-files "~/annex/etc" nil (eshell-glob-regexp "wg*conf"))))))
      (setq forge/vpn-config cfg)
      (when (forge/system-type-darwin-p)
        (shell-command (concat "scutil --nc start " cfg)))))

  (defun vpn-disconnect ()
    "Disconnect VPN configuration CFG.
Assumes you are are on MacOS and using Wireguard to connect."
    (interactive)
    (when (forge/system-type-darwin-p)
      (shell-command (concat "scutil --nc stop " forge/vpn-config))))

  (defun openvpn-connect ()
    "Connect to OpenVPN configuration CFG.
Assumes you are on MacOS and using Tunnelblick to connect."
    (interactive)
    (require 'em-glob)
    (let ((cfg (completing-read "Config: "
                                (mapcar #'file-name-sans-extension
                                        (directory-files "~/annex/etc" nil (eshell-glob-regexp "*ovpn"))))))
      (setq forge/vpn-config cfg)
      (when (forge/system-type-darwin-p)
        (let ((osatmpl ""))
          (setq osatmpl (concat "tell application \"/Applications/Tunnelblick.app\"\n"
                                "    connect \"" cfg "\"\n"
                                "end tell"))
          (do-applescript osatmpl)))))

  (defun openvpn-disconnect ()
    "Disconnect from VPN.
Assumes you are on MacOS and using Tunnelblick to manage your VPN."
    (interactive)
    (let ((osatmpl ""))
      (setq osatmpl (concat "tell application \"/Applications/Tunnelblick.app\"\n"
                            "    disconnect \"" forge/vpn-config "\"\n"
                            "end tell"))
      (do-applescript osatmpl))))

(when (forge/system-type-darwin-p)
  (defun forge/get-current-song-itunes ()
    "Get current song playing via itunes."
    (let ((osa-tmpl "")
          (cursong nil))
      (setq osa-tmpl "tell application \"Music\"
	if player state is not stopped then
		set ct to (properties of current track)
		set this_song to \"\"
		if (class of ct is URL track) and (get current stream title) is not missing value then
			set this_song to (get current stream title)
		else
			set this_song to artist in ct & \" - \" & name in ct
		end if
		this_song
	end if
end tell")
      (condition-case nil
          (setq cursong (split-string (do-applescript osa-tmpl) " - "))
        (error nil))
      cursong)))


(defun my-reload-emacs-configuration ()
  "Reload emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun forge/toggle-highlight-line ()
  "Toggle `hl-line-mode'."
  (interactive)
  (if (bound-and-true-p hl-line-mode) (hl-line-mode -1) (hl-line-mode t)))

(defun forge/turn-on-delete-trailing-whitespace ()
  "Turn on `delete-trailing-whitespace' when saving files."
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(defun forge/turn-off-delete-trailing-whitespace ()
  "Turn off `delete-trailing-whitespace' when saving files."
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

;; Via jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(defun lookup-password (host user port)
  "Look up password for HOST, USER, and PORT."
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

;; Via https://emacs.stackexchange.com/questions/8104/is-there-a-mode-to-automatically-update-copyright-years-in-files
(defun forge/enable-copyright-update ()
  "Update copyright year when saving a file."
  (when (fboundp 'copyright-update)
    (setq copyright-names-regexp "Free Software")
    (add-hook 'before-save-hook #'copyright-update)))

;; Delete window if not the only one.
(defun forge/delete-window ()
  "Delete window if it is not the only one."
  (when (not (one-window-p))
    (delete-window)))

(defun forge/transparency (value)
  "Set the transparency of the frame window with VALUE 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun forge/whitespace-visualize ()
  "Enable whitespace visualizations."
  (setq highlight-tabs t)
  (setq show-trailing-whitespace t))

(defun forge/untabify-buffer ()
  "Remove tab characters from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun forge/sudo-file-path (file)
  "Return path for FILE with sudo access."
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host) host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun forge/sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (forge/sudo-file-path file)))

(defun forge/sudo-this-file ()
  "Open current file as root."
  (interactive)
  (find-file
   (forge/sudo-file-path
    (or buffer-file-name
        (or buffer-file-name
            (when (or (derived-mode-p 'dired-mode)
                      (derived-mode-p 'wdired-mode))
              default-directory))))))

(defun dig-extended (fn &optional
                        domain query-type query-class query-option dig-option server)
  "Wrapper for `dig'.
Query for DNS records for DOMAIN of QUERY-TYPE."
  (message "domain: '%s'" domain)
  (unless domain
    (setq domain (read-string "Host: ")))
  (unless query-type
    (setq query-type (completing-read "Type: " '("A" "SOA" "NS" "TXT" "CNAME" "PTR"))))
  (funcall fn domain query-type query-class query-option dig-option server))

(advice-add 'dig :around #'dig-extended)

(defconst speed_of_light 299792458 "Speed of light, m/s.")

(defun wavelength-to-frequency (wavelength)
  "Convert a wavelength to frequency."
  (interactive "nWavelength: ")
  (message "Frequency: %0.2f" (/ (/ speed_of_light wavelength) 1000)))

(defun frequency-to-wavelength (frequency)
  "Convert a frequency to wavelength (nm)."
  (interactive "nFrequency: ")
  (message "Wavelength: %0.4f" (/ (/ speed_of_light frequency) 1000)))

(defun forge/date-today ()
  "Insert friendly date string for today."
  (interactive)
  (insert (format-time-string "%B %d, %Y" (current-time))))

(defmacro forge-mkhome-target (target)
  "Macro to run mkhome makefile TARGET."
  `(with-temp-buffer
     (progn
       (cd (getenv "HOME"))
       (compile (mapconcat 'shell-quote-argument (list "make" "-f" "Makefile.mkhome" ,target) " ")))))

(defun forge-mkhome-update ()
  "Run mkhome git."
  (interactive)
  (forge-mkhome-target "update"))

(defun forge-mkhome-www ()
  "Run mkhome www."
  (interactive)
  (forge-mkhome-target "www"))

(defun forge-mkhome-src ()
  "Run mkhome src."
  (interactive)
  (forge-mkhome-target "src"))

(require 'init-appearance)


(use-package which-key
  :custom (which-key-idle-delay 1.5)
  :demand t
  :diminish
  :commands which-key-mode
  :config (which-key-mode))

(define-prefix-command 'forge-mkhome-map)
(define-key forge-mkhome-map (kbd "g") 'forge-mkhome-update)
(define-key forge-mkhome-map (kbd "w") 'forge-mkhome-www)
(define-key forge-mkhome-map (kbd "s") 'forge-mkhome-src)

(define-prefix-command 'forge-map)
(define-key forge-map (kbd "w") 'forge/window/body)
(define-key forge-map (kbd "n") 'forge/navigate/body)
(define-key forge-map (kbd "m") 'notmuch-cycle-notmuch-buffers)
(define-key forge-map (kbd "h") 'forge-mkhome-map)
(define-key forge-map (kbd "f") 'elfeed)
(define-key forge-map (kbd "j") 'forge/jabber-start-or-switch)
(define-key forge-map (kbd "g") 'magit-status)
(define-key forge-map (kbd "s") 'eshell-here)
(define-key forge-map (kbd "S") 'forge/slack/body)
(define-key forge-map (kbd "t") 'org-pomodoro)
(define-key forge-map (kbd "p") 'paradox-list-packages)
(define-key forge-map (kbd "u") 'browse-url-at-point)
(define-key forge-map (kbd "F") 'forge-focus)
(global-set-key (kbd "C-z") 'forge-map)


(use-package hydra
  :demand t
  :config
  (defhydra forge/navigate (:foreign-keys run)
    "[Navigate] or q to exit."
    ("a" beginning-of-line)
    ("e" end-of-line)
    ("l" forward-char)
    ("h" backward-char)
    ("n" next-line)
    ("j" next-line)
    ("p" previous-line)
    ("k" previous-line)
    ("d" View-scroll-half-page-forward)
    ("u" View-scroll-half-page-backward)
    ("SPC" scroll-up-command)
    ("S-SPC" scroll-down-command)
    ("[" backward-page)
    ("]" forward-page)
    ("{" backward-paragraph)
    ("}" forward-paragraph)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("." end-of-buffer)
    ("C-'" nil)
    ("q" nil :exit t))

  (defhydra forge/window ()
    ("a" ace-window "Ace Window" :exit t)
    ("t" transpose-frame "Transpose" :exit t)
    ("o" ace-delete-other-windows "Delete other windows " :exit t)
    ("s" ace-swap-window "Swap window" :exit t)
    ("d" ace-delete-window "Delete window" :exit t)
    ("b" consult-buffer "Switch" :exit t)
    ("g" golden-ratio "Golden ratio" :exit t)
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)) "Split Vert")
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)) "Split Horz")
    ("m" consult-bookmark "Bookmark" :exit t)
    ("q" nil))

  (defhydra forge/music-mpd-hydra ()
    "MPD Actions"
    ("p" mingus-toggle "Play/Pause")
    ("/" mingus-search "Search" :exit t)
    ("c" (message "Currently Playing: %s" (shell-command-to-string "mpc status")) "Currently Playing")
    ("m" mingus "Mingus" :exit t)
    ("<" (progn
           (mingus-prev)
           (message "Currently Playing: %s" (shell-command-to-string "mpc status"))) "Previous")
    (">" (progn
           (mingus-next)
           (message "Currently Playing: %s" (shell-command-to-string "mpc status"))) "Next")
    ("+" (dotimes (i 5) (mingus-vol-up)) "Louder")
    ("-" (dotimes (i 5) (mingus-vol-down)) "Quieter")
    ("q" nil "Quit"))

  (defhydra forge/music-emms-hydra ()
    "EMMS Actions"
    ("SPC" emms-pause "Play/Pause")
    ("s" emms-stop "Stop")
    ("c" emms-show "Currently Playing")
    ("m" emms "EMMS")
    ("S" emms-streams "EMMS Streams")
    ("<" emms-previous "Previous")
    (">" emms-next "Next")
    ("+" emms-volume-raise "Louder")
    ("-" emms-volume-lower "Quieter")
    ("C" emms-playlist-clear "Clear")
    ("q" nil "Quit"))

  (defhydra forge/slack (:color blue)
    ("s" slack-start "Start")
    ("i" slack-im-select "IM")
    ("g" slack-group-select "Group")
    ("c" slack-channel-select "Channel")
    ("d" slack-ws-close "Disconnect")
    ("q" nil))

  )


;; https://github.com/minad/vertico
(use-package vertico
  :demand t
  :init
  (vertico-mode))

;; https://github.com/oantolin/orderless
(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic)))

;; https://github.com/minad/consult
(use-package consult
  :demand t
  :bind
  ;; M-g go-to map
  (("M-g g" . consult-goto-line)
   ("M-g h" . consult-org-heading)
   ("M-g i" . consult-imenu)
   ;; M-s search map
   ("M-s l" . consult-line)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-y" . consult-yank-pop)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ("C-c f" . consult-find))
  :config
  (setq consult-narrow-key "<"
        consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; https://github.com/minad/marginalia
(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("C-M-a" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (marginalia-mode))


;;; Navigation

;;; windmove
(use-package windmove
  :bind
  (("s-l" . windmove-right)
   ("s-h" . windmove-left)
   ("s-k" . windmove-up)
   ("s-j" . windmove-down))
  :custom (windmove-wrap-around t)
  :config (windmove-default-keybindings 'super))

;;; ace-window
(use-package ace-window
  :bind
  (([remap other-window] . ace-window)))

(use-package dumb-jump
  :demand t
  :commands (xref-find-definitions)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; this requires at least xref-1.1.0, which comes with emacs-28.1 or newer
  (when (version<= "28.1" emacs-version)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))

(use-package avy
  :bind ("C-." . avy-goto-char-timer)
  :custom
  (avy-case-fold-search t)
  :functions (avy-setup-default)
  :config (avy-setup-default))


(when (require 'tab-bar nil 'noerror)
  (tab-bar-mode)
  (setq tab-bar-close-tab-select 'recent
        tab-bar-close-button-show nil
        tab-bar-new-tab-choice 'ibuffer
        tab-bar-new-tab-to 'right
        tab-bar-position nil
        tab-bar-select-tab-modifiers '(super meta)
        tab-bar-tab-hints t
        tab-bar-show 1))

;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-tab.el
(defun prot-tab--tab-bar-tabs ()
  "Return a list of `tab-bar' tabs, minus the current one."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar--tabs-recent)))

(defun forge/switch-tab-dwim ()
  "Do-What-I-Mean (DWIM) switch to other tab.
This will create a new tab if no tabs exist, switch
to the other tab if there are only 2 tabs, and finally
prompt for what tab to switch to."
  (interactive)
  (let ((tabs (prot-tab--tab-bar-tabs)))
    (cond ((eq tabs nil)
           (tab-new))
          ((eq (length tabs) 1)
           (tab-next))
          (t
           (call-interactively #'tab-bar-select-tab-by-name)))))

(global-set-key (kbd "C-x t t") #'forge/switch-tab-dwim)

(use-package transpose-frame)

(use-package golden-ratio
  :hook
  (ediff-before-setup-windows . (lambda () (golden-ratio-mode -1)))
  :config
  (setq golden-ratio-exclude-modes '(messages-buffer-mode
                                     fundamental-mode
                                     ediff-mode
                                     calendar-mode
                                     calc-mode
                                     calc-trail-mode
                                     magit-popup-mode))
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"
        uniquify-after-kill-buffer-p t))

(use-package olivetti
  :custom
  (olivetti-hide-mode-line t)
  (olivetti-body-width 80)
  :commands olivetti-mode
  :preface
  (defun forge-focus ()
    "Enable features to focus."
    (interactive)
    (olivetti-mode)))

(require 'init-ibuffer)


(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(setq scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-follow-mouse 't)         ;; scroll window under mouse
;; (setq hscroll-margin 2
;;       hscroll-step 1
;;       scroll-conservatively 101
;;       scroll-preserve-screen-position t
;;       auto-window-vscroll nil
;;       mouse-wheel-follow-mouse 't         ;; scroll window under mouse
;;       mouse-wheel-progressive-speed nil   ;; don't accelerate scrolling
;;       mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))
;;       mouse-wheel-scroll-amount-horizontal 2)

(defun forge/save-all ()
  "Save any file-related buffers."
  (interactive)
  (message "Saving buffers at %s" (format-time-string "%Y-%m-%dT%T"))
  (save-some-buffers t))

;; If focus switches away, save all files.
(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'forge/save-all))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(setq version-control t        ;; number each backup file
      backup-by-copying t      ;; instead of renaming current file
      delete-old-versions t    ;; clean up after oneself
      kept-new-versions 5      ;; Number of newer versions to keep.
      kept-old-versions 5      ;; Number of older versions to keep.
      trash-directory "~/.Trash"
      backup-directory-alist (list (cons "." forge-backup-dir))
      tramp-backup-directory-alist backup-directory-alist)

;; Turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      auto-save-timeout 120
      auto-save-interval 64
      auto-save-include-big-deletions t ;; don't auto-disable auto-save after deleting large chunks
      auto-save-list-file-prefix (expand-file-name "autosave/" forge-state-dir)
      ;; handle tramp paths differently than local ones, borrowed from doom
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))


(require 'savehist)
(with-eval-after-load 'savehist
  (setq savehist-file (expand-file-name "savehist" forge-state-dir)
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
        history-length 1000
        history-delete-duplicates t
  (add-hook 'after-init-hook #'savehist-mode))

(require 'init-editing)

(require 'init-editing-lang)


(use-package undo-tree
  :disabled t
  :diminish undo-tree-mode
  :bind
  (("C-/" . undo-tree-undo)
   ("C-?" . undo-tree-redo)
   ("C-x u" . undo-tree-visualize))
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package kind-icon
  :after corfu
  :if (display-graphic-p)
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package emacs
  :init
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(require 'init-chat)

(require 'notifications)
(require 'tls)

(require 'init-dired)

(require 'init-git)

(require 'init-email)

(require 'init-org)

(require 'init-pass)

(require 'init-elfeed)

(require 'init-term)

(require 'init-utils)

(forge/load-directory-modules forge-personal-dir)
