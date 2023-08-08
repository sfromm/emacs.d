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

(setq package-archives '(("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(defun my-package-install (package)
  "Install PACKAGE if not yet installed."
  (unless (fboundp 'package-installed-p)
    (package-initialize))
  (unless (package-installed-p package)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (package-install package)
    (message "Installed package %s." package)
    (delete-other-windows)))

(defvar init-core-packages '(use-package diminish quelpa quelpa-use-package org org-contrib)
  "A list of core packages that will be automatically installed.")

(defun init-install-core-packages ()
  "Install core packages to install for Emacs."
  (interactive)
  (dolist (package init-core-packages)
    (progn (my-package-install package))))

(init-install-core-packages)

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'diminish)
  (require 'use-package)
  (require 'use-package-ensure))

(setq use-package-verbose t
      use-package-compute-statistics t       ;; compute stats
      use-package-always-ensure t
      use-package-minimum-reported-time 0.1) ;; carp if it takes awhile to load a package

(use-package quelpa
  :init
  (setq quelpa-dir (expand-file-name "quelpa" forge-state-dir)
        quelpa-checkout-melpa-p nil  ;; I'm not using quelpa for packages already in melpa
        quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :demand t
  :after quelpa)

(use-package paradox
  :init
  (setq paradox-execute-asynchronously t))

(defun my-package-upgrade-packages ()
  "Upgrade all installed packages."
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute 'noquery)
    (message "Packages updated.")))

;; Via spacemacs/core/core-funcs.el
;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/core/core-funcs.el
(defun my-recompile-elpa ()
  "Recompile packages in elpa directory.  Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

(defun init-clean-user-emacs-directory ()
  "Set appropriate paths to keep `user-emacs-directory' clean."
  (interactive)
  (with-no-warnings
    (setq gamegrid-user-score-file-directory (expand-file-name "games" forge-state-dir)
          async-byte-compile-log-file (expand-file-name "async-bytecomp.log" forge-state-dir)
          bookmark-default-file (expand-file-name "bookmarks" forge-state-dir)
          calc-settings-file (expand-file-name "calc-settings.el" forge-state-dir)
          transient-history-file (expand-file-name "transient/history.el" forge-state-dir)
          transient-levels-file (expand-file-name "transient/levels.el" forge-personal-dir)
          transient-values-file (expand-file-name "transient/values.el" forge-personal-dir)
          message-auto-save-directory (expand-file-name "messages" forge-state-dir)
          package-quickstart-file (expand-file-name "package-quickstart.el" forge-state-dir)
          project-list-file (expand-file-name "project-list.el" forge-state-dir)
          tramp-auto-save-directory (expand-file-name "tramp/auto-save" forge-state-dir)
          tramp-persistency-file-name (expand-file-name "tramp/persistency.el" forge-state-dir)
          url-cache-directory (expand-file-name "url/cache/" forge-state-dir)
          url-configuration-directory (expand-file-name "url/configuration/" forge-state-dir))))

(defun my-initialize ()
  "Initialize paths and session for this Emacs instance."
  (dolist (dir (list forge-site-dir forge-personal-dir forge-state-dir forge-backup-dir forge-log-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))
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

;; Load custom and then do basic initialization.
(setq custom-file (expand-file-name "custom.el" forge-personal-dir))
(when (file-exists-p custom-file)
  (load custom-file))
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

(use-package ip-query
  :quelpa (ip-query :fetcher github :repo "sfromm/ip-query")
  :commands (ip-query))

(defcustom forge-peek-buffer-name "*forge-peek*"
  "Buffer for peeking at data."
  :group 'forge
  :type 'string)

(defun forge/peek-first ()
  "Go to beginning of peek buffer."
  (interactive)
  (goto-char (point-min)))

(defun forge/peek-last ()
  "Go to end of peek buffer."
  (interactive)
  (goto-char (point-max)))

(defvar forge-peek-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'delete-frame)
    (define-key map (kbd "<") 'forge/peek-first)
    (define-key map (kbd ">") 'forge/peek-last)
    map)
  "Keymap for forge-peek mode.")

(define-derived-mode forge-peek-mode fundamental-mode "ForgePeek"
                     "A major mode for peeking at query responses."
                     :group 'forge
                     (setq buffer-read-only t)
                     (setq buffer-undo-list t))

(defun forge/peek-make-buffer ()
  "Return the peek query buffer."
  (let ((buffer (get-buffer-create forge-peek-buffer-name)))
    (with-current-buffer buffer (forge-peek-mode))
    buffer))

;; Make a peek-frame, a modified version of what is from here:
;; https://tuhdo.github.io/emacs-frame-peek.html
(defun forge/peek-make-frame (func &rest args)
  "Make a new frame for peeking at information.  Provide FUNC that will return data and optional ARGS."
  (let ((summary)
        (peek-frame)
        (x) (y)
        (abs-pixel-pos (save-excursion
                         ;; (beginning-of-thing 'word)
                         (window-absolute-pixel-position))))
    (setq x (car abs-pixel-pos))
    (setq y (+ (cdr abs-pixel-pos) (frame-char-height)))

    (setq peek-frame (make-frame '((minibuffer . nil)
                                   (name . "*Peek*")
                                   (width . 80)
                                   (visibility . nil)
                                   (height . 25))))
    (message "peek %s" peek-frame)

    (set-frame-position peek-frame x y)

    (with-selected-frame peek-frame
      (forge/peek-make-buffer)
      (funcall func)
      (recenter-top-bottom 0)
      (select-window (display-buffer forge-peek-buffer-name t t))
      (delete-other-windows))

    (make-frame-visible peek-frame)))

(defun forge/peek-ip-qry ()
  "Look up information on IP address."
  (interactive)
  (let ((qry (lambda ()
               (let ((ipqry (concat (getenv "HOME") "/src/ncon/ncon.sh"))
                     (ipaddr))
                 (if (not (region-active-p))
                     (setq ipaddr (read-string "IP address: "))
                   (setq ipaddr (buffer-substring (region-beginning) (region-end))))
                 (with-current-buffer forge-peek-buffer-name
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (call-process ipqry nil forge-peek-buffer-name t "ip qry " ipaddr)))))))
    (forge/peek-make-frame qry)))

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

(defcustom forge-font "IBM Plex Mono"
  "Preferred default font."
  :type 'string
  :group 'forge)

(defcustom forge-font-size 12
  "Preferred font size."
  :type 'integer
  :group 'forge)

(defcustom forge-variable-pitch-font "Fira Sans"
  "Preferred variable pitch font."
  :type 'string
  :group 'forge)

(defcustom forge-variable-pitch-scale 1.1
  "Preferred variable pitch font."
  :type 'decimal
  :group 'forge)

(defcustom forge-unicode-font "Fira Sans"
  "Preferred Unicode font.  This takes precedence over `forge-unicode-extra-fonts'."
  :type 'string
  :group 'forge)

(defvar forge-unicode-extra-fonts
  (list "all-the-icons"
        "FontAwesome"
        "github-octicons"
        "Weather Icons")
  "List of extra Unicode fonts.")

(defun forge/font-name-and-size ()
  "Compute and return font name and size string."
  (interactive)
  (let* ((size (number-to-string forge-font-size))
         (name (concat forge-font "-" size)))
    (if (interactive-p) (message "Font: %s" name))
    name))

(defun forge/font-ok-p ()
  "Is configured font valid?"
  (interactive)
  (member forge-font (font-family-list)))

(defun forge/font-size-increase ()
  "Increase font size."
  (interactive)
  (setq forge-font-size (+ forge-font-size 1))
  (forge/font-update))

(defun forge/font-size-decrease ()
  "Decrease font size."
  (interactive)
  (setq forge-font-size (- forge-font-size 1))
  (forge/font-update))

(defun forge/font-update ()
  "Update font configuration."
  (interactive)
  (when (forge/font-ok-p)
    (progn
      (message "Font: %s" (forge/font-name-and-size))
      ;; (set-frame-font forge-font)
      (set-face-attribute 'default nil :family forge-font :weight 'semi-light :height (* forge-font-size 10))
      (set-face-attribute 'fixed-pitch nil :family forge-font :height 1.0)
      (when forge-variable-pitch-font
        (set-face-attribute 'variable-pitch nil :family forge-variable-pitch-font :height forge-variable-pitch-scale))
      (when (fboundp 'set-fontset-font) ;; from doom-emacs
        (dolist (font (append (list forge-unicode-font) forge-unicode-extra-fonts))
          (set-fontset-font t 'unicode (font-spec :family font) nil 'prepend))))))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun forge/emoji-shrug () "Shrug emoji." (interactive) (insert "¯\\_(ツ)_/¯"))
(defun forge/emoji-glare () "Glare emoji." (interactive) (insert "ಠ_ಠ"))
(defun forge/emoji-table-flip () "Table fip emoji." (interactive) (insert "(╯°□°）╯︵ ┻━┻"))

(use-package emojify
  :init (setq emojify-emojis-dir (expand-file-name "emojis" forge-state-dir)))

(use-package rainbow-mode)

(defcustom forge-theme 'modus-operandi
  "Preferred graphics theme."
  :type 'symbol
  :group 'forge)

;; https://github.com/hlissner/emacs-solaire-mode
;; Encouraged by doom-themes
(use-package solaire-mode
  :disabled t)

;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  (doom-themes-org-config))

;; https://github.com/dracula/emacs
(use-package dracula-theme)

;; https://github.com/fniessen/emacs-leuven-theme
(use-package leuven-theme)

;; https://github.com/cpaulik/emacs-material-theme
(use-package material-theme)

;; https://gitlab.com/protesilaos/ef-themes
(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t))

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :custom
  (modus-themes-mixed-fonts t))

;; https://github.com/rougier/nano-theme
(use-package nano-theme)

;; https://github.com/kunalb/poet
(use-package poet-theme)

;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :custom
  (solarized-use-variable-pitch t)
  (solarized-scale-org-headlines t))

;; https://github.com/ianpan870102/tron-legacy-emacs-theme
(use-package tron-legacy-theme
  :custom
  (tron-legacy-theme-vivid-cursor t)
  (tron-legacy-theme-softer-bg t))

;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :custom
  (zenburn-use-variable-pitch t)
  (zenburn-scale-org-headlines t))

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :custom
  (doom-modeline-github nil "Disable github integration")
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-lsp nil "Disable integration with lsp")
  (doom-modeline-workspace-name t)
  :hook
  (doom-modeline-mode . column-number-mode)
  (doom-modeline-mode . size-indication-mode)
  (after-init . doom-modeline-mode))

;; https://github.com/milkypostman/powerline
(use-package powerline
  :disabled t
  :custom
  (powerline-default-separator 'slant)
  (powerline-default-separator-dir (quote (left . right)))
  (powerline-display-buffer-size nil)
  (powerline-display-hud nil)
  (powerline-display-mule-info nil)
  (powerline-gui-use-vcs-glyph t)
  :hook
  (after-init . powerline-default-theme))

;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :disabled t
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'respectful)
  (sml/mode-width 'full)
  (sml/name-width 30)
  (sml/shorten-modes t)
  :hook
  (after-load-theme . smart-mode-line-enable)
  (after-init . sml/setup))

(use-package nyan-mode)

(defun forge/setup-ui ()
  "Set up the look and feel."
  (interactive)
  (when forge-theme
    (load-theme forge-theme t))
  (setq visible-bell t                 ;; set a visible bell ...
        ring-bell-function #'ignore)    ;; and squash the audio bell
  (when (display-graphic-p)
    (when (forge/system-type-darwin-p)
      (setq frame-resize-pixelwise t))  ;; allow frame resizing by pixels, instead of character dimensions
    (forge/font-update)
    (line-number-mode t)                ;; show line number in modeline
    (column-number-mode t)              ;; show column number in modeline
    (size-indication-mode t)            ;; show buffer size in modeline
    (tool-bar-mode -1)                  ;; disable toolbar
    (scroll-bar-mode -1)                ;; disable scroll bar
    (display-battery-mode)))

(defun forge/setup-ui-in-daemon (frame)
  "Reload the UI in a daemon frame FRAME."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'forge/setup-ui))))

(when (daemonp)
  (add-hook 'after-make-frame-functions #'forge/setup-ui-in-daemon))
(add-hook 'after-init-hook #'forge/setup-ui)

(use-package lin
  :config
  (setq lin-face 'lin-mac-override-fg)
  (dolist (hook '(dired-mode-hook
                  elfeed-search-mode-hook
                  log-view-mode-hook
                  magit-log-mode-hook
                  notmuch-search-mode-hook
                  notmuch-tree-mode-hook
                  occur-mode-hook
                  org-agenda-mode-hook
                  package-menu-mode-hook))
    (add-hook hook #'lin-mode)))

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
  :custom (completion-styles '(orderless)))

;; https://github.com/raxod502/selectrum
(use-package selectrum
  :disabled t
  :demand t
  :config
  (selectrum-mode 1))

;; https://github.com/raxod502/prescient.el
(use-package prescient
  :disabled t
  :demand t
  :config
  (setq prescient-history-length 200)
  (setq prescient-save-file (expand-file-name "prescient-items" forge-state-dir))
  (prescient-persist-mode 1))

;; https://github.com/raxod502/selectrum
(use-package selectrum-prescient
  :disabled t
  :demand t
  :after (:all selectrum prescient)
  :config
  (selectrum-prescient-mode +1))

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
  (setq consult-project-root-function
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

;;; ivy, swiper, and counsel
;;; https://github.com/abo-abo/swiper
(use-package ivy
  :disabled t
  :diminish (ivy-mode . "")
  :bind
  (("C-c C-r" . ivy-resume))
  :init
  (ivy-mode 1)
  :config
  (define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-alt-done)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package swiper
  :disabled t
  :diminish
  :bind (("C-s" . swiper-isearch)))

(use-package counsel
  :disabled t
  :requires ivy
  :bind
  (("C-c f" . counsel-git)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-use-virtual-buffers t))

;;; avy
;;;
(use-package avy
  :disabled t
  :bind
  (("M-g g" . avy-goto-line)
   ("M-s" . avy-goto-word-1)))

(use-package smex
  :disabled t
  :init
  (setq smex-completion-method 'ivy
        smex-save-file (expand-file-name "smex-items" forge-state-dir)))

;;; windmove
(use-package windmove
  :bind
  (("s-l" . windmove-right)
   ("s-h" . windmove-left)
   ("s-k" . windmove-up)
   ("s-j" . windmove-down))
  :custom (windmove-wrap-around t)
  :config (windmove-default-keybindings 'super))

(use-package dumb-jump
  :demand t
  :commands (xref-find-definitions)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; this requires at least xref-1.1.0, which comes with emacs-28.1 or newer
  (when (version<= "28.1" emacs-version)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))

(use-package avy
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

(use-package ace-window)
(use-package transpose-frame)

(use-package eyebrowse
  :disabled t
  :custom (eyebrowse-keymap-prefix (kbd "C-\\"))
  :bind
  (("M-1" . eyebrowse-switch-to-window-config-1)
   ("M-2" . eyebrowse-switch-to-window-config-2)
   ("M-3" . eyebrowse-switch-to-window-config-3)
   ("M-4" . eyebrowse-switch-to-window-config-4))
  :config
  (eyebrowse-mode 1))

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

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook #'(lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Notmuch"
       (or
        (name . "\\*notmuch-")
        (mode . notmuch-search-mode)
        (mode . notmuch-show-mode)
        (mode . notmuch-message-mode)
        (mode . message-mode)))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (name . "^diary$")
        (mode . org-mode)))
      ("RANCID"
       (or
        (filename . "-rancid/")
        (mode . eos-mode)))
      ("DNS"
       (or
        (mode . dns-mode)
        (filename . "dns-zones/")))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired" (mode . dired-mode))
      ("SSH"   (filename . "^/ssh:*"))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "\\*magit")
        (name . "magit-")
        (name . "git-monitor")))
      ("Slack"
       (or
        (name . "^\\*Slack Log")
        (name . "^\\*Slack Event")
        (mode . slack-message-buffer-mode)
        (mode . slack-mode)))
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Emacs"
       (or
        (filename . ".emacs.d/emacs.org")
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)")))))))

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-follow-mouse 't         ;; scroll window under mouse
      mouse-wheel-progressive-speed nil   ;; don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control)))
      mouse-wheel-scroll-amount-horizontal 2)

(defun forge/save-all ()
  "Save any file-related buffers."
  (interactive)
  (message "Saving buffers at %s" (format-time-string "%Y-%m-%dT%T"))
  (save-some-buffers t))

;; If focus switches away, save all files.
(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'forge/save-all))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq backup-directory-alist (list (cons ".*" forge-backup-dir)) ;; make backups of files to the backup directory
      auto-save-file-name-transforms `((".*" ,forge-backup-dir t))   ;;
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 7      ;; Number of newest versions to keep when a new numbered backup is made.
      kept-old-versions 3      ;; Number of oldest versions to keep when a new numbered backup is made.
      auto-save-timeout 120
      auto-save-interval 1000)

(require 'savehist)
(with-eval-after-load 'savehist
  (setq savehist-file (expand-file-name "savehist" forge-state-dir)
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
        history-length 1000
        history-delete-duplicates t
  (add-hook 'after-init-hook #'savehist-mode))

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

(show-paren-mode)
(setq-default indent-tabs-mode nil
              fill-column 80
	      require-final-newline t)

(defun forge/join-next-line ()
  "Join the next line with the current line."
  (interactive)
  (join-line -1))

(global-set-key (kbd "M-j") 'forge/join-next-line)

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(use-package page-break-lines
  :diminish page-break-lines-mode)

(use-package yasnippet
  :diminish yasnippet-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" forge-personal-dir))
  (add-hook 'term-mode-hook (lambda () "Disable yasnippet in terminal" (setq yas-dont-activate t))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package hideshow
  :diminish hs-minor-mode
  :hook ((prog-mode) . hs-minor-mode)
  :bind (("C-c h" . hs-toggle-hiding)))

(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character))

(use-package recentf
  :bind ("<f7>" . consult-recent-file)
  :custom
  (recentf-save-file (expand-file-name "recentf" forge-state-dir))
  (recentf-max-menu-items 500)
  (recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "/tmp" "/ssh:"))
  :init
  (recentf-mode 1))

(defun dos2unix (buffer)
  "Do replacement of ^M characters with newlines in BUFFER."
  ;; This is basically: "M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))

(use-package flycheck
  :diminish flycheck-mode
  :custom (flycheck-global-modes '(not org-mode))
  :init (global-flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :diminish company-mode)

(use-package ediff
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(with-eval-after-load 'lisp-mode
  (add-hook 'before-save-hook #'forge/turn-on-delete-trailing-whitespace)
    (setq lisp-indent-offset nil))

(use-package eldoc
  :diminish eldoc-mode
  :hook
  (emacs-lisp-mode . eldoc-mode)
  (lisp-interaction-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.3))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc -f markdown_github+smart")
  :preface
  (defun orgtbl-to-gfm (table params)
    "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
    (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                                 org-table-last-alignment ""))
           (params2
            (list
             :splice t
             :hline (concat alignment "|")
             :lstart "| " :lend " |" :sep " | ")))
      (orgtbl-to-generic table (org-combine-plists params2 params))))

  (defun forge/insert-org-to-md-table (table-name)
    "Helper function to create markdown and orgtbl boilerplate."
    (interactive "*sEnter table name: ")
    (insert "<!---
#+ORGTBL: SEND " table-name " orgtbl-to-gfm

-->
<!--- BEGIN RECEIVE ORGTBL " table-name " -->
<!--- END RECEIVE ORGTBL " table-name " -->")
    (previous-line)
    (previous-line)
    (previous-line)))

(use-package python
  :interpreter ("python" . python-mode)
  :hook
  (python-mode . forge/turn-on-delete-trailing-whitespace)
  (python-mode . forge/whitespace-visualize)
  :config
  ;; set python-shell-interpreter to python3
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (setq-default python-indent-offset 4))

(use-package anaconda-mode
  :after python
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (expand-file-name "anaconda" forge-state-dir)))

(use-package company-anaconda
  :after anaconda-mode)

(use-package go-mode
  :mode "\\.go\\ '"
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package ess)

(with-eval-after-load 'shell-script-mode
  (add-hook 'shell-script-hook #'forge/whitespace-visualize)
  (add-hook 'shell-script-hook #'forge/turn-on-delete-trailing-whitespace))

(use-package web-mode
  :mode ("\\.html\\'" "\\.j2\\'")
  :init
  (setq web-mode-enable-auto-indentation nil ;; temporary for now.
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package php-mode
  :mode "\\.php\\'")

(use-package csv-mode
  :hook
  (csv-mode . forge/toggle-highlight-line)
  (csv-mode . forge/turn-on-delete-trailing-whitespace)
  (csv-mode . forge/whitespace-visualize))

(use-package json-mode
  :hook
  (json-mode . forge/turn-on-delete-trailing-whitespace)
  (json-mode . forge/whitespace-visualize))

(use-package yaml-mode
  :hook
  (yaml-mode . forge/turn-on-delete-trailing-whitespace)
  (yaml-mode . forge/whitespace-visualize)
  :config
  (setq yaml-indent-offset 2))

(with-eval-after-load 'nxml
  (defalias 'xml-mode 'nxml-mode)
  (autoload 'sgml-skip-tag-forward "sgml-mode")
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

(with-eval-after-load 'junos-mode
  (add-to-list 'magic-mode-alist '("!RANCID-CONTENT-TYPE: fujitsu_1finity" . junos-mode))
  (setq-local c-basic-offset 4))

(use-package eos-mode
  :quelpa (eos-mode :fetcher github :repo "sfromm/eos-mode")
  :commands (eos-mode)
  :magic ("!RANCID-CONTENT-TYPE: arista" . eos-mode)
  :hook (eos-mode . highlight-indent-guides-mode))

(use-package ledger-mode
  :commands ledger-mode)

(with-eval-after-load 'calendar
  (setq diary-file (expand-file-name "diary" org-directory)))

(require 'notifications)
(require 'tls)

(defun forge/jabber-notification (from buf text title)
  "Take a notification from jabber and send to `alert'.
Arguments are from the `jabber-alert-message-hooks' FROM, BUF, TEXT, and TITLE."
  (alert text :title title :id 'new-jabber-alert))

(use-package jabber
  :disabled t
  :preface
  (defun forge/jabber-start-or-switch ()
    "Connect to Jabber services"
    (interactive)
    (unless (get-buffer "*-jabber-roster-*")
      (jabber-connect-all))
    (if (or nil jabber-activity-jids)
        (jabber-activity-switch-to)
      (jabber-switch-to-roster-buffer)))
  :hook
  (jabber-post-connect . jabber-autoaway-start)
  :config
  ;; jabber-account-list is set via customize.
  (setq jabber-auto-reconnect t  ; reconnect automatically
        jabber-avatar-cache-directory (expand-file-name "jabber/avatar-cache" forge-state-dir)
        jabber-history-dir (expand-file-name "jabber" forge-log-dir)
        jabber-history-enabled t ; enable logging
        jabber-history-muc-enabled t
        jabber-use-global-history nil
        jabber-backlog-number 40
        jabber-backlog-days 30
        jabber-chat-buffer-show-avatar t ; show avatar in chat buffer
        jabber-vcard-avatars-retrieve t ; automatically download vcard avatars
        jabber-alert-info-message-hooks (quote (jabber-info-echo jabber-info-display))
        jabber-alert-message-hooks (quote (forge/jabber-notification jabber-message-echo jabber-message-scroll))
        jabber-alert-presence-hooks (quote ()) ; don't show anything on presence changes
        jabber-alert-muc-hooks (quote (jabber-muc-notifications-personal jabber-muc-echo jabber-muc-scroll)))
                                        ; jabber uses the fsm package
  (setq fsm-debug nil)       ; defaults to "*fsm-debug*"
  (dolist (hook '(jabber-chat-mode-hook jabber-roster-mode-hook))
    (add-hook hook (lambda () "Disable yasnippet in jabber" (setq yas-dont-activate t)))))

(with-eval-after-load 'erc
  (defun sf/erc-connect ()
    "Connect to IRC via ERC"
    (interactive)
    (when (y-or-n-p "Connect to libera? ")
      (erc-tls :server "irc.libera.chat" :port 6697))
    (when (y-or-n-p "Connect to bitlbee? ")
      (progn
        (use-package bitlbee :demand t)
        (bitlbee-start)
        (sleep-for 2)
        (erc :server "localhost" :port 6667))))

  (setq erc-nick user-login-name
        erc-server "irc.libera.chat"
        erc-away-nickname (concat erc-nick "|afk")
        erc-user-full-name erc-nick
        erc-fill-column 100
        erc-fill-static-center 16
        erc-fill-function 'erc-fill-static
        erc-modules '(autojoin autoaway button completion fill irccontrols
                               list log match menu move-to-prompt netsplit
                               networks notifications readonly ring
                               services smiley spelling stamp track))

  (erc-services-mode t)

  ;; use customize for `erc-keywords', and `erc-auto-join-channels-alist'
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-format "%H:%M:%S "
        erc-kill-buffer-on-part t         ;; kill buffer after channel /part
        erc-kill-server-buffer-on-quit t  ;; kill buffer for server messages after /quit
        erc-auto-discard-away t           ;; autoaway
        erc-autoaway-use-emacs-idle t
        ;; logging
        erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
        erc-log-channels-directory (expand-file-name "erc" forge-log-dir)
        erc-log-insert-log-on-open nil
        erc-prompt-for-nickserv-password nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "333" "353" "477")
        erc-save-buffer-on-part t))

(defun bitlbee-netrc-identify ()
  "Auto-identify for Bitlbee channels using authinfo or netrc.

   The entries that we look for in netrc or authinfo files have
   their 'port' set to 'bitlbee', their 'login' or 'user' set to
   the current nickname and 'server' set to the current IRC
   server's name.  A sample value that works for authenticating
   as user 'keramida' on server 'localhost' is:

   machine localhost port bitlbee login keramida password supersecret"
  (interactive)
  (when (string= (buffer-name) "&bitlbee")
    (let* ((secret (plist-get (nth 0 (auth-source-search :max 1
                                                         :host erc-server
                                                         :user (erc-current-nick)
                                                         :port "bitlbee"))
                              :secret))
           (password (if (functionp secret)
                         (funcall secret)
                       secret)))
      (erc-message "PRIVMSG" (concat (erc-default-target) " " "identify" " " password) nil))))
;; Enable the netrc authentication function for &biblbee channels.
(add-hook 'erc-join-hook 'bitlbee-netrc-identify)

(defvar forge-slack-client-id nil
  "Slack Client ID.")

(defvar forge-slack-client-token nil
  "Slack client token.")

(use-package slack
  :commands (slack-start)
  :bind (:map slack-mode-map
              ("C-c C-e" . slack-message-edit)
              ("C-c C-k" . slack-channel-leave)
              ("@" . slack-message-embed-mention)
              ("#" . slack-message-embed-channel))
  :init
  (setq slack-buffer-emojify t
        slack-prefer-current-team t))

(with-eval-after-load 'dired
  (diminish 'dired-omit-mode)

  (define-key global-map (kbd "C-c d") 'dired-jump)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "Y") 'forge/dired-rsync)
  (define-key dired-mode-map (kbd "^") 'forge/dired-up)

  (defun forge/dired-mode-hook ()
    "Set misc settings in dired mode."
    (setq-local truncate-lines t)
    (forge/toggle-highlight-line))

  (defun forge/dired-up ()
    "Move up a directory without opening a new buffer."
    (interactive)
    (find-alternate-file ".."))

  (put 'dired-find-alternate-file 'disabled nil)
  (when (forge/system-type-darwin-p)
    (setq dired-use-ls-dired nil))

  (setq dired-dwim-target t
        dired-ls-F-marks-symlinks t
        dired-listing-switches "-laFh1v --group-directories-first"
        ;; -F (classify), -h (human readable), -1 (one file per line), -v (version sorting)
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        global-auto-revert-non-file-buffers t) ;; auto refresh dired buffers

  ;; This requires installing coreutils via homebrew
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t)))

(setq auto-revert-verbose nil)

(use-package async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(with-eval-after-load 'dired-x
  (setq dired-bind-jump nil
        dired-guess-shell-alist-user (list '("\\.\\(mkv\\|mpe?g\\|avi\\|mp3\\|mp4\\|ogm\\|webm\\)$" "mpv")
                                           '("\\.\\(docx?\\|xlsx?\\|kmz\\)$" "open")
                                           '("\\.pdf$" "open")))
  (global-unset-key (kbd "C-x C-j"))
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

(defun forge/maybe-convert-directory-to-rsync-target (directory)
  "Adapt dired target DIRECTORY in case it is a remote target.

If directory starts with /scp: or /ssh: it is probably a tramp
target and should be converted to rsync-compatible destination
string, else we do (shell-quote-argument (expand-file-name
directory)) as is required for normal local targets acquired with
read-file-name and dired-dwim-target-directory."
  (if (or (string-prefix-p "/scp:" directory)
	  (string-prefix-p "/ssh:" directory))
      ;; - throw out the initial "/scp:" or "/ssh:"
      ;; - replace spaces with escaped spaces
      ;; - surround the whole thing with quotes
      ;; TODO: double-check that target ends with "/""
      ;; which in the case of DWIM is what we want
      (prin1-to-string
       (replace-regexp-in-string "[[:space:]]" "\\\\\\&"
	                         (substring directory 5)))
    (shell-quote-argument (expand-file-name directory))))

(defun forge/dired-rsync (dest)
  (interactive
   (list
    (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        ;; the rsync command
        (forge/rsync-command "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq forge/rsync-command (concat forge/rsync-command (shell-quote-argument file) " ")))
    ;; append the destination
    (setq forge/rsync-command (concat forge/rsync-command (forge/maybe-convert-directory-to-rsync-target dest)))
    ;; run the async shell command
    (async-shell-command forge/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

;;(use-package emacs-conflict :commands (emacs-conflict-show-conflicts-dired))

(use-package disk-usage)

(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :custom
  (magit-push-always-verify nil)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine-toggle)
  :commands git-timemachine)

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (setq nov-save-place-file (expand-file-name "nov-places" forge-state-dir)))

(with-eval-after-load 'go-jira
  (defvar jira-token nil)
  (defun jira-create ()
    "Create a ticket in Jira."
    (interactive)
    (unless jira-token
      (setq jira-token (lookup-password "go-jira.atlassian.net" user-login-name 6697)))
    (setenv "JIRA_API_TOKEN" jira-token)
    (require 'with-editor)
    (start-process "go-jira" (get-buffer-create " *go-jira*")
                   "jira" "create" "-b"
                   "--editor"
                   (concat with-editor-emacsclient-executable " -s " server-socket-dir "/server"))))

(require 'message)
(with-eval-after-load 'message
  (defun forge/mail-toggle-forward-mime ()
    "Toggle whether to forward as MIME or inline."
    (interactive)
    (if (bound-and-true-p message-forward-as-mime)
        (setq message-forward-as-mime nil)
      (setq message-forward-as-mime t)))
  (setq mail-from-style 'angles
        message-kill-buffer-on-exit t
        message-forward-as-mime t
        message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:"
        message-citation-line-function (quote message-insert-formatted-citation-line)
        message-make-forward-subject-function (quote message-forward-subject-fwd)
        message-signature t
        message-signature-file "~/.signature"
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail)
  (add-hook 'message-mode-hook #'footnote-mode)
  (add-hook 'message-mode-hook #'turn-on-flyspell)
  (add-hook 'message-mode-hook #'yas-minor-mode)
  (add-hook 'message-mode-hook #'turn-on-auto-fill
            (lambda()
              (auto-fill-mode t)
              (setq fill-column 72)
              (setq mail-header-separator ""))))

(require 'mm-decode)
(with-eval-after-load 'mm-decode
  (setq mm-text-html-renderer 'shr
        mm-inline-large-images nil
        mm-inline-text-html-with-images nil
        mm-discouraged-alternatives '("text/html" "text/richtext")))

(use-package gnus-alias
  :custom
  (gnus-alias-default-identity "work")
  :hook
  (message-setup . gnus-alias-determine-identity))

(add-hook 'dired-mode #'turn-on-gnus-dired-mode)
(with-eval-after-load
    (setq gnus-dired-mail-mode 'notmuch-user-agent))

(defun forge/mail-toggle-compose-new-frame ()
  "Toggle whether to compose email in new frame."
  (interactive)
  (let ((frame "same"))
    (if (boundp 'notmuch-mua-compose-in)
        (if (eq notmuch-mua-compose-in 'current-window)
            (progn (setq frame "new") (setq notmuch-mua-compose-in 'new-frame))
          (setq notmuch-mua-compose-in 'current-window))
      (if mu4e-compose-in-new-frame
          (setq mu4e-compose-in-new-frame nil)
        (progn (setq mu4e-compose-in-new-frame t) (setq frame "new"))))
    (message "Compose mail in %s frame" frame)))

(use-package boxquote)

(defun forge/mail-forward-complaint (template)
  "Forward an abuse complaint using TEMPLATE."
  (interactive)
  (if (boundp 'notmuch-mua-compose-in) (notmuch-show-forward-message) (mu4e-compose 'forward))
  (message-goto-body)
  (yas-expand-snippet (yas-lookup-snippet template))
  (message-add-header (concat "Cc: " forge-mail-abuse-poc))
  (message-goto-to))

(defun forge/mail-forward-abuse-complaint ()
  "Forward an abuse complaint to responsible party."
  (interactive)
  (forge/mail-forward-complaint "abuse-template"))

(defun forge/mail-forward-infringement-complaint ()
  "Forward a infringement complaint to responsible party."
  (interactive)
  (forge/mail-forward-complaint "infringement-template"))

(defun forge/mail-forward-spam-complaint ()
  "Forward a spam complaint to responsible party."
  (interactive)
  (forge/mail-forward-complaint "spam-template"))

(defun forge/mail-forward-compromised-complaint ()
  "Forward a compromised account report to responsible party."
  (interactive)
  (forge/mail-forward-complaint "compromise-template"))

(defun forge/mail-reply-to-abuse ()
  "Set Reply-To header to Abuse POC."
  (interactive)
  (message-add-header (concat "Reply-To: " forge-mail-abuse-poc)))

(defun forge/mail-reply-to-noc ()
  "Set Reply-To header to NOC POC."
  (interactive)
  (message-add-header (concat "Reply-To: " forge-mail-noc-poc)))

(defun forge/mail-org-notes ()
  "Send org notes as an email."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-mime-org-subtree-htmlize)
    (message-goto-to)))

(defhydra forge/hydra-email (:color blue)
  "

  _A_ Forward Abuse report  _S_  Forward Spam report  _N_  Toggle compose New frame
  _I_ Forward Infringement  _C_  Comporomised report  _W_  Save all attachments
  _sd_ Search last day      _sw_ Search last week     _sm_ Search last month
  _ra_ Reply to Abuse POC   _rn_ Reply to NOC POC     _O_ Email Org notes
  "
  ("A" forge/mail-forward-abuse-complaint)
  ("ra" forge/mail-reply-to-abuse)
  ("rn" forge/mail-reply-to-noc)
  ("sd" notmuch-search-last-day)
  ("sw" notmuch-search-last-week)
  ("sm" notmuch-search-last-month)
  ("O" forge/mail-org-notes)
  ("I" forge/mail-forward-infringement-complaint)
  ("S" forge/mail-forward-spam-complaint)
  ("C" forge/mail-forward-compromised-complaint)
  ("W" forge/notmuch-save-all-attachments)
  ("N" forge/mail-toggle-compose-new-frame))

(global-set-key (kbd "C-c m") 'forge/hydra-email/body)

(with-eval-after-load 'sendmail
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header
        sendmail-program (executable-find "sendmail.py")))

(use-package smtpmail
  :disabled t
  :config
  (setq smtpmail-stream-type 'ssl
        smtpmail-default-smtp-server forge-smtp-server-work
        smtpmail-smtp-server forge-smtp-server-work
        smtpmail-smtp-service 465
        smtpmail-smtp-user forge-smtp-user-work
        smtpmail-queue-dir (expand-file-name "queue" forge-state-dir)))

(with-eval-after-load 'mml-sec
  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-smime-encrypt-to-self t
        mml-secure-smime-sign-with-sender t)
  (add-to-list 'mml-secure-openpgp-signers "A852499F"))

(use-package notmuch
  :ensure nil
  :commands (notmuch)
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-hello-thousands-separator ",")
  (notmuch-crypto-process-mime t)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)

  :preface
  (defmacro forge-notmuch-show-tag (tags)
    "Macro to take list of tags and apply to query."
    `(progn
       (notmuch-show-add-tag ,tags)
       (unless (notmuch-show-next-open-message)
         (notmuch-show-next-thread t))))

  (defmacro forge-notmuch-search-tag (tags)
    "Macro to take list of tags and apply to query."
    `(progn
       (notmuch-search-tag ,tags)
       (notmuch-search-next-thread)))

  (defmacro forge-notmuch-show-toggle-tag (tag)
    "Macro to toggle presence of tag for query."
    `(progn
       (if (member ,tag (notmuch-show-get-tags))
           (notmuch-show-remove-tag (list (concat "-" ,tag)))
         (notmuch-show-add-tag (list (concat "+" ,tag))))))

  (defmacro forge-notmuch-search-toggle-tag (tag)
    "Macro to toggle presence of tag for query."
    `(progn
       (if (member ,tag (notmuch-search-get-tags))
           (notmuch-search-tag (list (concat "-" ,tag)))
         (notmuch-search-tag (list (concat "+" ,tag))))))

  (defun notmuch-search-attachment (ext)
    "Search for attachments with extension EXT.

You can provide a space-delimited list of extensions to search for.
Will open a notmuch search buffer of the search results."
    (interactive "sExtension: ")
    (notmuch-search
     (mapconcat 'identity
                (mapcar (lambda (arg) (concat "attachment:" arg)) (split-string ext)) " or ")))

  (defun notmuch-search-recent (period &optional query)
    "Search for recent mail for time period PERIOD.

Prompts for  QUERY and this will amend the search to
limit it to the provided time PERIOD.
Will open a notmuch search buffer of the search results."
    (let* ((query (or query (notmuch-read-query "Query: "))))
      (notmuch-search (concat "date:" period ".. AND " query))))

  (defun notmuch-search-last-day (&optional query)
    "Search recent mail for prompted query.

Search notmuch for QUERY and this will amend the search to
limit it to the last day.
Will open a notmuch search buffer of the search results."
    (interactive)
    (notmuch-search-recent "1d" query))

  (defun notmuch-search-last-week (&optional query)
    "Search recent mail for prompted query.

Search notmuch for QUERY and this will amend the search to
limit it to the last 7 days.
Will open a notmuch search buffer of the search results."
    (interactive)
    (notmuch-search-recent "7d" query))

  (defun notmuch-search-last-month (&optional query)
    "Search recent mail for prompted query.

Search notmuch for QUERY and this will amend the search to
limit it to the last month.
Will open a notmuch search buffer of the search results."
    (interactive)
    (notmuch-search-recent "1M" query))

  :bind
  (:map notmuch-show-mode-map
        ("g" . notmuch-refresh-this-buffer)
        ("y" . notmuch-show-archive-message-then-next-or-next-thread)
        ("Y" . notmuch-show-archive-thread-then-next)
        ("d" . (lambda ()
                 "mark message for trash"
                 (interactive)
                 (forge-notmuch-show-tag (list "+trash" "-inbox" "-unread" "-archive"))))
        ("I" . (lambda ()
                 "mark message for inbox and delete trash, if present."
                 (interactive)
                 (forge-notmuch-show-tag (list "-trash" "+inbox"))))
        ("J" . (lambda ()
                 "mark message as junk"
                 (interactive)
                 (forge-notmuch-show-tag (list "+bulk" "+trash" "-inbox" "-unread" "-archive"))))
        ("F" . (lambda ()
                 "toggle message as flagged"
                 (interactive)
                 (forge-notmuch-show-toggle-tag "flagged")))
        ("M" . (lambda ()
                 "toggle message as muted"
                 (interactive)
                 (forge-notmuch-show-toggle-tag "mute")))
        ("b" . (lambda (&optional address)
                 "Bounce the current message"
                 (interactive "sBounce to: ")
                 (notmuch-show-view-raw-message)
                 (message-resend address)))
        ("S-SPC" . notmuch-show-rewind))
  (:map notmuch-search-mode-map
        ("g" . notmuch-refresh-this-buffer)
        ("y" . notmuch-search-archive-thread)
        ("Y" . notmuch-search-archive-thread)
        ("d" . (lambda ()
                 "mark thread for trash"
                 (interactive)
                 (forge-notmuch-search-tag (list "+trash" "-inbox" "-unread" "-archive"))))
        ("I" . (lambda ()
                 "mark message for inbox and delete trash tag, if present."
                 (interactive)
                 (forge-notmuch-search-tag (list "-trash" "+inbox"))))
        ("J" . (lambda ()
                 "mark thread as junk"
                 (interactive)
                 (forge-notmuch-search-tag (list "+bulk" "+trash" "-inbox" "-unread" "-archive"))))
        ("F" . (lambda ()
                 "toggle thread as flagged"
                 (interactive)
                 (forge-notmuch-search-toggle-tag "flagged")))
        ("M" . (lambda ()
                 "toggle thread as muted"
                 (interactive)
                 (forge-notmuch-search-toggle-tag "mute")))
        ("S-SPC" . notmuch-search-scroll-down))
  (:map notmuch-show-part-map
        ("c" . forge/notmuch-show-calendar-invite))

  :config
  (add-hook 'notmuch-show-hook '(lambda () (setq show-trailing-whitespace nil)))
  (setq notmuch-archive-tags '("-unread" "-inbox" "-trash" "-bulk" "-spam")
        notmuch-saved-searches '(( :name "📥 Inbox"
                                   :key "i"
                                   :query "tag:inbox")
                                 ( :name "🚩 Flagged"
                                   :key "f"
                                   :query "tag:flagged or tag:important")
                                 ( :name "📅 Today"
                                   :key "t"
                                   :query "date:24h.. and ( tag:inbox or tag:unread )")
                                 ( :name "💬 Unread"
                                   :key "u"
                                   :query "tag:unread")
                                 ( :name "Sent"
                                   :key "s"
                                   :query "tag:sent")
                                 ( :name "3 days"
                                   :key "3"
                                   :query "date:3d..  and ( tag:inbox or tag:unread )")
                                 ( :name "Last 7 days"
                                   :key "7"
                                   :query "date:7d..  and ( tag:inbox or tag:unread )")
                                 ( :name "Last 30 days"
                                   :key "m"
                                   :query "date:1M..1d and ( tag:inbox or tag:unread )")
                                 ( :name "Old messages"
                                   :key "o"
                                   :query "date:..1M and ( tag:inbox or tag:bulk or tag:unread ) ")
                                 ( :name "Attachments"
                                   :key "A"
                                   :query "tag:attachment")
                                 ( :name "Bulk"
                                   :key "B"
                                   :query "tag:unread and ( tag:bulk or tag:spam )")
                                 ( :name "Meeting Invites"
                                   :key "c"
                                   :query "mimetype:text/calendar"))))

(defun forge/twiddle-luminance (value)
  "Twiddle the luminance value to VALUE."
  (interactive "nLuminance: ")
  (message "Current luminance level: %s" shr-color-visible-luminance-min)
  (setq shr-color-visible-luminance-min value))

(defcustom forge-attachment-dir
  (expand-file-name "Downloads" "~/")
  "Directory to save attachments from email."
  :group 'forge
  :type 'string)

(defun forge/mu4e-save-all-attachments (&optional msg)
  "Save all attachments in MSG to attachment directory.
The sub-directory in `forge-attachment-dir' is derived from the subject of the email message."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (subject (message-wash-subject (mu4e-message-field msg :subject)))
         (attachdir (concat forge-attachment-dir "/" subject))
         (count (hash-table-count mu4e~view-attach-map)))
    (if (> count 0)
        (progn
          (mkdir attachdir t)
          (dolist (num (number-sequence 1 count))
            (let* ((att (mu4e~view-get-attach msg num))
                   (fname (plist-get att :name))
                   (index (plist-get att :index))
                   fpath)
              (setq fpath (expand-file-name (concat attachdir "/" fname)))
              (mu4e~proc-extract
               'save (mu4e-message-field msg :docid)
               index mu4e-decryption-policy fpath))))
      (message "Nothing to extract"))))

;; This is derived in part from notmuch-show-save-attachments
;; but calls mm-save-part-to-file instead so as to save files without prompting.
(defun forge/notmuch-save-all-attachments ()
  "Save all attachments in MSG to attachment directory."
  (interactive)
  (let* ((subject (message-wash-subject (notmuch-show-get-subject)))
         (attachdir (concat (file-name-as-directory forge-attachment-dir) subject)))
    (with-current-notmuch-show-message
     (let ((mm-handle (mm-dissect-buffer)))
       (message "%s" subject)
       (mkdir attachdir t)
       (notmuch-foreach-mime-part
        (lambda (p)
          (let ((disposition (mm-handle-disposition p)))
            (and (listp disposition)
                 (or (equal (car disposition) "attachment")
                     (and (equal (car disposition) "inline")
                          (assq 'filename disposition)))
                 (mm-save-part-to-file
                  p (concat (file-name-as-directory attachdir) (cdr (assq 'filename disposition)))))))
        mm-handle)))))

(defun forge/mail-add-calendar-invite (handle &optional prompt)
  "Open calendar ICS part in Calendar."
  (ignore prompt)
  (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (mm-add-meta-html-tag handle)
    (let ((path (expand-file-name "~/Download/invite.ics")))
      (mm-write-region (point-min) (point-max) path nil nil nil 'binary t)
      (start-process "add-calendar-invite" nil "/usr/bin/open" "-a" "/Applications/Microsoft Outlook.app" path))))

(defun forge/notmuch-show-calendar-invite ()
  "Save ics MIME part."
  (interactive)
  (notmuch-show-apply-to-current-part-handle #'forge/mail-add-calendar-invite))

(defun forge/mail-open-html ()
  "Open HTML part in browser."
  (interactive)
  (with-current-notmuch-show-message
      (let ((mm-handle (mm-dissect-buffer)))
        (notmuch-foreach-mime-part
         (lambda (p)
           (if (string-equal (mm-handle-media-type p) "text/html")
               (mm-display-part p "open")))
         ;;             (notmuch-show-view-part)))
         ;;             (notmuch-show-apply-to-current-part-handle #'mm-display-part)))
         mm-handle))))

(define-prefix-command 'my-mail-search-map)
(define-key my-mail-search-map (kbd "s") 'notmuch-search)
(define-key my-mail-search-map (kbd "r") 'notmuch-search-last-week)
(define-key my-mail-search-map (kbd "m") 'notmuch-search-last-month)
(define-key my-mail-search-map (kbd "A") 'notmuch-search-attachment)

(define-prefix-command 'my-mail-map)
(define-key my-mail-map (kbd "N") 'forge/mail-toggle-compose-new-frame)
(define-key my-mail-map (kbd "W") 'forge/notmuch-save-all-attachments)
(define-key my-mail-map (kbd "ra") 'forge/mail-reply-to-abuse)
(define-key my-mail-map (kbd "rn") 'forge/mail-reply-to-noc)
(define-key my-mail-map (kbd "s") 'my-mail-search-map)
(global-set-key (kbd "C-c m") 'my-mail-map)

(use-package mingus
  :disabled t
  :preface
  (defun forge/get-current-song-mpd ()
    "Get the current song playing via MPD."
    (interactive)
    (let ((conn (mpd-conn-new "localhost" 6600))
          (cursong nil))
      (condition-case nil
          (setq cursong (split-string (plist-get (mpd-get-current-song conn) 'Title) " - "))
        (error nil))
      cursong)))

(use-package emms
  :custom
  (emms-directory (expand-file-name "emms" forge-state-dir))
  (emms-source-file-default-directory (expand-file-name "~/annex/Audio"))
  :config
  (emms-all)
  (emms-history-load)
  (setq emms-player-list (list emms-player-mpv)
        emms-stream-info-backend 'mplayer
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
        emms-browser-covers 'emms-browser-cache-thumbnail)
  (add-to-list 'emms-player-mpv-parameters "--no-audio-display")
  (add-to-list 'emms-info-functions 'emms-info-cueinfo)
  (if (executable-find "emms-print-metadata")
      (progn
        (require 'emms-info-libtag)
        (add-to-list 'emms-info-functions 'emms-info-libtag)
        (delete 'emms-info-ogginfo emms-info-functions)
        (delete 'emms-info-mp3info emms-info-functions))
    (add-to-list 'emms-info-functions 'emms-info-ogginfo)
    (add-to-list 'emms-info-functions 'emms-info-mp3info)))

(use-package net-utils
  :commands (ping traceroute)
  :config
  (setq ping-program-options (list "-c" "5"))
  (setq traceroute-program-options (list "-I" "-m" "30" "-w" "1")))

(use-package org
  :preface
  (defun forge/org-fixed-font-faces ()
    "Keep the following with fixed-pitch fonts."
    (interactive)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

  (defun forge/tangle-org-mode-on-save ()
    "Tangle org-mode file when saving."
    (when (string= (message "%s" major-mode) "org-mode")
      (org-babel-tangle)))

  (defun forge/org-mode-hook ()
    "Turn on settings for org-mode."
    (interactive)
    (when (fboundp 'turn-on-auto-fill)
      (turn-on-auto-fill))
    (when (fboundp 'turn-on-flyspell)
      (turn-on-flyspell)))

  (defun forge/org-set-property (property value)
    "Set arbitrary PROPERTY to VALUE for current heading."
    (org-back-to-heading)
    (when (not (org-element-property :CREATED (org-element-at-point)))
      (org-set-property property value)))

  (defun forge/org-set-uuid ()
    "Set ID property for current headline."
    (interactive)
    (forge/org-set-property "ID" (org-id-uuid)))

  (defun forge/org-set-created ()
    "Set CREATED property for current headline."
    (interactive)
    (forge/org-set-property "CREATED" (with-temp-buffer (org-insert-time-stamp (current-time) t t))))

  (defun forge/org-timer-clock-in ()
    "Clock in when starting a org-timer."
    (if (eq major-mode 'org-agenda-mode)
	(call-interactively 'org-agenda-clock-in)
      (call-interactively 'org-clock-in)))

  (defun forge/org-timer-clock-out ()
    "Clock in when starting a org-timer."
    (if (eq major-mode 'org-agenda-mode)
	(call-interactively 'org-agenda-clock-out)
      (call-interactively 'org-clock-out)))

  (defun forge/org-set-properties ()
    "Set stock org properties for current headline."
    (interactive)
    (forge/org-set-uuid)
    (forge/org-set-created))

  (defun forge/org-clip-web-page ()
    "Clip web page for org capture."
    (interactive)
    (when (derived-mode-p 'eww-mode)
      (require 'ol-eww)
      (org-eww-copy-for-org-mode)
      (concat
       "* %a %? :ARTICLE:
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\")
:CREATED:  %U
:URL:      " (eww-current-url) "
:END:\n\n" (car kill-ring))))


  :hook
  ((org-mode . forge/org-mode-hook)
   (after-save . forge/tangle-org-mode-on-save)
   (org-timer-set . forge/org-timer-clock-in)
   (org-timer-done . forge/org-timer-clock-out)
   (org-timer-stop . forge/org-timer-clock-out)
   (org-mode . variable-pitch-mode))

  :custom
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-attach-id-dir "~/annex/org/data/")
  (org-attach-method 'mv)
  (org-babel-python-command "python3")
  (org-catch-invisible-edits 'smart)
  (org-clock-display-default-range 'thisweek)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist t)
  (org-clock-sound (expand-file-name "drip.ogg" "~/annex/Music/"))
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (concat org-directory "/journal.org"))
  (org-ellipsis "⤵")
  (org-export-allow-bind-keywords t)
  (org-export-backends '(ascii html icalendar latex md))
  (org-export-coding-system 'utf-8)
  (org-html-checkbox-type 'html)
  (org-log-done t)
  (org-log-reschedule "note")
  (org-log-into-drawer t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '(("inbox.org" :maxlevel . 2)
                        ("agenda.org" :maxlevel . 3)
                        ("articles.org" :maxlevel . 3)
                        ("notebook.org" :maxlevel . 5)
                        ("work.org" :maxlevel . 5)
                        ("personal.org" :maxlevel . 5)))
  (org-reverse-note-order t)
  (org-src-fontify-natively t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-timer-default-timer 25)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

  :bind
  (("<f8>" . org-cycle-agenda-files)
   ("<f12>" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb))
  (:map org-mode-map
        ("M-q" . endless/fill-or-unfill)
        ("RET" . org-return))

  :init
  (setq org-directory "~/forge")
  (setq org-file-apps (quote ((auto-mode . emacs)
                              ("\\.doc\\'" . "open %s")
                              ("\\.docx\\'" . "open %s")
                              ("\\.xlsx\\'" . "open %s")
                              ("\\.pptx\\'" . "open %s")
                              ("\\.pdf\\'" . default))))
  (setq org-structure-template-alist '(("a" . "export ascii")
                                       ("c" . "center")
                                       ("C" . "comment")
                                       ("e" . "example")
                                       ("E" . "export")
                                       ("h" . "export html")
                                       ("l" . "src emacs-lisp")
                                       ("m" . "export md")
                                       ("p" . "src python")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("v" . "verse")
                                       ("y" . "src yaml")))
  (setq org-agenda-sticky t
        org-agenda-hide-tags-regexp "."
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-compact-blocks t
        org-agenda-files (list
                          (concat org-directory "/inbox.org")
                          (concat org-directory "/agenda.org")
                          (concat org-directory "/journal.org")
                          (concat org-directory "/work.org")
                          (concat org-directory "/personal.org"))
        org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo   . " %i %-12:c")
                                   (tags   . " %i %-12:c")
                                   (search . " %i %-12:c")))
  ;; There's a lot to org-agenda-custom-commands
  ;; For type:
  ;;   type     The command type, any of the following symbols:
  ;;     agenda      The daily/weekly agenda.
  ;;     todo        Entries with a specific TODO keyword, in all agenda files.
  ;;     search      Entries containing search words entry or headline.
  ;;     tags        Tags/Property/TODO match in all agenda files.
  ;;     tags-todo   Tags/P/T match in all agenda files, TODO entries only.
  ;;     todo-tree   Sparse tree of specific TODO keyword in *current* file.
  ;;     tags-tree   Sparse tree with all tags matches in *current* file.
  ;;     occur-tree  Occur sparse tree for *current* file.
  (setq org-agenda-custom-commands '(("g" "Agenda"
                                      ((tags-todo "inbox"
                                                  ((org-agenda-overriding-header "Inbox\n")
                                                   (org-agenda-prefix-format "  %?-12t% s")))

                                       (agenda ""
                                               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                                                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("SOMEDAY")))
                                                (org-deadline-warning-days 0)))

                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "\nNext\n")
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                                              (org-agenda-prefix-format "  %i %-12:c [%e] ")))

                                       (todo "WAITING"
                                             ((org-agenda-overriding-header "\nWaiting\n")
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                                              (org-agenda-prefix-format "  %i %-12:c [%e] ")))

                                       (tags "CLOSED>=\"<-10d>\""
                                             ((org-agenda-overriding-header "\nCompleted last 10 days\n")))))))
  ;;  (agenda nil
  ;;          ((org-agenda-entry-types '(:deadline))
  ;;           (org-agenda-format-date "")
  ;;           (org-deadline-warning-days 7)
  ;;           (org-agenda-skip-function
  ;;            '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
  ;;           (org-agenda-overriding-header "\nDeadlines")))



  ;; For template expansion,
  ;; see https://orgmode.org/manual/Template-expansion.html#Template-expansion
  (setq org-capture-templates '(("l" "Log" entry
                                 (file+olp+datetree "~/forge/journal.org")
                                 "* %U - %?\n" )

                                ("i" "Inbox" entry
                                 (file "inbox.org")
                                 "* TODO %? \n:PROPERTIES:\n:CAPTURED:  %U\n:END:\nReference: %a\n")

                                ("c" "Calendar invite" entry
                                 (file+headline "agenda.org" "Future")
                                 (function notmuch-calendar-capture-event) :prepend t)

                                ("n" "Meeting notes" entry
                                 (file+olp+datetree "~/forge/journal.org")
                                 "* Notes - %a \n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n%U\nAttendees:\n\nAgenda:\n\nDiscussion:\n" :clock-in t :clock-resume t)

                                ("j" "Journal" entry
                                 (file+olp+datetree "~/forge/journal.org")
                                 "* %?\n%U\n" :clock-in t :clock-resume t)

                                ("b" "Bookmark" entry
                                 (file+headline "~/forge/notebook.org" "Unfiled")
                                 "* %^L %^g \n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n" :prepend t)

                                ("m" "Music" entry
                                 (file+olp+datetree "~/forge/journal.org")
                                 "* %(forge/capture-current-song) :music:\n%U\n")

                                ("r" "Reference")
                                ("rr" "Reference" entry
                                 (file+olp+datetree "~/forge/articles.org")
                                 "* %a %?\n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n" :prepend t)

                                ("rw" "Web Page" entry
                                 (file+olp+datetree "~/forge/articles.org")
                                 (function forge/org-clip-web-page) :prepend t)

                                ("rf" "Elfeed/News Article" entry
                                 (file+olp+datetree "~/forge/articles.org")
                                 "* %a %? :%(forge/elfeed-get-entry-tags):ARTICLE:\n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n" :prepend t)

                                ("rt" "Twitter Post" entry
                                 (file+olp+datetree "~/forge/articles.org")
                                 "* %a %? :TWITTER:\n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n" :prepend t)))

  ;; Workflow states
  ;; https://orgmode.org/manual/Workflow-states.html#Workflow-states
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "SOMEDAY(m)" "|" "DONE(d)" "DELEGATED(l)" "CANCELLED(c)")
                            (sequence "PROJECT" "|" "DONE(d)")
                            (sequence "|" "MEETING" "REFERENCE(r)")))
  ;; List of tags that should never be inherited.
  (setq org-tags-exclude-from-inheritance '("crypt"))
  ;;
  (setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM"
        org-modules '(org-id ol-eww ol-docview ol-info ol-irc org-habit)
        org-src-preserve-indentation t
        org-src-window-setup 'current-window                    ;; use current window when editing a source block
        org-cycle-separator-lines 2                             ;; leave this many empty lines in collapsed view
        org-table-export-default-format "orgtbl-to-csv"         ;; export tables as CSV instead of tab-delineated
        org-publish-project-alist '(("public"
                                     :base-directory "~/forge"
                                     :publishing-directory "~/Documents")))
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)
                                                           (dot . t)
                                                           (emacs-lisp . t)
                                                           (org . t)
                                                           (perl . t)
                                                           (python . t)
                                                           (ruby . t)
                                                           (shell . t)
                                                           (calc . t)))

  (forge/org-fixed-font-faces))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package ol-notmuch      ;; use package from site-lisp
  :after (:any org notmuch))

(use-package org-mime
  :config
  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))
  :init
  (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))

(use-package org-contacts   ;; use package from site-lisp
  :after org
  :config
  (setq org-contacts-files (list  "~/forge/contacts.org"))
  (add-to-list 'org-capture-templates '("C" "Contacts" entry
                                        (file "~/forge/contacts.org")
                                        "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:PHONE:\n:ADDRESS:\n:BIRTHDAY:\n:END:")))

(use-package org-tree-slide
  :bind (:map org-tree-slide-mode-map
              ("<f8>" . org-tree-slide-mode)
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree))
  :init
  (setq org-tree-slide-skip-outline-level 4))

(with-eval-after-load 'org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save t
        org-crypt-key user-full-name))

(with-eval-after-load 'org-id
  (setq org-id-method 'uuid
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-id-locations-file (expand-file-name "org/id-locations.el" forge-state-dir)))

;;(use-package ol-git-link :straight (org-contrib :includes ol-git-link))

(use-package ol-eww
  :ensure nil
  :after org)

;; support links to manual pages
(use-package ol-man
  :ensure nil
  :after org)

(use-package ox-md
  :ensure nil
  :after org)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package htmlize)

(use-package ox-twbs
  :commands (org-twbs-export-to-html
             org-twbs-export-as-html
             org-twbs-convert-region-to-html))

;; (use-package ox-reveal
;;   :after org-compat
;;   :custom (org-reveal-note-key-char nil))

;; (use-package ox-tufte :after org)

(defun forge/capture-current-song ()
  "Capture the current song details."
  (let ((itunes-song (forge/get-current-song-itunes))
        (mpd-song (when (fboundp 'forge/get-current-song-mpd) (forge/get-current-song-mpd)))
        (song-info nil))
    (setq song-info (if itunes-song itunes-song mpd-song))
    (concat (car song-info) ", \"" (car (cdr song-info)) "\"")))

(defun forge/org-set-lastupdated ()
  "Set LASTUPDATED property to today."
  (interactive)
  (org-set-property "LASTUPDATED" (format-time-string (org-time-stamp-format nil t))))

(defun forge/org-table-export (name)
  "Search for table named `NAME` and export"
  (interactive "sTable: ")
  (outline-show-all)
  (push-mark)
  (goto-char (point-min))
  (let ((case-fold-search t))
    (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
        (progn
          (forward-line)
          (org-table-export (format "%s.csv" name) "orgtbl-to-csv"))))
  (pop-mark))

;; via https://vxlabs.com/2018/10/29/importing-orgmode-notes-into-apple-notes/
(defun forge/org-html-publish-to-html-for-apple-notes (plist filename pub-dir)
  "Convert exported files to format that plays nicely with Apple Notes. Takes PLIST, FILENAME, and PUB-DIR."
  ;; https://orgmode.org/manual/HTML-preamble-and-postamble.html
  ;; disable author + date + validate link at end of HTML exports
  ;;(setq org-html-postamble nil)

  (let* ((org-html-with-latex 'imagemagick)
         (outfile
          (org-publish-org-to 'html filename
                              (concat "." (or (plist-get plist :html-extension)
                                              org-html-extension
                                              "html"))
                              plist pub-dir)))
    ;; 1. apple notes handles <p> paras badly, so we have to replace all blank
    ;;    lines (which the orgmode export accurately leaves for us) with
    ;;    <br /> tags to get apple notes to actually render blank lines between
    ;;    paragraphs
    ;; 2. remove large h1 with title, as apple notes already adds <title> as
    ;; the note title
    (shell-command (format "sed -i \"\" -e 's/^$/<br \\/>/' -e 's/<h1 class=\"title\">.*<\\/h1>$//' %s" outfile)) outfile))

(defun forge/tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)
  (org-babel-tangle)
  (kill-buffer))

(defun forge/tangle-files (path &optional full)
  "Tangle files in PATH (directory), FULL for absolute paths.
Example: (forge/tangle-files \"~/.emacs.d/*.org\")."
  (interactive)
  (mapc 'forge/tangle-file (forge/get-files path full)))

(defun forge/get-files (path &optional full)
  "Return list of files in directory PATH that match glob pattern, FULL for absolute paths."
  (directory-files (file-name-directory path)
                   full
                   (eshell-glob-regexp (file-name-nondirectory path))))

(defun my/migrate-datetree-entry ()
  "Take an org entry from a datetree outline and migrate to an org-journal file.

The general intent behind this function is that it will migrate the current heading
and advance to the next heading.  One can then bind it to a macro for the repetition piece.
It will not remove entries from the source org file."
  (interactive)
  (org-beginning-of-line)
  (let* ((heading (nth 4 (org-heading-components)))
         (tags (nth 5 (org-heading-components)))
         (year (format-time-string "%Y" (apply 'encode-time (org-parse-time-string heading))))
         (time (format-time-string "%H:%M" (apply 'encode-time (org-parse-time-string heading))))
         (day (format-time-string "%A, %d %B %Y" (apply 'encode-time (org-parse-time-string heading))))
         (subject (when (string-match "\] *\\(.*\\)" heading) (match-string 1 heading)))
         (day-heading (format "* %s" day))
         (jrnl-heading (format "** %s %s   %s" time subject (or tags ""))))
    (org-copy-subtree)
    (with-current-buffer year
      (my/migrate-datetree-goto-heading day-heading)
      (message "%s" jrnl-heading)
      (insert (format "%s\n" jrnl-heading))
      (org-paste-subtree)
      (kill-line 1))
    (forward-line)
    ;; go to next datetree heading
    (re-search-forward "^\\*\\*\\*\\* \\[" nil t)))

(defun my/migrate-journal-entry ()
  "Migrate an org-journal entry."
  (interactive)
  (org-beginning-of-line)
  (if (= 1 (nth 1 (org-heading-components)))
      (org-next-visible-heading 1)
    (let* ((heading (nth 4 (org-heading-components)))
           (time (when (string-match "\\(..:..\\)" heading) (match-string 1 heading))))
      (push-mark)
      (outline-up-heading 1)
      (let* ((date (org-entry-get (point) "CREATED"))
             (date-heading (nth 4 (org-heading-components)))
             (weekday (when (string-match "\\(.*\\)," date-heading) (match-string 1 date-heading)))
             (shortday (when (string-match "\\(...\\)" weekday) (match-string 1 weekday)))
             (month (when (string-match ", [0-9]+ \\(.*\\) " date-heading) (match-string 1 date-heading)))
             (year (when (string-match "\\(....\\)" date) (match-string 1 date)))
             (monthint (when (string-match "....\\(..\\).." date) (match-string 1 date)))
             (dayint (when (string-match "......\\(..\\)" date) (match-string 1 date)))
             (month-heading (format "** %s-%s %s" year monthint month))
             (day-heading (format "*** %s-%s-%s %s" year monthint dayint weekday))
             (org-ts (format "[%s-%s-%s %s %s]" year monthint dayint shortday time)))
        (pop-global-mark)
        (org-copy-subtree)
        (with-current-buffer "journal.org"
          (my/migrate-datetree-goto-heading month-heading)
          (message "month heading %s" month-heading)
          (my/migrate-datetree-goto-heading day-heading)
          (message "day heading %s" day-heading)
          (org-paste-subtree 4)
          (forward-line)
          (insert (format "%s\n" org-ts)))
        (org-next-visible-heading 1)))))

(defun my/migrate-datetree-goto-heading (heading)
  "Go to day heading HEADING in org-journal file.  Create if it doesn't exist."
  (interactive)
  (goto-char (point-min))
  (unless (search-forward heading nil t)
    (progn (goto-char (point-max))
           (insert (format "%s\n" heading))
           (goto-char (point-min))))
  (search-forward heading nil t)
  (goto-char (point-max)))

(use-package org-journal
  :disabled t
  :preface
  (defun org-journal-find-location ()
    "Open today's journal file."
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    ;; This should also get org-mode to the right place to add a heading at the correct depth
    (org-journal-new-entry t)
    (goto-char (point-max)))
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  ;; (goto-char (point-max)))

  :init
  (setq org-journal-dir (concat org-directory "/journal/")
        org-journal-file-type 'yearly
        org-journal-file-format "%Y"
        org-journal-date-format "%A, %d %B %Y"))

(when (forge/system-type-darwin-p)
  (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
  (setq epa-pinentry-mode 'loopback))

(use-package auth-source)

(use-package auth-source-pass
  :after auth-source
  :init
  (setq auth-sources '(password-store "~/.authinfo.gpg")))

;; https://github.com/ccrusius/auth-source-xoauth2
(use-package auth-source-xoauth2
  :after auth-source)

;;
(use-package pass
  :config
  (advice-add 'pass-quit :after 'forge/delete-window))

;; https://github.com/ecraven/ivy-pass
(use-package ivy-pass
  :bind
  ("C-c p" . ivy-pass))

(use-package elfeed
  :commands (elfeed)
  :bind
  (:map elfeed-search-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("d" . elfeed-search-youtube-dl)
        ("f" . forge/elfeed-search-toggle-starred)
        ("o" . elfeed-search-mpv)
        ("J" . elfeed-unjam)
        ("S" . forge/elfeed-search-save-db)
        ("R" . forge/elfeed-search-mark-all-read)
        ("F" . forge/elfeed-search-starred)
        ("U" . forge/elfeed-search-unread)
        ("<" . forge/elfeed-search-first-article)
        (">" . forge/elfeed-search-last-article)
        :map elfeed-show-mode-map
        ("j" . elfeed-show-next)
        ("k" . elfeed-show-prev)
        ("d" . elfeed-show-youtube-dl)
        ("e" . elfeed-show-open-eww)
        ("f" . forge/elfeed-show-toggle-starred)
        ("o" . elfeed-show-mpv))
  :preface
  (defun forge/elfeed-load-db ()
    "Wrapper to load elfeed database from disk when running elfeed."
    (elfeed-db-load))

  (advice-add 'elfeed :before #'forge/elfeed-load-db)

  (defun forge/elfeed-stop-timer ()
    "Cancel elfeed-update-timer."
    (interactive)
    (when elfeed-update-timer (cancel-timer elfeed-update-timer)))

  (defun forge/elfeed-start-timer ()
    "Start elfeed-update-timer."
    (interactive)
    (setq elfeed-update-timer (run-at-time 180 (* 120 60) 'forge/elfeed-update)))

  :config
  (defun elfeed-search-mpv ()
    "Play the current entry with mpv"
    (interactive)
    (message "url %s" (elfeed-entry-link (car (elfeed-search-selected))))
    (start-process "*elfeed-mpv*" nil "mpv" (elfeed-entry-link (car (elfeed-search-selected)))))

  (defun elfeed-show-mpv ()
    "Play the current entry with mpv"
    (interactive)
    (start-process "*elfeed-mpv*" nil "mpv" (elfeed-entry-link elfeed-show-entry)))

  (defun elfeed--youtube-dl (entry)
    "Download a video for ENTRY via youtube-dl."
    (if (null (youtube-dl (elfeed-entry-link entry)
                          :title (elfeed-entry-title entry)
                          ;; elfeed-feed-author will return a list of plist that will look like ((:name "HappyBlog" :uri "https://example.com/happyblog"))
                          :directory (concat youtube-dl-directory "/" (plist-get (car (elfeed-feed-author (elfeed-entry-feed entry))) :name))))
        (message "Entry is not a youtube link")
      (message "Downloading %s" (elfeed-entry-title entry))))

  ;; from skeeto
  ;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
  (defun elfeed-search-youtube-dl ()
    "Download the current entry/entries with youtube-dl"
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (elfeed--youtube-dl entry)
        (elfeed-untag entry 'unread)
        (elfeed-search-update-entry entry)
        (unless (use-region-p) (forward-line)))))

  ;; from skeeto
  ;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
  (defun elfeed-show-youtube-dl ()
    "Download the current entry with youtube-dl"
    (interactive)
    (elfeed--youtube-dl elfeed-show-entry))

  (defun forge/elfeed-entry-tags ()
    "Return entry tags as a string."
    (interactive)
    (let ((entry))
      (if (eq major-mode 'elfeed-show-mode)
          (setq entry elfeed-show-entry)
        (setq entry (car (elfeed-search-selected))))
      (upcase (mapconcat #'symbol-name (elfeed-entry-tags entry) ":"))))

  (defun forge/elfeed-get-entry-tags ()
    "hello"
    (interactive)
    (with-current-buffer "*elfeed-entry*"
      (forge/elfeed-entry-tags)))

  (defun elfeed-show-open-eww ()
    "Open the current entry with eww."
    (interactive)
    (eww (elfeed-entry-link elfeed-show-entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t))

  (defun forge/elfeed-search-starred ()
    "Show starred elfeed articles"
    (interactive)
    (elfeed-search-set-filter "+starred"))

  (defun forge/elfeed-search-unread ()
    "Show elfeed articles tagged with unread"
    (interactive)
    (elfeed-search-set-filter "@6-months-ago +unread"))

  (defun forge/elfeed-search-save-db ()
    "Save elfeed database to disk."
    (interactive)
    (elfeed-db-save)
    (message "elfeed db saved."))

  ;; from manuel uberti
  ;; https://manuel-uberti.github.io/emacs/2017/08/01/elfeed/
  (defun forge/elfeed-search-mark-all-read ()
    "Mark all articles as read."
    (interactive)
    (call-interactively 'mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defalias 'forge/elfeed-search-toggle-starred (elfeed-expose #'elfeed-search-toggle-all 'starred))

  (defun forge/elfeed-show-toggle-starred ()
    "Toggle starred tag for elfeed article"
    (interactive)
    (forge/elfeed-show-toggle-tag 'starred))

  (defun forge/elfeed-show-toggle-tag (tag)
    "Toggle tag for elfeed article."
    (interactive)
    (if (elfeed-tagged-p tag elfeed-show-entry)
        (elfeed-show-untag tag)
      (elfeed-show-tag tag)))

  (defun forge/elfeed-update ()
    "Update elfeed database."
    (message "Updating elfeed articles...")
    (elfeed-update)
    (elfeed-db-save))

  (defun forge/elfeed-search-first-article ()
    "Go to first message in search."
    (interactive)
    (goto-char (point-min)))

  (defun forge/elfeed-search-last-article ()
    "Go to last message in search."
    (interactive)
    (goto-char (point-max)))

  (defface elfeed-search-starred-title-face '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)

  (elfeed-org)
  (setq url-queue-timeout 30
        elfeed-db-directory (expand-file-name "elfeed" (concat (getenv "HOME") "/annex/var"))))
        ;; create timer to update elfeed
        ;; elfeed-update-timer (run-at-time 180 (* 120 60) 'forge/elfeed-update)))

(use-package elfeed-org
  :after (:all org elfeed)
  :commands (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory)))
  (elfeed-org))

(with-eval-after-load 'youtube-dl
  (setq youtube-dl-directory "~/annex/Video/youtube"))

(use-package rg)

(defun forge/multi-term-here ()
  "Open up a multi-term window."
  (interactive)
  (let* ((parent (if (buffer-file-name) (file-name-directory (buffer-file-name)) (getenv "HOME")))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (multi-term)))

(use-package multi-term
  :config
  (setq multi-term-program "/bin/zsh"))

(with-eval-after-load 'em-unix
  (unintern 'eshell/su nil)
  (unintern 'eshell/sudo nil))

(with-eval-after-load 'eshell
  (define-key global-map (kbd "C-!") 'eshell-here)

  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the current buffer's file."
    (interactive)
    (let* ((parent (if (buffer-file-name) (file-name-directory (buffer-file-name)) (getenv "HOME")))
           (height (/ (window-total-height) 3))
           (name (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))
      (insert (concat "ls"))
      (eshell-send-input)))

  (setenv "TERM" "xterm-256color")
  (setq explicit-shell-file-name "/bin/bash") ;; this is from term.el
  (advice-add 'eshell-life-is-too-much :after 'forge/delete-window)
  (setq tramp-default-method "ssh"
        eshell-directory-name (expand-file-name "eshell" forge-state-dir)
        eshell-visual-commands '("less" "tmux" "htop" "top" "docker" "nethack")
        eshell-visual-subcommands '(("git" "log" "diff" "show"))
        eshell-prompt-function (lambda ()
                                 (concat
                                  "┌─["
                                  (user-login-name) "" (system-name)
                                  " " (abbreviate-file-name (eshell/pwd))
                                  " | " (format-time-string "%a %b %d %H:%M" (current-time))
                                  "]\n"
                                  "└─>" (if (= (user-uid) 0) " # " " $ "))) )
  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell/alias "q" "exit")
                                (eshell/alias "l" "ls -al")
                                (eshell/alias "ll" "ls -al")
                                (eshell/alias "e" "find-file \$1")
                                (eshell/alias "ff" "find-file \$1")
                                (eshell/alias "vi" "find-file \$1")
                                (eshell/alias "d" "dired \$1")
                                (eshell/alias "ee" "find-file-other-window \$1")
                                (eshell/alias "gd" "magit-diff-unstaged")
                                (eshell/alias "gds" "magit-diff-staged")
                                (eshell/alias "gst" "magit-status"))))

(defun forge/terminal ()
  "Switch to terminal; launch if non-existent."
  (interactive)
  (if (get-buffer "*ansi-term*")
    (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/bash"))
  (get-buffer-process "*ansi-term*"))

(use-package mastodon
  :custom
  (mastodon-client--token-file (expand-file-name "mastodon/mastodon.plstore" forge-state-dir))
  :config
  (mastodon-discover))

(use-package lorem-ipsum)

(use-package gist
  :custom (gist-view-gist t))

(quelpa '(wttrin :fetcher github :repo "sfromm/emacs-wttrin"))
(use-package wttrin
  :quelpa (wttrin :fetcher github :repo "sfromm/emacs-wttrin")
  :commands (wttrin)
  :custom
  (wttrin-default-cities '("Eugene" "Portland" "Sonoma" "Kapolei" "New Orleans"))
  (wttrin-language "en-US"))

(use-package with-editor)

(forge/load-directory-modules forge-personal-dir)
