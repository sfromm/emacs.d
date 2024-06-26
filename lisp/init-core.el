;;; init-core.el --- Init Core -*- lexical-binding: t -*-
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

(defgroup forge nil
  "Forge custom settings."
  :group 'environment)


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
    (setopt bookmark-default-file (expand-file-name "bookmarks" forge-state-dir))
    (setopt calc-settings-file (expand-file-name "calc-settings.el" forge-state-dir))
    (setopt transient-history-file (expand-file-name "transient/history.el" forge-state-dir))
    (setopt transient-levels-file (expand-file-name "transient/levels.el" forge-personal-dir))
    (setopt transient-values-file (expand-file-name "transient/values.el" forge-personal-dir))
    (setopt message-auto-save-directory (expand-file-name "messages" forge-state-dir))
    (setopt package-quickstart-file (expand-file-name "package-quickstart.el" forge-state-dir))
    (setopt project-list-file (expand-file-name "project-list.el" forge-state-dir))
    (setopt tramp-auto-save-directory (expand-file-name "tramp/auto-save" forge-state-dir))
    (setopt tramp-persistency-file-name (expand-file-name "tramp/persistency.el" forge-state-dir))
    (setopt url-cache-directory (expand-file-name "url/cache/" forge-state-dir))
    (setopt url-configuration-directory (expand-file-name "url/configuration/" forge-state-dir))))

(init-mkdirs-user-emacs-directory)
(init-clean-user-emacs-directory)

(when (forge/native-comp-p)
  (setq native-comp-deferred-compilation nil
        native-comp-async-report-warnings-errors 'silent
        package-native-compile nil))

(add-to-list 'load-path forge-site-dir)
(add-to-list 'custom-theme-load-path forge-themes-dir)

(setopt inhibit-splash-screen t)
(setopt initial-major-mode 'org-mode)
(setopt initial-scratch-message
        (concat
         "#+TITLE: Scratch Buffer\n\n"
         "* Welcome to Emacs\n"
         "[[file:~/.emacs.d/emacs.org][emacs.org]]\n"
         "#+begin_src emacs-lisp\n"
         "#+end_src"))

(setopt visible-bell t)                 ;; set a visible bell ...
(setopt ring-bell-function #'ignore)    ;; and squash the audio bell
(setopt load-prefer-newer t)            ;; always load the newer version of a file
(setopt large-file-warning-threshold 50000000) ;; warn when opening files bigger than 50MB

(when (display-graphic-p)
  (when (forge/system-type-darwin-p)
    (setopt frame-resize-pixelwise t))  ;; allow frame resizing by pixels, instead of character dimensions
  (line-number-mode t)                ;; show line number in modeline
  (column-number-mode t)              ;; show column number in modeline
  (size-indication-mode t)            ;; show buffer size in modeline
  (tool-bar-mode -1)                  ;; disable toolbar
  (scroll-bar-mode -1)                ;; disable scroll bar
  (display-battery-mode))


(require 'savehist)
(with-eval-after-load 'savehist
  (setq savehist-file (expand-file-name "savehist" forge-state-dir)
        savehist-save-minibuffer-history 1
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
        history-length 1000
        history-delete-duplicates t
  (add-hook 'after-init-hook #'savehist-mode))


(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(setopt mouse-wheel-follow-mouse 't)         ;; scroll window under mouse


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


;;; exec-path-from-shell
;;; Set exec-path based on shell PATH.
;;; Some platforms, such as MacOSX, do not get this done correctly.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(when (forge/system-type-darwin-p)
  (dolist (path (list "/usr/local/bin" (expand-file-name "~/bin")))
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (setenv "PATH" (concat path ":" (getenv "PATH")))
      (add-to-list 'exec-path path))))


;; dbus is a linux thing -- only load on that platform
(when (forge/system-type-linux-p)
  (require 'dbus))


(defun macos-networksetup-status ()
  "Run networksetup to get network status."
  (let* ((output (shell-command-to-string "networksetup -listnetworkserviceorder | grep 'Hardware Port'"))
         (netsetup (split-string output "\n")))
    (catch 'found
      (dolist (elt netsetup)
        (when (> (length elt) 0)
          (let* ((netifseq (string-match "Device: \\([a-z0-9]+\\))" elt))
                 (netif (match-string 1 elt)))
            (when (string-match "status: active" (shell-command-to-string (concat "ifconfig " netif " | grep status")))
              (throw 'found netif))))))))

(defun linux-networkmanager-status ()
  "Query NetworkManager for network status."
  (let ((nm-service "org.freedesktop.NetworkManager")
        (nm-path "/org/freedesktop/NetworkManager")
        (nm-interface "org.freedesktop.NetworkManager")
        (nm-state-connected-global 70))
    (eq nm-state-connected-global
        (dbus-get-property :system nm-service nm-path nm-interface "State"))))

(defun forge/network-online-p ()
  "Check if we have a working network connection"
  (interactive)
  (when (forge/system-type-linux-p)
    (linux-networkmanager-status))
  (when (forge/system-type-darwin-p)
    (macos-networksetup-status)))


;; What follows are various helper functions that are used
;; either interactively or in other parts of the configuration.

(defun my-reload-emacs-configuration ()
  "Reload emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

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

(defun forge/untabify-buffer ()
  "Remove tab characters from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun sudo-file-path (file)
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

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (sudo-file-path file)))

(defun sudo-this-file ()
  "Open current file as root."
  (interactive)
  (find-file
   (sudo-file-path
    (or buffer-file-name
        (or buffer-file-name
            (when (or (derived-mode-p 'dired-mode)
                      (derived-mode-p 'wdired-mode))
              default-directory))))))


(when (forge/system-type-darwin-p)
  (defun my-get-current-song-itunes ()
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


(provide 'init-core)
