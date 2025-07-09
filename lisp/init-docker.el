;;; init-docker.el --- Init docker/podman integration -*- lexical-binding: t -*-
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

(defun my-toggle-docker-podman-command ()
  "Toggle between docker and podman environment."
  (interactive)
  (if (string= docker-command "docker")
      (setopt docker-command "podman"
              docker-compose-command "podman-compose"
              docker-container-tramp-method "podman")
    (setopt docker-command "docker"
            docker-compose-command "docker-compose"
            docker-container-tramp-method "docker"))
  (message "container env %s" docker-command))

(use-package docker
  :bind ("C-c d" . docker)
  :custom
  ;; Set defaults for the following variables
  ;; The above function allows toggling between docker & podman.
  (docker-command "docker")
  (docker-compose-command "docker-compose")
  (docker-container-tramp-method "docker"))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(provide 'init-docker)
