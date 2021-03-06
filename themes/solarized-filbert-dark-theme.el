;;; solarized-filbert-dark-theme.el ---  filbert palettes   -*- no-byte-compile: t; -*-

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

;;; Commentary:

;;; Code:
(require 'solarized)

(defvar solarized-filbert-dark-color-palette-alist
  '(;; solarized-filbert palette
    (base03      . "#282828")
    (base02      . "#3c3836")
    (base01      . "#928374")
    (base00      . "#a89984")
    (base0       . "#d5c4a1")
    (base1       . "#ebddb2")
    (base2       . "#fef9e0")
    (base3       . "#fefcef")
    (yellow      . "#b58900")
    (orange      . "#cb4b16")
    (red         . "#dc322f")
    (magenta     . "#d33682")
    (violet      . "#6c71c4")
    (blue        . "#268bd2")
    (cyan        . "#2aa198")
    (green       . "#859900")
    (yellow-1bg  . "#273532")
    (yellow-1fg  . "#af8f41")
    (yellow-2bg  . "#433e20")
    (yellow-2fg  . "#b39a5e")
    (yellow-d    . "#866300")
    (yellow-l    . "#e1af4b")
    (orange-1bg  . "#2b2d2e")
    (orange-1fg  . "#ca6f48")
    (orange-2bg  . "#4d2c1f")
    (orange-2fg  . "#c47c5d")
    (orange-d    . "#992700")
    (orange-l    . "#fb7640")
    (red-1bg     . "#2d2c31")
    (red-1fg     . "#d66556")
    (red-2bg     . "#532725")
    (red-2fg     . "#ce7667")
    (red-d       . "#a7020a")
    (red-l       . "#ff6849")
    (magenta-1bg . "#272d3c")
    (magenta-1fg . "#cc6791")
    (magenta-2bg . "#4c2942")
    (magenta-2fg . "#c47896")
    (magenta-d   . "#a00559")
    (magenta-l   . "#ff699e")
    (violet-1bg  . "#0c3144")
    (violet-1fg  . "#8085c0")
    (violet-2bg  . "#1a365a")
    (violet-2fg  . "#888dbc")
    (violet-d    . "#243e9b")
    (violet-l    . "#8d85e7")
    (blue-1bg    . "#003547")
    (blue-1fg    . "#5c93c5")
    (blue-2bg    . "#003f5e")
    (blue-2fg    . "#709bc3")
    (blue-d      . "#0061a8")
    (blue-l      . "#74adf5")
    (cyan-1bg    . "#013841")
    (cyan-1fg    . "#54a099")
    (cyan-2bg    . "#00464a")
    (cyan-2fg    . "#6ba8a2")
    (cyan-d      . "#007d76")
    (cyan-l      . "#6ccec0")
    (green-1bg   . "#1d3732")
    (green-1fg   . "#8c9a43")
    (green-2bg   . "#2f4321")
    (green-2fg   . "#97a35f")
    (green-d     . "#5b7300")
    (green-l     . "#b3c34d")
    ;; palette end
    )
  "The solarized filbert color palette alist.")

(deftheme solarized-filbert-dark
  "The dark variant of the Solarized colour theme with filbert color palette")

(solarized-with-color-variables 'dark 'solarized-filbert-dark
  solarized-filbert-dark-color-palette-alist)

(provide-theme 'solarized-filbert-dark)

(provide 'solarized-filbert-dark-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-filbert-dark-theme.el ends here
