;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Load custom keybindings specified in file 'bindings.el'
(load! "bindings")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Heikki Lappalainen"
      user-mail-address "heikki.lappalainen@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default: (setq doom-theme 'doom-nord)
;; Available themse:
;;   doom-one
;;   doom-one-light
;;   doom-vibrant
;;   doom-acario-dark
;;   doom-acario-light
;;   doom-city-lights
;;   doom-challenger-deep
;;   doom-dark+
;;   doom-dracula
;;   doom-ephemeral
;;   doom-fairy-floss
;;   doom-flatwhite
;;   doom-gruvbox
;;   doom-gruvbox-light
;;   doom-henna
;;   doom-horizon
;;   doom-Iosvkem
;;   doom-laserwave
;;   doom-material
;;   doom-manegarm
;;   doom-miramare
;;   doom-molokai
;;   doom-monokai-classic
;;   doom-monokai-pro
;;   doom-moonlight
;;   doom-nord
;;   doom-nord-light
;;   doom-nova
;;   doom-oceanic-next
;;   doom-old-hope
;;   doom-opera
;;   doom-opera-light
;;   doom-outrun-electric
;;   doom-palenight
;;   doom-plain
;;   doom-peacock
;;   doom-rouge
;;   doom-snazzy
;;   doom-solarized-dark
;;   doom-solarized-light
;;   doom-sourcerer
;;   doom-spacegrey
;;   doom-tomorrow-day
;;   doom-tomorrow-night
;;   doom-wilmersdorf
;;   doom-zenburn
;;   (unavailable) doom-mono-dark
;;   (unavailable) doom-mono-light
;;   (unavailable) doom-tron
(setq doom-theme 'doom-Iosvkem)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; NOTE: DROPBOX POISSA KÄYTÖSTÄ
;;(setq org-directory "~/Dropbox/org/")

;; Open Org Agenda at startup
;;(add-hook 'after-init-hook 'org-agenda-list)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Helm-bibtex
(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq bibtex-completion-bibliography
      '("./bibliography.bib"))

;; Start with minimap enabled
;; (minimap-mode 1)

;; Enable minimap in LaTeX mode
;;(setq minimap-major-modes '(prog-mode latex-mode))

;; Set Emacs to save temporarily active regions (selected with mouse) into
;; primary selection
(setq mouse-drag-copy-region t)
(setq select-enable-primary t)
;; (setq select-enable-clipboard nil)
(setq select-active-regions t)

;; Tabnine config
(add-to-list 'company-backends #'company-tabnine)
(setq company-idle-delay 0)
(setq company-show-numbers t)

;; ;; Config Emacs as X window manager
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)
;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-monitor-plist '(0 "LVDS1"))
;; (add-hook 'exwm-randr-screen-change-hook
;;          ;(lambda ()
;;            ;(start-process-shell-command
;;             ;"xrandr" nil "xrandr --output LVDS1 --mode 1600x900 --pos 0x0 --rotate normal")))
;; (exwm-randr-enable)
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

;; ;; Mu4e Configurations
;; ;;
;; ;; Contexts function
;; (defun my-make-mu4e-context (name address signature)
;;   "Return a mu4e context named NAME with :match-func matching
;; its ADDRESS in From or CC fields of the parent message. The
;; context's `user-mail-address' is set to ADDRESS and its
;; `mu4e-compose-signature' to SIGNATURE."
;;   (lexical-let ((addr-lex address))
;;     (make-mu4e-context :name name
;;                        :vars `((user-mail-address . ,address)
;;                                (mu4e-compose-signature . ,signature))
;;                        :match-func
;;                        (lambda (msg)
;;                          (when msg
;;                            (or (mu4e-message-contact-field-matches msg :to addr-lex)
;;                                (mu4e-message-contact-field-matches-msg :cc addr-lex)))))))
;; ;; Define contexts
;; (setq mu4e-contexts
;;       `( ,(my-make-mu4e-context "Gmail" "heikki.olavi.lappalainen@gmail.com"
;;                                 "")
;;          ,(my-make-mu4e-context "Aalto" "heikki.2.lappalainen@aalto.fi"
;;                                 "")))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
