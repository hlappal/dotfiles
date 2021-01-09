;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-

;; Comment line
(map! :leader
      :desc "Comment line"
      "c l" #'comment-line)

;; Treemacs
(map! :leader
      :desc "Toggle Treemacs"
      "t t" #'treemacs)

;; Helm-bibtex
(map! :leader
      :desc "Helm-bibtex"
      "m b" #'helm-bibtex)

;; Dired image preview
(after! dired-jump
  (map! :localleader
        :nv "i" #'peep-dired))
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
