(autoload 'domtool-mode "domtool-mode" "\
Major Mode for editing Domtool files." t nil)

(add-to-list 'auto-mode-alist '("\\(\\.dtl\\'\\|/\\.domtool/\\)" . domtool-mode))

(provide 'domtool-mode-startup)
