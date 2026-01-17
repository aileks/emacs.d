;;; -*- lexical-binding: t -*-

(use-package all-the-icons
  :demand t)

(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\\\" "{-" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "---" "-->" "->" "->>"
     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
     "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|="
     "|^" "$>" "++" "+++" "+>" "=:" "==" "===" "==>" "=>"
     "=>>" "<=" "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-"
     ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>" "<!--"
     "<-" "<--" "<->" "<+" "<+>" "<=" "<==" "<=>" "<=<"
     "<>" "<<" "<<-" "<<=" "<<<" "<~" "<~~" "</" "</>"
     "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode 't))

(set-face-attribute 'default nil :family "Monaspace Neon NF")
(set-face-attribute 'font-lock-comment-face nil :family "Monaspace Radon NF")
(set-face-attribute 'font-lock-doc-face nil :family "Monaspace Argon NF")
