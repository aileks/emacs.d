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

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'italic)
