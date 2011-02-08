(ns imparsonate.lib
  (use imparsonate.core)
  )


(defparser double-quoted-string
  :root ["\"" #"[^\"]*" "\""])
