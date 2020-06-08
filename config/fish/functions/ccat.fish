function ccat
  
  highlight --out-format=ansi "$argv" 2>/dev/null || cat "$argv"

end
