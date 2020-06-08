function fish_prompt
  if test "$status" != "0" 
    set_color e27878
    echo -n '!'
  end
  if jobs -q
    set_color e27878
    echo -n '★'
  end
  if test "$IN_NIX_SHELL" = "impure" 
    set_color 84a0c6
    echo -n 'λ '
  else if test "IN_NIX_SHELL" = "pure"
    set_color e27878
    echo -n 'λ '
  else 
    set_color normal
    echo '$ '
  end
end
