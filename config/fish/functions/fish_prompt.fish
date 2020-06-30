function fish_prompt
  if test "$status" != "0" 
    set_color e27878
    echo -n '!'
  end
  if jobs -q
    set_color e27878
    echo '★'
  end
  if test "$IN_NIX_SHELL" = "impure" 
    set_color 84a0c6
    prompt_echo 'λ'
  else if test "IN_NIX_SHELL" = "pure"
    set_color e27878
    prompt_echo 'λ'
  else 
    set_color normal
    prompt_echo '$'
  end
end

function echo_replicate --description "Echo the character for each running instance of a shell"  
  echo -n (seq 1 (ps | grep fish | wc -l) | tr -d '\n' | sed "s/[0-9]/$argv/g")
end

function prompt_echo --description "Echo the character and append <space> to it"
  echo_replicate $argv
  echo -n ' '
end

