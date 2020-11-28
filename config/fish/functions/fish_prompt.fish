function fish_prompt
  set status_before $status


  set_color white ; echo -n [

  set dir (basename (pwd))
  set_color green ; echo -n "$dir"


  set branch (git rev-parse --abbrev-ref HEAD 2>/dev/null)
  if test -n "$branch"
    set_color white ; echo -n :
    set_color green ; echo -n "$branch"
  end

  set_color white ; echo -n ]

  if test "$status_before" != "0" 
    set_color e27878 ; echo -n 'ᚧ'
  end

  if jobs -q
    set_color e27878 ; echo -n 'ᛃ'
  end
  if test "$IN_NIX_SHELL" = "impure" 
    set_color 84a0c6 ; prompt_echo 'λ'
  else if test "IN_NIX_SHELL" = "pure"
    set_color e27878 ; prompt_echo 'λ'
  else 
    set_color normal ; prompt_echo '$'
  end 
end

function echo_replicate --description "Echo the character for each running instance of a shell"  
  echo -n (seq 1 (ps | grep fish | wc -l) | tr -d '\n' | sed "s/[0-9]/$argv/g")
end

function prompt_echo --description "Echo the character and append <space> to it"
  echo_replicate $argv
  echo -n ' '
end

