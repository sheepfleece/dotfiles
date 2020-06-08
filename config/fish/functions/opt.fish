function opt

  eval "$argv[1] --help 2>&1 | grep -- $argv[2]"

end
