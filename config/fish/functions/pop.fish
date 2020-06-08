function pop
  if ! set -q argv[1]
    set argv[1] 1
  end
  for x in (seq "$argv[1]")
    cd ..
  end
end
