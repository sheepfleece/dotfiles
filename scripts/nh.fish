function nh
  set tfile (mktemp)

  set link $argv
  wget $link --directory-prefix $tfile

  set line (rg --line-number Pages $tfile/index.html | sed 's/\:.*//')
  set line (math $line + 1)

  set pages (cat $tfile/index.html | sed $line!d | sed -e 's/<[^>]*>//g')
  set pages (math $pages)

  wget $link/1/ --directory-prefix $tfile
  set images (rg --no-line-number -o 'https://i\.nhentai.net/galleries/[0-9]*/' $tfile/index.html.1)

  set name (rg 'itemprop' $tfile/index.html | sed -e  's/.*<meta itemprop="name" content="//g' | sed -e 's/".*//g')
  mkdir $name 

  wget -e robots=off (for i in (seq 1 $pages); echo $images/$i.jpg; echo $images/$i.png; end) --directory-prefix "$PWD/$name"  2&>/dev/null

  rm $tfile/index.html $tfile/index.html.1
  rmdir $tfile

end
