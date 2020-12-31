xmodmap -e 'clear lock'
xmodmap -e 'keycode 9 = Caps_Lock NoSymbol Caps_Lock'
xmodmap -e 'keycode 66 = Escape NoSymbol Escape'

feh --bg-fill  '/home/sheep/Pictures/1320743621940.jpg'  
xbindkeys

PATH="${PATH}:/home/sheep/dotfiles/scripts"
mpdscribble
