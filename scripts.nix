{ pkgs, ... }:

let 
  touchpad-toggle = pkgs.writeScriptBin "touchpad-toggle" ''
    #!/bin/sh

    device='SYNA3081:00 06CB:826F Touchpad'

    # true || echo 1 && echo 2 == (true || echo 1) && echo 2 == 2 
    # in C it is the other way around
    [[ "$(xinput list-props "$device" | grep -P ".*Device Enabled.*\K.(?=$)" -o)" == "1" ]] &&
        xinput disable "$device" ||
        xinput enable "$device"
  '';

in 
{
    home.packages = [ touchpad-toggle ];
}
