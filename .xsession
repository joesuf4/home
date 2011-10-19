LANG="en_US.UTF-8"
export LANG
PATH=$PATH:$HOME/bin
floatbg &
numlockx on
xrdb -load .Xresources
xset r rate 500 30
xinput set-int-prop 6 "Middle Button Emulation" 8 1
mixer vol 100:100
mixer mic 100:100
mixer igain 50:50
ssh-agent fvwm2
