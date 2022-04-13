# dotfiles-dasboss-pc

## dotFilez repo
- [The best way to store your dotfiles: A bare Git repository](https://www.atlassian.com/git/tutorials/dotfiles)
- [A simpler way to manage your dotfiles - ANAND IYER](https://www.anand-iyer.com/blog/2018/a-simpler-way-to-manage-your-dotfiles.html)

## backup config

```bash
config add <fichier/répertoire>
config commit -m "Add config pour xyz"
config push
```

## restore
[gist](https://gist.github.com/mdupuis13/3376d8f73ee4e66e5364f991efcd5428)

## special config

### Logitech M570 trackbakll mouse
Ajouter le fichier `/etc/X11/xorg.conf.d/10-libinput.conf`

```
Section "InputClass"
    # User-defined name for this profile/input class
    Identifier      "Logitech M570"
    # Tailed /var/log/Xorg.0.log to figure out the following
    MatchProduct    "Logitech M570" 
    Driver          "libinput"
    ## OPTIONS
    Option "ScrollMethod" "button"
    Option "ScrollButton" "8"
    Option "MiddleEmulation" "on"
    Option "SendCoreEvents" "true"
    # EmulateWheel refers to emulating a mouse wheel using the trackball
    Option "EmulateWheel" "true"
    # Set to middle-click
    Option "EmulateWheelButton" "8"
    # Affects distance trackball needs to move register scroll movement 
    Option "EmulateWheelInertia" "10"
    # Timeout between EmulateWheelButton click and "emulation" to begin
    Option "EmulateWheelTimeout" "200"
    # Comment out XAxis if you don't want horizontal scroll
    Option "ZAxisMapping" "4 5"
    Option "XAxisMapping" "6 7"
EndSection
```
Et/ou le fichier `/etc/X11/xorg.conf.d/99-elecom-huge-scroll.conf`
``` 
Section "InputClass"
    Identifier   "Elecom HUGE scroll config"
    MatchDriver  "libinput"
    MatchVendor  "ELECOM"
    MatchProduct "HUGE TrackBall"
    Option       "ScrollMethod" "button"
    Option       "ScrollButton" "9"
    Option       "ButtonMapping" "1 2 3 4 5 6 7 8 2 10 11 12"
EndSection
```

### Keychron K8 function keys mapping
[gist](https://gist.github.com/andrebrait/961cefe730f4a2c41f57911e6195e444)
#### Summary
Here's some [documentation](https://help.ubuntu.com/community/AppleKeyboard#Change_Function_Key_behavior) on it, but a quick summary can be found below:

> - 0 = `disabled`: Disable the 'fn' key. Pressing 'fn'+'F8' will behave like you only press 'F8'
> - 1 = `fkeyslast`: Function keys are used as last key. Pressing 'F8' key will act as a special key. Pressing 'fn'+'F8' will behave like a F8.
> - -> 2 = `fkeysfirst`: Function keys are used as first key. Pressing 'F8' key will behave like a F8. Pressing 'fn'+'F8' will act as special key (play/pause).

```bash
# replace <value> below with the one that worked for you in the previous step (0, 1 or 2)
# example: echo "options hid_apple fnmode=2 | sudo tee /etc/modprobe.d/hid_apple.conf"
# this will erase any pre-existing contents from /etc/modprobe.d/hid_apple.conf
echo "options hid_apple fnmode=<value>" | sudo tee /etc/modprobe.d/hid_apple.conf
# the "-k all" part is not always needed, but it's better to do that for all kernels anyway
sudo update-initramfs -u -k all
sudo systemctl reboot

```
