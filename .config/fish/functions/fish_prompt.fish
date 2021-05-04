# "Icons"
set -g __dmorrell_prompt_icon_fail    "⌁"
set -g __dmorrell_prompt_icon_success "—"
set -g __dmorrell_prompt_icon_ssh     " SSH: "
set -g __dmorrell_prompt_icon_prompt  "\$"
set -g __dmorrell_prompt_icon_root    "!"

# Colors
set -g __dmorrell_prompt_color_fail    "brred"
set -g __dmorrell_prompt_color_success "green"

set -g __dmorrell_prompt_color_ssh_bg  "blue"
set -g __dmorrell_prompt_color_ssh_fg  "white"

set -g __dmorrell_prompt_color_user "yellow"
set -g __dmorrell_prompt_color_host "green"
set -g __dmorrell_prompt_color_path "yellow"

set -g __dmorrell_prompt_color_prompt  "717f24"

## Function to show a segment
function __dmorrell_prompt_segment -d "Function to show a segment"
  # Get colors
  set -l bg $argv[1]
  set -l fg $argv[2]

  # Set 'em
  set_color -b $bg
  set_color $fg

  # Print text
  if [ -n "$argv[3]" ]
    echo -n -s $argv[3]
  end
end

## Function to show current status
function __dmorrell_show_status -d "Function to show the current status"
  if [ $RETVAL -ne 0 ]
    __dmorrell_prompt_segment \
      normal \
      $__dmorrell_prompt_color_fail \
      $__dmorrell_prompt_icon_fail
  else
    __dmorrell_prompt_segment \
      normal \
      $__dmorrell_prompt_color_success \
      $__dmorrell_prompt_icon_success
  end
  if [ -n "$SSH_CLIENT" ]
      __dmorrell_prompt_segment \
        $__dmorrell_prompt_color_ssh_bg \
        $__dmorrell_prompt_color_ssh_fg \
        $__dmorrell_prompt_icon_ssh
    end
end

function __dmorrell_show_virtualenv -d "Show active python virtual environments"
  if set -q VIRTUAL_ENV
    set -l venvname (basename "$VIRTUAL_ENV")
    __dmorrell_prompt_segment normal white " ($venvname) "
  end
end

## Show user if not in default users
function __dmorrell_show_user -d "Show user"
  if not contains $USER $default_user; or test -n "$SSH_CLIENT"
    set -l host (hostname -s)
    set -l who (whoami)
   __dmorrell_prompt_segment normal $__dmorrell_prompt_color_user " $who"

    # Skip @ bit if hostname == username
    if [ "$USER" != "$HOST" ]
      __dmorrell_prompt_segment normal white "@"
      __dmorrell_prompt_segment normal $__dmorrell_prompt_color_host "$host "
    end
  else
    __dmorrell_prompt_segment normal normal " "
  end
end

function __dmorrell_set_venv_project --on-variable VIRTUAL_ENV
    if test -e $VIRTUAL_ENV/.project
        set -g VIRTUAL_ENV_PROJECT (cat $VIRTUAL_ENV/.project)
    end
end

# Show directory
function __dmorrell_show_pwd -d "Show the current directory"
  set -l pwd
  if [ (string match -r '^'"$VIRTUAL_ENV_PROJECT" $PWD) ]
    set pwd (string replace -r '^'"$VIRTUAL_ENV_PROJECT"'($|/)' '≫ $1' $PWD)
  else
    set pwd (prompt_pwd)
  end
  __dmorrell_prompt_segment normal $__dmorrell_prompt_color_path "$pwd"
end

# Show prompt w/ privilege cue
function __dmorrell_show_prompt -d "Shows prompt with cue for current priv"
  set -l uid (id -u $USER)
  
  if [ $uid -eq 0 ] # This is root!
    __dmorrell_prompt_segment \
      red \
      $__dmorrell_prompt_color_prompt \
      " $__dmorrell_prompt_icon_root "
    set_color normal
    echo -n -s " "
  else
    __dmorrell_prompt_segment \
      normal \
      $__dmorrell_prompt_color_prompt \
      " $__dmorrell_prompt_icon_prompt "
  end

  set_color normal
end

## SHOW PROMPT
function fish_prompt
  set -g RETVAL $status
  __dmorrell_show_status
  __dmorrell_show_virtualenv
  __dmorrell_show_pwd
  __dmorrell_show_prompt
end
