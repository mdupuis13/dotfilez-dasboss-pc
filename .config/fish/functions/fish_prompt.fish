# this theme inspired from dmorrell
# "Icons"
set -g __md_prompt_icon_fail    "⌁"
set -g __md_prompt_icon_success "—"
set -g __md_prompt_icon_ssh     " SSH: "
set -g __md_prompt_icon_prompt  "\$"
set -g __md_prompt_icon_root    "!"

# Colors
set -g __md_prompt_color_fail    "bf616a"
set -g __md_prompt_color_success "a3be8c"

set -g __md_prompt_color_ssh_bg  "blue"
set -g __md_prompt_color_ssh_fg  "d8dee9"

set -g __md_prompt_color_user "88c0d0"
set -g __md_prompt_color_host "81a1c1"
set -g __md_prompt_color_path "81a1c1"

set -g __md_prompt_color_prompt  "5e81ac"

## Function to show a segment
function __md_prompt_segment -d "Function to show a segment"
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
function __md_show_status -d "Function to show the current status"
  if [ $RETVAL -ne 0 ]
    __md_prompt_segment \
      normal \
      $__md_prompt_color_fail \
      $__md_prompt_icon_fail
  else
    __md_prompt_segment \
      normal \
      $__md_prompt_color_success \
      $__md_prompt_icon_success
  end
  if [ -n "$SSH_CLIENT" ]
      __md_prompt_segment \
        $__md_prompt_color_ssh_bg \
        $__md_prompt_color_ssh_fg \
        $__md_prompt_icon_ssh
    end
end

function __md_show_virtualenv -d "Show active python virtual environments"
  if set -q VIRTUAL_ENV
    set -l venvname (basename "$VIRTUAL_ENV")
    __md_prompt_segment normal white " ($venvname) "
  end
end

## Show user if not in default users
function __md_show_user -d "Show user"
  if not contains $USER $default_user; or test -n "$SSH_CLIENT"
    set -l host (hostname -s)
    set -l who (whoami)
   __md_prompt_segment normal $__md_prompt_color_user " $who"

    # Skip @ bit if hostname == username
    if [ "$USER" != "$HOST" ]
      __md_prompt_segment normal white "@"
      __md_prompt_segment normal $__md_prompt_color_host "$host "
    end
  else
    __md_prompt_segment normal normal " "
  end
end

function __md_set_venv_project --on-variable VIRTUAL_ENV
    if test -e $VIRTUAL_ENV/.project
        set -g VIRTUAL_ENV_PROJECT (cat $VIRTUAL_ENV/.project)
    end
end

# Show directory
function __md_show_pwd -d "Show the current directory"
  set -l pwd
  if [ (string match -r '^'"$VIRTUAL_ENV_PROJECT" $PWD) ]
    set pwd (string replace -r '^'"$VIRTUAL_ENV_PROJECT"'($|/)' '≫ $1' $PWD)
  else
    set pwd (prompt_pwd)
  end
  __md_prompt_segment normal $__md_prompt_color_path "$pwd"
end

# Show prompt w/ privilege cue
function __md_show_prompt -d "Shows prompt with cue for current priv"
  set -l uid (id -u $USER)
  
  if [ $uid -eq 0 ] # This is root!
    __md_prompt_segment \
      red \
      $__md_prompt_color_prompt \
      " $__md_prompt_icon_root "
    set_color normal
    echo -n -s " "
  else
    __md_prompt_segment \
      normal \
      $__md_prompt_color_prompt \
      " $__md_prompt_icon_prompt "
  end

  set_color normal
end

### BEGIN git prompt
# "Icons" 
# taking some hints from stefanmaric/bigfish
# additional spaces to work around not-really-monospace glyphs
set -g __md_git_icon_new     "⚹ "
set -g __md_git_icon_changed "↺ "
set -g __md_git_icon_removed "🗴"
set -g __md_git_icon_stashed "≡ "

set -g __md_git_icon_branch   '🜉'
set -g __md_git_icon_tag      '⌂'
set -g __md_git_icon_commit   '⌀'

set -g __md_git_icon_diverged '⭿'
set -g __md_git_icon_ahead    '⭱'
set -g __md_git_icon_behind   '⭳'

# Colors
# cf. http://fishshell.com/docs/current/commands.html#set_color
set -g __md_git_color_normal  "8fbcbb"
set -g __md_git_color_new     "a3be8c"
set -g __md_git_color_changed "d08770"
set -g __md_git_color_removed "bf616a"
set -g __md_git_color_stashed "8fbcbb"

set -g __md_git_color_rev_bg      "2e3440"
set -g __md_git_color_rev_fg      "d8dee9"
set -g __md_git_color_rev_fg_warn "bf616a"


function __md_print_git_branch_state
    set upstream_state (command git rev-list --count --left-right "@{upstream}...HEAD" ^ /dev/null)
    if [ $status = 0 ]
      echo -n "$upstream_state" | command awk "
          /0\t0/          { exit 0 }
          /[0-9]+\t0/     { print \"$__md_git_icon_behind \";   exit 0 }
          /0\t[0-9]+/     { print \"$__md_git_icon_ahead \";    exit 0 }
          //              { print \"$__md_git_icon_diverged \"; exit 0 }
      "
    else
      # no upstream branch configured
      echo -n "$__md_git_icon_new "
    end
end


function __md_print_git_revlabel
  # Is it a branch?
  set rev (command git symbolic-ref --short HEAD 2> /dev/null)
  if [ $status = 0 ]
    set state (__md_print_git_branch_state)
    echo -n " $__md_git_icon_branch $rev $state"
    return 0
  end
  # No!

  # Is it a tag?
  set rev (command git describe --tags --exact-match ^/dev/null)
  if [ $status = 0 ]
    echo -n " $__md_git_icon_tag $rev "
    return 0
  end
  # No!

  # Is it just a generic commit?
  set rev (command git rev-parse --short HEAD 2> /dev/null)
  if [ $status = 0 ]
    echo -n " $__md_git_icon_commit $rev "
    return 0
  end
  # No!

  # What is it? ... Not Git.
  return 1
end


function __md_print_git_status -d "Gets the current git status"
  if command git rev-parse --is-inside-work-tree >/dev/null 2>&1
    set -l new (command git status --porcelain=v1 --ignore-submodules=dirty \
                | grep -e '^ \?A' | wc -l)
    set -l mod (command git status --porcelain=v1 --ignore-submodules=dirty \
                | grep -e '^ \?\(M\|R\)' | wc -l)
    set -l del (command git status --porcelain=v1 --ignore-submodules=dirty \
                | grep -e '^ \?D' | wc -l)
    set -l stashed (command git stash list --no-decorate \
                | wc -l)

    # set -g fish_emoji_width 1

    set_color -b normal

    # Show the number of new files
    set_color $__md_git_color_normal
    if [ "$new" != "0" ]
      set_color $__md_git_color_new
    end
    echo -n "$new$__md_git_icon_new "

    # Show the number of modified files
    set_color $__md_git_color_normal
    if [ "$mod" != "0" ]
      set_color $__md_git_color_changed
    end
    echo -n "$mod$__md_git_icon_changed "

    # Show the number of removed files
    set_color $__md_git_color_normal
    if [ "$del" != "0" ]
      set_color $__md_git_color_removed
    end
    echo -n "$del$__md_git_icon_removed "

    # Show the number of stashes
    set_color $__md_git_color_normal
    if [ "$stashed" != "0" ]
      set_color $__md_git_color_stashed
    end
    echo -n "$stashed$__md_git_icon_stashed "

    # Show the current revision, with bells and whistles
    set_color -b $__md_git_color_rev_bg
    if begin     test -f (git rev-parse --git-dir)/MERGE_HEAD;
             or  test -d (git rev-parse --git-path rebase-merge);
             or  test -d (git rev-parse --git-path rebase-apply)
             # cf. https://stackoverflow.com/a/3921928/539599
       end
       # Indicate that "something" is afoot
       set_color $__md_git_color_rev_fg_warn
    else
      set_color $__md_git_color_rev_fg
    end
    __md_print_git_revlabel
    set_color normal
   end
end
### END git prompt

## SHOW PROMPT
function fish_prompt
  set -g RETVAL $status
  __md_show_status
  __md_show_virtualenv
  __md_show_user
  __md_show_pwd 
  echo -n -e "\t"
  __md_print_git_status
  echo
  __md_show_prompt
end
