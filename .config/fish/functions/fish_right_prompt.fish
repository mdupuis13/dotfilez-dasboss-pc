# "Icons" 
# taking some hints from stefanmaric/bigfish
# additional spaces to work around not-really-monospace glyphs
set -g __dmorrell_git_icon_new     "⚹ "
set -g __dmorrell_git_icon_changed "↺ "
set -g __dmorrell_git_icon_removed "🗴"
set -g __dmorrell_git_icon_stashed "≡ "

set -g __dmorrell_git_icon_branch   '🜉'
set -g __dmorrell_git_icon_tag      '⌂'
set -g __dmorrell_git_icon_commit   '⌀'

set -g __dmorrell_git_icon_diverged '⭿'
set -g __dmorrell_git_icon_ahead    '⭱'
set -g __dmorrell_git_icon_behind   '⭳'

# Colors
# cf. http://fishshell.com/docs/current/commands.html#set_color
set -g __dmorrell_git_color_normal  "555"
set -g __dmorrell_git_color_new     "0a0"
set -g __dmorrell_git_color_changed "ddc203"
set -g __dmorrell_git_color_removed "a00"
set -g __dmorrell_git_color_stashed "fff"

set -g __dmorrell_git_color_rev_bg      "555"
set -g __dmorrell_git_color_rev_fg      "fff"
set -g __dmorrell_git_color_rev_fg_warn "brred"


function __dmorrell_print_git_branch_state
    set upstream_state (command git rev-list --count --left-right "@{upstream}...HEAD" ^ /dev/null)
    if [ $status = 0 ]
      echo "$upstream_state" | command awk "
          /0\t0/          { exit 0 }
          /[0-9]+\t0/     { print \"$__dmorrell_git_icon_behind \";   exit 0 }
          /0\t[0-9]+/     { print \"$__dmorrell_git_icon_ahead \";    exit 0 }
          //              { print \"$__dmorrell_git_icon_diverged \"; exit 0 }
      "
    else
      # no upstream branch configured
      echo "$__dmorrell_git_icon_new "
    end
end


function __dmorrell_print_git_revlabel
  # Is it a branch?
  set rev (command git symbolic-ref --short HEAD 2> /dev/null)
  if [ $status = 0 ]
    set state (__dmorrell_print_git_branch_state)
    echo " $__dmorrell_git_icon_branch $rev $state"
    return 0
  end
  # No!

  # Is it a tag?
  set rev (command git describe --tags --exact-match ^/dev/null)
  if [ $status = 0 ]
    echo " $__dmorrell_git_icon_tag $rev "
    return 0
  end
  # No!

  # Is it just a generic commit?
  set rev (command git rev-parse --short HEAD 2> /dev/null)
  if [ $status = 0 ]
    echo " $__dmorrell_git_icon_commit $rev "
    return 0
  end
  # No!

  # What is it? ... Not Git.
  return 1
end


function __dmorrell_print_git_status -d "Gets the current git status"
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
    set_color $__dmorrell_git_color_normal
    if [ "$new" != "0" ]
      set_color $__dmorrell_git_color_new
    end
    echo "$new$__dmorrell_git_icon_new "

    # Show the number of modified files
    set_color $__dmorrell_git_color_normal
    if [ "$mod" != "0" ]
      set_color $__dmorrell_git_color_changed
    end
    echo "$mod$__dmorrell_git_icon_changed "

    # Show the number of removed files
    set_color $__dmorrell_git_color_normal
    if [ "$del" != "0" ]
      set_color $__dmorrell_git_color_removed
    end
    echo "$del$__dmorrell_git_icon_removed "

    # Show the number of stashes
    set_color $__dmorrell_git_color_normal
    if [ "$stashed" != "0" ]
      set_color $__dmorrell_git_color_stashed
    end
    echo "$stashed$__dmorrell_git_icon_stashed "

    # Show the current revision, with bells and whistles
    set_color -b $__dmorrell_git_color_rev_bg
    if begin     test -f (git rev-parse --git-dir)/MERGE_HEAD;
             or  test -d (git rev-parse --git-path rebase-merge);
             or  test -d (git rev-parse --git-path rebase-apply)
             # cf. https://stackoverflow.com/a/3921928/539599
       end
       # Indicate that "something" is afoot
       set_color $__dmorrell_git_color_rev_fg_warn
    else
      set_color $__dmorrell_git_color_rev_fg
    end
    __dmorrell_print_git_revlabel
    set_color normal
   end
end


function fish_right_prompt -d "Prints right prompt"
  __dmorrell_print_git_status
end