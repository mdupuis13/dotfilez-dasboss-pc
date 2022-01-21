# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# include sbin in PATH
if [ -d "/sbin" ] ; then
    PATH="/sbin:$PATH"
fi
if [ -d "/usr/sbin" ] ; then
    PATH="/usr/sbin:$PATH"
fi
# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes snaps bin if it exists
if [ -d "/snap/bin" ] ; then
    PATH="$PATH:/snap/bin"
fi

export GOPATH=$HOME/gopath
export PATH=$GOPATH:$GOPATH/bin:$PATH


## ZSH Unplugged section
# https://github.com/mattmc3/zsh_unplugged

# clone a plugin, identify its init file, source it (with zsh-defer if available)
# and add it to your fpath
function plugin-load() {
  local repo plugin_name plugin_dir initfile initfiles

  ZPLUGINDIR=${ZPLUGINDIR:-${ZDOTDIR:-$HOME/.config/zsh}/plugins}
  
  for repo in $@; do
    plugin_name=${repo:t}
    plugin_dir=$ZPLUGINDIR/$plugin_name
    initfile=$plugin_dir/$plugin_name.plugin.zsh

    if [[ ! -d $plugin_dir ]]; then
      echo "Cloning $repo"
      
      git clone -q --depth 1 --recursive --shallow-submodules https://github.com/$repo $plugin_dir
    fi
    
    if [[ ! -e $initfile ]]; then
      initfiles=($plugin_dir/*.plugin.{z,}sh(N) $plugin_dir/*.{z,}sh{-theme,}(N))
    
      [[ ${#initfiles[@]} -gt 0 ]] || { echo >&2 "Plugin has no init file '$repo'." && continue }
      
      ln -s "${initfiles[1]}" "$initfile"
    fi
    
    fpath+=$plugin_dir
    
    (( $+functions[zsh-defer] )) && zsh-defer . $initfile || . $initfile
  
  done
}

# Define some usefull env variables
ZPLUGINDIR=${ZPLUGINDIR:-${ZDOTDIR:-$HOME/.config/zsh}/plugins}

# make list of the Zsh plugins you use
plugins=(
  # plugins like prompts and defer that need loaded first
  romkatv/powerlevel10k  # is a power prompt https://github.com/romkatv/powerlevel10k

  # all plugins loaded afeter this line are defered
  romkatv/zsh-defer

  # all other plugins
  zsh-users/zsh-autosuggestions
  zsh-users/zsh-history-substring-search
  # ...

  # plugins like syntax highlighting that need loaded last
  zdharma-continuum/fast-syntax-highlighting
)

# now load your plugins
plugin-load $plugins



# make zsh/terminfo work for terms with application and cursor modes
#case "$TERM" in
#    vte*|xterm*)
#        zle-line-init() { zle -N zle-keymap-select; echoti smkx }
#        zle-line-finish() { echoti rmkx }
#        zle -N zle-line-init
#        zle -N zle-line-finish
#        ;;
#esac

# User configuration

# Vi mode
bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward


# By default, there is a 0.4 second delay after you hit the <ESC> key and when the mode change is registered.
# This results in a very jarring and frustrating transition between modes.
# Let's reduce this delay to 0.1 seconds.
export KEYTIMEOUT=1


# backspace fix for certain terminals (Kitty is among them)
if [[ -n $terminfo[kbs] ]]; then
    bindkey          "$terminfo[kbs]"   backward-delete-char
    bindkey -M vicmd "$terminfo[kbs]"   backward-char
fi

if [ -e /home/mdupuis/.nix-profile/etc/profile.d/nix.sh ]; then . /home/mdupuis/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

# Load bash files to zsh 
# test -f $HOME/.bashrc && source $HOME/.bashrc 
test -f $HOME/.bash_aliases && source $HOME/.bash_aliases

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
