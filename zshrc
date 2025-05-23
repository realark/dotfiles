# -*- mode: shell-script -*-

case "$(uname -a)" in
  Linux*)     machine=linux;;
  Darwin*)    machine=mac;;
  CYGWIN*)    machine=cygwin;;
  MINGW*)     machine=mingw;;
  *)          machine="unknown"
esac

# COMPLETION SETTINGS
# add custom completion scripts
fpath=(~/.zsh/completion $fpath)

# compsys initialization
autoload -U compinit
compinit

# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2

#Colors

if which dircolors >/dev/null 2>&1; then
  # no dircolors on osx
  eval `dircolors -b`
  if which gdircolors >/dev/null 2>&1; then
    alias dircolors=gdircolors
  fi
fi

#Colors for man pages
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_md=$(tput bold; tput setaf 6) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3; tput setab 4) # yellow on blue
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)

PROMPT='%(!.%F{red}.%F{cyan})%n%F{white}@%F{cyan}%m%F{white}::%F{magenta}%3C%F{green}%# '

#Some shell variables
cdpath=( ~ )

#history settings
export HISTFILE=~/.zsh_history
export HISTSIZE=50000
export SAVEHIST=50000
setopt append_history
setopt share_history
export HISTIGNORE="[ ]*"

setopt autopushd pushdminus pushdsilent pushdtohome
setopt autocd
setopt cdablevars
setopt interactivecomments
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt SH_WORD_SPLIT
setopt nohup
setopt print_exit_value

# Vars used later on by Zsh
export BROWSER="links"
export PAGER="less"
export ALTERNATE_EDITOR=""
export EDITOR="vim"                             # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode

##################################################################
# allow approximate
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# tab completion for PID :D
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# cd not select parent dir
zstyle ':completion:*:cd:*' ignore-parents parent pwd


##################################################################
# Key bindings
bindkey "e[1~" beginning-of-line
bindkey "^[OH" beginning-of-line
bindkey "^[[H" beginning-of-line
bindkey "e[4~" end-of-line
bindkey "^[OF" end-of-line
bindkey "^[[F" end-of-line
bindkey "e[5~" beginning-of-history
bindkey "e[6~" end-of-history
bindkey "e[3~" delete-char
bindkey "^[[3~" delete-char
bindkey "e[2~" quoted-insert
bindkey "e[5C" forward-word
bindkey "e[5D" backward-word
bindkey "ee[C" forward-word
bindkey "ee[D" backward-word
bindkey "^H" backward-delete-word
# for rxvt
bindkey "e[8~" end-of-line
bindkey "e[7~" beginning-of-line
# for non RH/Debian xterm, can't hurt for RH/DEbian xterm
bindkey "eOH" beginning-of-line
bindkey "eOF" end-of-line
# for freebsd console
bindkey "e[H" beginning-of-line
bindkey "e[F" end-of-line
# completion in the middle of a line
bindkey '^i' expand-or-complete-prefix
#Vim keybindings
bindkey -v

##################################################################
# Some aliases
if [ "$machine" = "linux" ]; then
  alias ls='ls --color=auto -F'
else
  if [ "$machine" = "mac" ]; then
    alias ls='ls -G -F'
  fi
fi
alias top='htop'
alias open='xdg-open'

alias histgrep="grep '$1' /home/ark/.zsh_history"
alias grep="grep --color=auto"

#alias engage="play -n -c1 synth whitenoise band -n 100 20 band -n 50 20 gain +25 fade h 1 864000 1"
alias rsync="rsync -p -rh --partial --progress"
alias se="sync && exit"
alias k="dolphin . >/dev/null 2>&1 & disown"
alias ll="ls -l"
alias blank="xset dpms force off"
alias duh="du -h -d 0"
alias sqlplus="launch_sqlplus"
alias mtial='multitail'
alias emacs='emacsclient -create-frame --alternate-editor=""'
alias less='less -R'
alias yaourt='yaourt --noconfirm'
alias telnet='rlwrap nc'
alias journalctl='journalctl --pager-end --since "1 day ago"'
alias docker-stop-all='docker stop $(docker ps -aq)'
alias docker-rm-all='docker rm $(docker ps -aq)'
alias fd='fdfind'
alias dolt-rpull='for dir in */; do if [ -d "${dir}/.dolt" ]; then (cd "$dir" && echo "Pulling $dir" && dolt pull); fi; done'
alias myhw='echo "HARDWARE FOR MACHINE \"$(hostname)\"" && \
echo "--- CPU ---" && lscpu | grep -E "Model name|CPU\(s\)|Thread|MHz" && \
echo -e "\n--- MEMORY ---" && free -h && \
echo -e "\n--- GPU ---" && lspci | grep -i vga && \
echo -e "\n--- STORAGE ---" && df -h /'
# Wrap ssh in an alias to change the terminal color scheme.
# Supported terminals:
# - konsole
function change_terminal_colors {
  NEW_COLORS="$!"
  if which konsoleprofile >/dev/null 2>&1; then
    konsoleprofile colors="$1"
  fi
}
function wrapped_ssh() {
  change_terminal_colors "Solarized"
  ssh "$@"
  change_terminal_colors "GreenOnBlack"
}
alias ssh='wrapped_ssh'
compdef wrapped_ssh=ssh
function diff_or_colordiff() {
  if command -v colordiff &> /dev/null; then
    colordiff "$@"
  else
    diff "$@"
  fi
}
alias diff='diff_or_colordiff'

# I don't want to set LC_ALL but perl and locale complain if I don't
export LC_ALL="$LANG"

export PATH=~/scripts:$PATH:/snap/bin:~/bin:~/.roswell/bin:~/.local/bin:~/.cargo/bin

#zsh-syntax-highlighting
#source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#Git Prompt
#source ~/.zsh/git-prompt/zshrc.sh
#username_prompt='%n'
#if [ `whoami` = "root" ]; then
#        username_prompt='$fg[red]'$username_prompt'$reset_color'
#fi
#PROMPT=$username_prompt'@%B%m%~%b$(git_super_status) %# '

##############Custom Functions

function lecho() {
  echo > $1 && less $1
}

function listMavenCompletions { reply=(cli:execute cli:execute-phase archetype:generate compile clean install test test-compile deploy package cobertura:cobertura jetty:run -Dmaven.test.skip=true -DarchetypeCatalog=http://tapestry.formos.com/maven-snapshot-repository -Dtest= `if [ -d ./src ] ; then find ./src -type f | grep -v svn | sed 's?.*/\([^/]*\)\..*?-Dtest=\1?' ; fi`); }
compctl -K listMavenCompletions mvn

function runGradle() {
  gradleTasks=""
  if [ -d "$1" -a $# -gt 1 ]; then
    #Passed a dir as arg1. We'll assume it's a gradle subproject
    gradleSubproject=$(echo $1 | sed -r 's/\//:/g' | sed -r 's/^:*(.*):*$/:\1:/g')
    for arg in "${@:2}"; do
      gradleTasks="$gradleTasks ${gradleSubproject}${arg}"
    done
  else
    gradleTasks=$@
  fi
  if [ -f ./gradlew ]; then
    echo "RUNNING: gradle $gradleTasks"
    ./gradlew $gradleTasks
  else
    echo "RUNNING: gradle $gradleTasks"
    $GRADLE_BIN $gradleTasks
  fi
}

# Show 'dots' if a tab completion stalls
function expand-or-complete-with-dots() {
  echo -n "\e[31m......\e[0m"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots
bindkey '^R' history-incremental-search-backward

#Set the window title for the terminal
#function precmd {
#  print -Pn "\e]0;%n@%M: %~\a"
#}

# Set the title of a Terminal window
function settitle() {
  if [ -n "$STY" ] ; then         # We are in a screen session
    echo "Setting screen titles to $@"
    printf "\033k%s\033\\" "$@"
    screen -X eval "at \\# title $@" "shelltitle $@"
  else
    printf "\033]0;%s\007" "$@"
  fi
}

function formatxml(){
  for file in $@; do
    tmpfile="/tmp/$file.tmp"
    xmllint --format $file > $tmpfile
    mv $tmpfile $file
  done
}

# In general, I'd like to only capture complicatated commands or commands which changed the state of the machine.
declare -a history_exclude_regexes=('^ls$' '^tree$' '^(h)?top$' '^pwd$' 'screen' '^whoami$' 'su -' '^\s*cd\s*$' '^idemacs$' '^git (status|log|ls|ll|show)$' '^(sudo )?poweroff$')

function zshaddhistory() {
  emulate -L zsh
  for regex in "${history_exclude_regexes[@]}"; do
    if echo $1 2>/dev/null | grep -E $regex >/dev/null 2>&1 ; then
      return 1
    fi
  done
}

function source_if_exists {
  filepath=$1
  if [ -f $filepath ]; then
    source $filepath
  fi
}

source_if_exists ~/.custom.sh

alias myipaddress='curl ifconfig.me'
alias myinternetspeed='speedtest-cli'

function hosts_on_network {
  if [ "$machine" != "linux" ]; then
    echo "Unsupported OS: $machine"
    return 1
  fi
  NETWORK="$1"
  if [ "$NETWORK" = "" ]; then
    NETWORK="192.168.1.0"
  fi
  echo "------ $NETWORK ------"
  nmap -sP "$NETWORK/24" | grep "Nmap scan report for " | sed -E "s/Nmap scan report for //g" | sed -E "s/(.*) \((.*)\)/\2 -- \1/g" | sed -E "s/(\.[0-9][0-9]) --/\1  --/g" | sed -E "s/(\.[0-9]) --/\1   --/g"
}

function docker-ls-tags {
  if [ $# -lt 1 ]
  then
    cat << HELP

docker-ls-tags  --  list all tags for a Docker image on a remote registry.

EXAMPLE:
    - list all tags for ubuntu:
       dockertags ubuntu

    - list all php tags containing apache:
       dockertags php apache

HELP
  fi

  image="$1"
  tags=`wget -q https://registry.hub.docker.com/v1/repositories/${image}/tags -O -  | sed -e 's/[][]//g' -e 's/"//g' -e 's/ //g' | tr '}' '\n'  | awk -F: '{print $3}'`

  if [ -n "$2" ]
  then
    tags=` echo "${tags}" | grep "$2" `
  fi

  echo "${tags}"
}

if [ -f "$HOME/.sdkman/bin/sdkman-init.sh" ]; then
  source "$HOME/.sdkman/bin/sdkman-init.sh"
fi

export GO111MODULE=on
export GOPATH="$HOME/go"
export GOBIN="$HOME/go/bin"
if [ -d $HOME/go/bin ]; then
  export "PATH=$PATH:$HOME/go/bin"
fi

if [ -d "$HOME/adb-fastboot/platform-tools" ] ; then
  export PATH="$HOME/adb-fastboot/platform-tools:$PATH"
fi

if [ -f /etc/motd ]; then
  cat /etc/motd
fi

export NVM_DIR="$HOME/.nvm"
function nvm {
  unset -f nvm
  unset -f npm
  # NVM is super slow to load and I usually don't need it
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
  nvm $@ # now call the real nvm
}
function npm {
  unset -f nvm
  unset -f npm
  # NVM is super slow to load and I usually don't need it
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
  npm $@ # now call the real npm
}

if [ -d "$HOME/.local/share/pnpm" ]; then
  # pnpm
  export PNPM_HOME="/home/ark/.local/share/pnpm"
  case ":$PATH:" in
    *":$PNPM_HOME:"*) ;;
    *) export PATH="$PNPM_HOME:$PATH" ;;
  esac
  # pnpm end
fi

# fnm
if [ -d "$HOME/.local/share/fnm" ]; then
  export PATH="$HOME/.local/share/fnm:$PATH"
  eval "`fnm env`"
fi

#Smartcd: https://github.com/cxreg/smartcd
if [ -f "$HOME/.smartcd/lib/core/smartcd" ]; then
  source $HOME/.smartcd/lib/core/smartcd
  if [ -f  -f "$HOME/.smartcd_config" ]; then
    source $HOME/.smartcd_config
  fi
fi

coinflip() {
  if (($RANDOM%2)); then
    echo "heads"
    return 0;
  else
    echo "tails"
    return 1;
  fi
}

if [ -f "$HOME/.lmstudio/bin" ]; then
  export PATH="$PATH:$HOME/.lmstudio/bin"
fi
