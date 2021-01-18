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

autoload -U compinit compinit
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
export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode

##################################################################
# Tab completions

autoload -Uz compinit
compinit


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
alias crontab="fcrontab"
alias duh="du -h -d 0"
alias sqlplus="launch_sqlplus"
alias mtial='multitail'
alias emacs='emacsclient -create-frame --alternate-editor=""'
alias less='less -R'
alias yaourt='yaourt --noconfirm'
alias telnet='rlwrap nc'
alias diff='colordiff'
alias journalctl='journalctl --pager-end --since "1 day ago"'
alias myipaddress='curl ifconfig.me'
alias myinternetspeed='speedtest-cli'
alias docker-stop-all='docker stop $(docker ps -aq)'
alias docker-rm-all='docker rm $(docker ps -aq)'

# I don't want to set LC_ALL but perl and locale complain if I don't
export LC_ALL="$LANG"

export PATH=~/scripts:$PATH:/snap/bin:~/bin:~/.roswell/bin

#zsh-syntax-highlighting
#source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

#Git Prompt
#source ~/.zsh/git-prompt/zshrc.sh
#username_prompt='%n'
#if [ `whoami` = "root" ]; then
#        username_prompt='$fg[red]'$username_prompt'$reset_color'
#fi
#PROMPT=$username_prompt'@%B%m%~%b$(git_super_status) %# '

#Smartcd
#source $HOME/.smartcd/lib/core/smartcd
#source $HOME/.smartcd_config

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

source_if_exists ~/.admin/sec.sh
source_if_exists ~/.admin/$(hostname)_custom.sh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
if [ -d "/Users/akent/.sdkman" ]; then
  export SDKMAN_DIR="/Users/akent/.sdkman"
  [[ -s "/Users/akent/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/akent/.sdkman/bin/sdkman-init.sh"
  # to use sdkman
  #     source "/Users/akent/.sdkman/bin/sdkman-init.sh"
  #     sdk list java
  #     sdk install java <version>


  PATH="/Users/akent/perl5/bin${PATH:+:${PATH}}"; export PATH;
  PERL5LIB="/Users/akent/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
  PERL_LOCAL_LIB_ROOT="/Users/akent/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
  PERL_MB_OPT="--install_base \"/Users/akent/perl5\""; export PERL_MB_OPT;
  PERL_MM_OPT="INSTALL_BASE=/Users/akent/perl5"; export PERL_MM_OPT;
fi
