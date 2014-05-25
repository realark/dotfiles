# COMPLETION SETTINGS
# add custom completion scripts
fpath=(~/.zsh/completion $fpath) 

# compsys initialization
autoload -U compinit
compinit

# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2

#Colors
eval `dircolors -b`
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


#Some shell variables
cdpath=(~ /media/Atlas /media)

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
export EDITOR="vim"
export BROWSER="links"
export PAGER="less"

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
alias ls='ls --color=auto -F'
alias top='htop'
alias minicom='sudo minicom'
alias open='xdg-open'

alias histgrep="grep '$1' /home/ark/.zsh_history"

#alias engage="play -n -c1 synth whitenoise band -n 100 20 band -n 50 20 gain +25 fade h 1 864000 1"
alias engage="mplayer -loop 0 ~/.sounds/enterprise_engine_sound.mp3"
alias use_the_force="telnet towel.blinkenlights.nl"
alias rsync="rsync -rh --partial --progress"
alias se="sync && exit"
alias minecraft="cd ~/minecraft && ./play_minecraft &"
alias k="konqueror . >/dev/null 2>&1 & disown"
alias wake_gc="ssh emergence './wake_gc'"
alias kill9="kill -9"
alias apt-get="sudo apt-get"
alias pacman="sudo powerpill"
alias ll="ls -l"
alias fswebcam="fswebcam -r 960x720"
alias blank="xset dpms force off"
alias tvtimesound="padsp sox -r 49000 -t ossdsp /dev/dsp3 -t ossdsp /dev/dsp" #Do something with arecord | aplay
alias ubuntup="apt-get update && apt-get dist-upgrade && apt-get upgrade"
alias pm-suspend="export DISPLAY=:0 ; qdbus org.freedesktop.PowerManagement /org/freedesktop/PowerManagement Suspend"
alias crontab="crontab -i"
alias duh="du -h --max-depth=0"
alias sqlplus="launch_sqlplus"
alias sbcl="rlwrap -c -H ~/.sbcl_history sbcl --noinform"
alias sudo='sudo ';
alias mtial='multitail'
alias cliemacs='emacs -nw'
alias idemacs='emacs >/dev/null 2>&1 &'
alias vimacs='emacs --no-desktop'
alias less='less -R'
alias yaourt='sudo yaourt --noconfirm'
alias telnet='rlwrap nc'
alias diff='colordiff'

PATH=~/scripts:$PATH

#Git Prompt
source ~/.zsh/git-prompt/zshrc.sh
username_prompt='%n'
if [ `whoami` = "root" ]; then
        username_prompt='$fg[red]'$username_prompt'$reset_color'
fi
PROMPT=$username_prompt'@%B%m%~%b$(git_super_status) %# '

##############Custom Functions

function listMavenCompletions { reply=(cli:execute cli:execute-phase archetype:generate compile clean install test test-compile deploy package cobertura:cobertura jetty:run -Dmaven.test.skip=true -DarchetypeCatalog=http://tapestry.formos.com/maven-snapshot-repository -Dtest= `if [ -d ./src ] ; then find ./src -type f | grep -v svn | sed 's?.*/\([^/]*\)\..*?-Dtest=\1?' ; fi`); }
compctl -K listMavenCompletions mvn

#cd to a file's directory (works on symlinks)
function follow() {
    file=$(readlink $1 || echo $1)
    #inode=$(ls -lai $file | grep -E "\s$file/?$" | awk '{print $1}')
    #find . -inum $inode
    cd $(dirname $file)
}

# Launch sqlplus
function launch_sqlplus() {
    prev_dir=`pwd`
    cd $ORACLE_HOME/sqlplus/admin/
    rlwrap -c -H ~/.sqlplus_history -pgreen sqlplus $@
    cd $prev_dir
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

#Use google to say things!
function gsay() {
	if [[ "${1}" =~ -[a-z]{2} ]]; then
		local lang=${1#-}
		local text="${*#$1}"
		else local lang=${LANG%_*}
		local text="$*";
	fi
	mplayer "http://translate.google.com/translate_tts?ie=UTF-8&tl=${lang}&q=${text}" &> /dev/null
}

. ~/.custom
