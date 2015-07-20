# .zshrc

# History
export HISTSIZE=5000000
export SAVEHIST=$HISTSIZE
export HISTFILE=~/.zsh_history

unsetopt bang_hist
setopt   hist_ignore_space
setopt   extended_history
setopt   hist_find_no_dups
setopt   inc_append_history
setopt   share_history

# completion
setopt   complete_in_word
setopt   list_packed
unsetopt list_types
setopt   mark_dirs
source ~/.zsh/git-flow-completion.zsh

# globbing
unsetopt auto_menu
unsetopt auto_remove_slash
setopt   nomatch
setopt   numeric_glob_sort
setopt   extended_glob

# job processing
unsetopt check_jobs
unsetopt hup

# miscellaneous
setopt   auto_cd
unsetopt clobber
setopt   dvorak
unsetopt flow_control
setopt   interactive_comments

autoload -U compinit
compinit -u

# C-w will stop at '/':
autoload -U select-word-style
select-word-style bash

# Completion control
zstyle ':completion:*' use-compctl false

# don't include current dir in completions involving `..'
zstyle ':completion:*' ignore-parents parent pwd ..

# show file completions with colors
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# case insensetive file name completion
zstyle ':completion:*' matcher-list '' \
    'm:{a-zåäöA-ZÅÄÖ}={A-ZÅÄÖa-zåäö}' \
    'r:|[._-]=** r:|=**'

# allow completion for `..'
zstyle ':completion:*' special-dirs ..

# from: ~/usr/tmp/zsh-completion/zsh-completion-intro.html
# (an introduction to zsh completion from www.linux-mag.com)
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

PROMPT="%S%n@%m:%2~%s"                         # left prompt: user@host:path
RPROMPT='%S%(?..?%?)%s'                        # right prompt: last exit code
WPROMPT='%~'                                   # window prompt: last path dir
if [[ $HOST != molly(.*|) ]]; then             # if not on molly [2003-10-11]
    WPROMPT="%m:$WPROMPT"                      #   host name in window prompt
fi                                             #
if [[ $USERNAME != luna ]]; then               # if not luna     [2003-10-11]
    WPROMPT="%n@$WPROMPT"                      #   user name in window prompt
fi                                             #
if [[ $UID -eq 0 ]]; then                      # if root
    PROMPT=$'%{\e[31m%}'$PROMPT$'%{\e[0m%}'    #   use red prompts
    RPROMPT=$'%{\e[31m%}'$RPROMPT$'%{\e[0m%}'  #
#elif [[ $TTY == /dev/pts/* ]]; then            # on remote terminal
#    PROMPT=$'%{\e[34;47m%}'$PROMPT$'%{\e[0m%}' #   use blue/white prompt
#    RPROMPT=$'%{\e[34;47m%}'$RPROMPT$'%{\e[0m%}'
elif [[ $HOST == deadpool(.*|) ]]; then        # if on deadpool
    PROMPT=$'%{\e[33m%}'$PROMPT$'%{\e[0m%}'    #   use yellow prompt
    RPROMPT=$'%{\e[33m%}'$RPROMPT$'%{\e[0m%}'  #
fi                                             #
if [[ $TERM == (xterm|ansi) ]]; then           # in xterm/MacSSH   [2003-10-08]
    PROMPT="%B${PROMPT}%b"                     #   boldify prompt text
    RPROMPT="%B$RPROMPT%b"                     #
fi                                             #
export PROMPT="$PROMPT "

if [[ $TERM = (xterm|rxvt(|-*)|screen(|.*)) ]];
then # graphic mode terminal
    precmd() {
        local T
        [[ -n $STY ]] && T="screen $STY" || T=${0#-}
        echo -En $'\e]0;'"$T [${(%)WPROMPT}]"$'\a'
    }
    preexec() {
        echo -En $'\e]0;'"$1 [${(%)WPROMPT}]"$'\a'
    }
fi

# for laziness
alias     al='alias'                # alias-alias                  [2003-12-11]
alias   cvsq='cvs -q -f -n update'
alias  cvsqd='cvs -q -f -n update -d'
alias   cvsd='cvs diff -Dnow'
alias       d='date -I'
alias       e='emacs'
#alias    grp='grep $* **/*rl **/*yaws' # HOW?
alias   mkae='make'                 #                              [2013-06-27]
alias   maek='make'                 #                              [2013-06-27]
alias     nd='mkdir `date -I` && cd `date -I`'
alias     ng='noglob '              # shorter noglob command       [2001­08­16]
alias   open='xdg-open'             # remember the command name    [2012-08-09]
alias     po='popd'
alias    psu='ps -u$USER'           # all my processes             [2003-12-16]
alias     pu='pushd'
alias  rwget='noglob wget -nc -p --no-parent -r -l0'
alias      t='date +"%H.%M"'        # time in HH.MM format         [2001­07­05]
alias  unzip='noglob unzip'         # don't use globs with unzip   [2001­10­16]
alias    utf='file apps/*/src/*erl apps/*/include/*hrl Makefile | grep "UTF"'
alias   word='sed `perl -e "print int rand(99999)"`"q;d" /usr/share/dict/words'

alias   aoeu='xmodmap ~/.Xmodmap.se'
alias   asdf='xmodmap ~/.Xmodmap'

alias     pu='pushd'
alias     po='popd'

if [[ $(uname) == Darwin ]]; then
    alias emacs=Emacs
fi

# ls
if [[ $(uname) == SunOS ]]; then
    alias ls='ls -F'
elif whence -p gls >& /dev/null; then
    alias ls='gls --color=auto'
elif ls --version |& grep -q 'Free Software Foundation'; then
    alias ls='ls --color=auto'
elif [[ $(uname) == FreeBSD ]]; then
    alias ls='ls -G'
else
    alias ls='ls -F'
fi

# alias expand after these commands
alias      noglob='noglob '         #                              [2003­06­14]
alias        sudo='sudo '           #                              [2003-04-23]
alias       watch='watch '          #                              [2003-06-14]

# for safety (make backups if destination already exists)
if [[ ! $(uname) == Darwin ]]; then
    alias -g    cp='cp -b'          # back up                      [2002­01­16]
    alias -g    ln='ln -b'          # back up                      [2001­01­16]
    alias       mv='mv -b'          # back up                      [2002­01­16]
fi

typeset -A account                             # "account" associative array
account=(
    amanda        dalu7049@amanda.it.uu.se
    cling         cl9dluna@starship.cling.gu.se
    fan           dalu7049@fan.it.uu.se
    grrowl        d98luna@grrowl.dtek.chalmers.se
    harpo         dalu7049@harpo.it.uu.se
    licia         d98luna@licia.dtek.chalmers.se
    mdstud        d98luna@lab.mdstud.chalmers.se
    pson          luna@pson.dyndns.org
    puck          luna@calliope.dyndns.org
    rackarberget  dalu7049@rackarberget.it.uu.se
    rama          dalu7049@rama.it.uu.se
    spikklubban   dalu7049@spikklubban.it.uu.se
    sshcd         luna@boris.cd.chalmers.se
    tempo         luna@tempo.update.uu.se
    psilo         luna@psilo.update.uu.se
    lal           luna@lal.dyndns.org
)

# create ssh aliases
for k (${(k)account}) {                         # for each key in account
    alias $k="ssh $account[$k]"                 #   create an ssh alias
    alias ${k}xterm="$k -f 'xterm -T $k -n $k'" #   and an xterm alias
}; unset k                                      #

# TAPS:

# common pipe­ending commands (taps)
alias -g  A='|head'           # head (also A<n> were <n> is 1-30)  [2001­10­20]
alias -g  B='|grep -v "^[       ]*$"' # kill blank rows            [2002­05­21]
alias -g  C='|sort -f|uniq -c' # unique w/ counter (C0=no pre­sort)[2002­08­15]
alias -g C0='|uniq -c'        #                                    [2002­05­21]
# D
alias -g  E='|perl -ne'       # as F, w/o implied print (E0 slurps)[2002­08­16]
alias -g E0='|perl -0777ne'   #                                    [2002­08­16]
alias -g  F='|perl -pe'       # filter (perl, F0 slurps)           [2002­01­10]
alias -g F0='|perl -0777pe'   #                                    [2002­08­16]
alias -g  G='|egrep'          # (e)grep (G0 searches stderr too)   [2002­08­15]
alias -g G0='|&egrep'         #                                    [2002­09­11]
# H
alias -g  I='|column'         # columnify (think: `I' is a column) [2002­05­21]
# J
alias -g  K='**/*rl(.)'       # note: not a tap                    [2010-06-16]
alias -g  L='|wc -l'          # line count                         [2007­03­27]
alias -g  M='|&less'          # more (M0 ingnores STDERR)          [2003­02­03]
alias -g M0='|less'           #                                    [2001­10­20]
# N  O  P  Q
alias -g  P='**/*py(.)'       # note: not a tap                    [2010-10-01]
alias -g  R='|unhtml'         # remove HTML (R0 removes man codes) [2002­08­17]
alias -g R0='|perl -pe "s/.[\b]//g"' # remove man page ^H codes    [2002­08­16]
alias -g RL='apps/**/*rl(.)'  # note: not a tap                    [2007-09-07]
alias -g  S='|sort -f'        # alphabetic sort (S0 for numeric)   [2002­01­10]
alias -g S0='|sort -n'        #                                    [2002­05­21]
alias -g  T='|iconv -flatin1 -tutf-8' # latin1 to utf-8            [2008-07-28]
alias -g TT='|iconv -futf-8 -tlatin1' # utf-8 to latin1            [2008-07-28]
alias -g  U='|sort |uniq'     # unique (U0=no pre­sort)            [2002­08­15]
alias -g U0='|uniq'           #                                    [2002­01­10]
# V
alias -g  W='apps/{**/*.html,*/*/*/src/**/*.js}(.)' # note: not a tap [2013-02-08]
alias -g  X='|tr -s " " "\t" |cut -f' # cut on tabs and spaces     [2002­04­27]
#alias -g  Y='|tee /tmp/tee_output.txt'
alias -g  Y='&>/dev/null &; disown' # fork process (`Y' is a fork) [2002­08­27]
alias -g  Z='|tail'           # tail (also Z<n> were <n> is 1-30)  [2001­10­20]


# aliases A<n> and Z<n> (where <n> is 1-30)
for ((i=1; i<=30; i++)); do     #
    alias -g A$i="|head -n$i"   # head (1-30 rows)                 [2002­05­20]
    alias -g Z$i="|tail -n$i"   # tail (1-30 rows)                 [2002­05­20]
done                            #
unset i                         #

alias -g HT='apps/**/*.html'
