typeset -U path fpath manpath
path=(~/devel/otp/bin /cygdrive/c/Program\ Files/erl7.0/bin /cygdrive/c/Users/luna/Dropbox/devel/rebar3 ~/usr/bin $path)

fpath=(~/.zsh $fpath)
manpath=(~/usr/man $manpath)

#export LANG=C
#export LC_ALL=C
export MANWIDTH=80
export VISUAL='~/bin/emacs-nw'
export EDITOR='~/bin/emacs-nw'
export PAGER=less
export GREP_COLOR='41'

# export ERL_COMPILER_OPTIONS='[{warn_format,2},compressed,debug_info]'
# export ERL_COMPILER_OPTIONS="[warn_unused_vars,nowarn_shadow_vars,debug_info]"

export BIBINPUTS=".:$HOME/doc/tex:$HOME/luna/HiPE/papers/bibtex"
export TEXINPUTS="$HOME/doc/tex:"

export GZIP='-9'
export ZIPOPT='-9'
export USER=$USERNAME                          # strange zsh      [2003-12-19]

export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.arj=01;31:*.bz2=01;31:*.gz=01;31:*.jar=01;31:*.lzh=01;31:*.rar=01;31:*.tar=01;31:*.taz=01;31:*.tbz2=01;31:*.tgz=01;31:*.Z=01;31:*.z=01;31:*.zip=01;31:*.bmp=01;35:*.gif=01;35:*.ico=01;35:*.jpeg=01;35:*.jpg=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.tga=01;35:*.tif=01;35:*.tiff=01;35:*.xbm=01;35:*.xcf=01;35:*.xpm=01;35:*.au=01;35:*.avi=01;35:*.mp3=01;35:*.mpeg=01;35:*.mpg=01;35:*.ogg=01;35:*.wav=01;35:*.xm=01;35'

export LESS='-RXQiz-2'
export LESSCHARDEF='8bcccbcc13bc4b95.33b.'
export LESSBINFMT='*s\%o'
export JLESSKEYCHARSET='latin1'

if [[ "${OSTYPE}" != cygwin ]]; then
    limit coredumpsize 0
fi

