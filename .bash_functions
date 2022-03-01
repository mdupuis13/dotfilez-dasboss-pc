# this function uses spark bash script from https://github.com/holman/spark
function sine_wave() {
    SIN=$(python -c "from math import *;print map( lambda x: ceil(6*sin((x+$i)*pi/5)), range($(tput cols)) )" | tr -d '[]' | spark)
    echo -ne $SIN\\r
}


########################
# git helper functions #
########################

function find_git_branch() {
    # $1 == starting folder
    # $2 == git command
    # echo "0 -> $0"
    # echo "1 -> $1"
    # echo "2 -> $2"

    set -x
    find $1 -exec test -d {}/.git \;  -print -execdir git -C {} --no-pager branch --list "$2" \;
}

function find_in_git_repos() {
    # $1 == starting folder
    # $2 == regex to search for
    echo "0 -> $0"
    echo "1 -> $1"
    echo "2 -> $2"

    set -x

    find $1 -exec test -d {}/.git \;  -print -exec egrep "$2" \;
}
