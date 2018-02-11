# Copyright © 2017-2018 Vasili Karaev
#
# This file is part of tvsm.
#
# tvsm is free software: you can redistribute  it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# tvsm is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHENTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tvsm. If not, see <http://www.gnu.org/licenses/>.

_tvsm_commands() {
    local commands="add watch ls rm set"

    COMPREPLY=($(compgen -W "$commands" -- "$cur"))
}

_tvsm_add() {
    if [[ "$cur" == --* ]]; then
        local opts="--name --path --airing --ep-current --ep-offset"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    if [[ "$cur" == -* ]]; then
        local opts="-n -p -a -e -o"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    local prev="${COMP_WORDS[COMP_CWORD - 1]}"

    if [[ "$prev" =~ -(p|-path) ]]; then
        _filedir -d
    fi
}

_tvsm_watch() {
    if [[ "$cur" == --* ]]; then
        local opts="--ep --set"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    if [[ "$cur" == -* ]]; then
        local opts"=-e -s"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    COMPREPLY=($(compgen -W "$(tvsm ls -CW)" -- "$cur"))
}

_tvsm_ls() {
    if [[ "$cur" == --* ]]; then
        local opts="--long --nocolor --all \
                    --watching --finished \
                    --airing --completed \
                    --watchable --non-watchable"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    if [[ "$cur" == -* ]]; then
        local opts="-l -C -A -w -f -a -c -W -N"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi
}

_tvsm_rm() {
    if [[ "$cur" == --* ]]; then
        local opts="--finished"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    if [[ "$cur" == -* ]]; then
        local opts="-f"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    COMPREPLY=($(compgen -W "$(tvsm ls -CA)" -- "$cur"))
}

_tvsm_set() {
    if [[ "$cur" == --* ]]; then
        local opts="--name --path --airing --completed --ep-currentt"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    if [[ "$cur" == -* ]]; then
        local opts="-n -p -a -c -e"
        COMPREPLY=($(compgen -W "$opts" -- "$cur"))
        return
    fi

    local prev="${COMP_WORDS[COMP_CWORD - 1]}"

    if [[ "$prev" =~ -(p|-path) ]]; then
        _filedir -d
        return
    fi

    COMPREPLY=($(compgen -W "$(tvsm ls -CA)" -- "$cur"))
}

_tvsm() {
    local c=1
    local word
    local cmd

    while [ "$c" -lt "$COMP_CWORD" ]; do
        word="${COMP_WORDS[c]}"
        case "$word" in
            -h|--help|-v|--version)
                ;;
            *)
                cmd="$word"
                break
                ;;
        esac
        c="$((c + 1))"
    done

    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ -z "$cmd" ]; then
        case "$cur" in
            -*) COMPREPLY=() ;;
            *) _tvsm_commands ;;
        esac
        return
    fi

    case "$cmd" in
        add)   _tvsm_add   ;;
        watch) _tvsm_watch ;;
        ls)    _tvsm_ls    ;;
        rm)    _tvsm_rm    ;;
        set)   _tvsm_set   ;;
        *)                 ;;
    esac
}

complete -F _tvsm tvsm
