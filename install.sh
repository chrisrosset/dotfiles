#!/bin/sh
dotfiles="$HOME/.dotfiles"

CommandAvailable ()
{
    [ -n "$1" ] && command -v "$1" 2>&1 > /dev/null
}

install_rc()
{
	IFS=`printf ' '` && for dotpath in $@; do

		file=$(echo $dotpath | sed 's/\//\t\t/' | sed "s/^.*\t\t//")
		target="$HOME/$file"

		echo "   $file"
		if [ ! -e "$dotfiles/$dotpath" ]; then
			echo "      - File not available in the dotfiles; skipping."
			continue
		elif [ -e "$target" ] && [ ! -L "$target" ]; then
			echo "      - Found a regular file or a directory; skipping."
			continue
		elif [ -L $target ]; then
			echo "      - Found a symlink; deleting."
			rm $target
		fi

		echo "      - Creating symlink."
		ln -s "$dotfiles/$dotpath" $target
	done
}

################################################################################

if [ ! -d "$dotfiles" ]; then
	echo "The dotfiles folder must be located in $HOME/$dotfiles."
	exit 1
fi

for program in *; do
    # skip top level files
    [ ! -d ${program} ] && continue

    if [ "${current_prog}" != "${program}" ]; then
        printf $program | tr '[:lower:]' '[:upper:]'
    else
        current_prog=${program}
    fi

    if CommandAvailable "$program"; then
        echo
    else
        echo " (not installed; skipping)"
        continue
    fi

    for rc in $program/.*; do
        ( [ ${rc} == "${program}/." ] || [ ${rc} == "${program}/.." ] ) && continue

        install_rc ${rc}
    done
done
