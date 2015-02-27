#!/bin/bash

#PROGRAMLIST="mu mbsync zsh gpg-agent ruby tj3 urxvt emacs unison git openconnect"
PROGRAMLIST="mu zsh gpg-agent emacs unison git openconnect"
HOMEDIR="/home/christian"
CONFIGDIR="$HOMEDIR/ownCloud/.configs"

function symlinks {
    echo "Creating symlinks"
#    ln -fs $CONFIGDIR/.offlineimap $HOMEDIR/.offlineimaprc
    ln -fs $CONFIGDIR/Maildir/ $HOMEDIR/.emails
    ln -fs $CONFIGDIR/.zshrc $HOMEDIR/.zshrc
}

function install_eOS_addon {
    sudo apt-add-repository ppa:versable/elementary-update
    sudo apt-get update
    sudo apt-get install elementary-tweaks
}

function install_emacsColorTheme {
    VERSION=6.6.0
    cd /tmp/
    wget http://download.savannah.gnu.org/releases/color-theme/color-theme-$VERSION.tar.gz
    tar zxvf color-theme-$VERSION.tar.gz
    mkdir ~/dotfiles/emacs/site-lisp/
    cp -r color-theme-$VERSION ~/dotfiles/emacs/site-lisp/color-theme
}

function install_conda {
    CONDAVERSION=3.8.3
    CONDAPATH=/usr/local/conda
    cd /tmp
    if [ ! -f Miniconda-$CONDAVERSION-Linux-x86_64.sh ]; then
	wget http://repo.continuum.io/miniconda/Miniconda-$CONDAVERSION-Linux-x86_64.sh
    fi
    if [ ! -d $CONDAPATH ]; then
	sudo mkdir $CONDAPATH
	sudo chown christian:christian $CONDAPATH
    fi

    chmod +x Miniconda-$CONDAVERSION-Linux-x86_64.sh
    ./Miniconda-$CONDAVERSION-Linux-x86_64.sh -b -p $CONDAPATH/$CONDAVERSION

    }

function install_git {
	sudo apt-get install git
}

function install_gnutls {
	# NOTE: You should really stick with the package manager on this one!
	GTLS_VERSION=gnutls-3.2.9
	wget ftp://ftp.gnutls.org/gcrypt/gnutls/v3.2/$GTLS_VERSION.tar.xz
}

function install_openconnect {
	OPENCONNECT_VERSION=openconnect-6.00
	sudo apt-get install openssl-dev
	cd /tmp
	rm -r openconnect*
	wget ftp://ftp.infradead.org/pub/openconnect/$OPENCONNECT_VERSION.tar.gz
	wget http://git.infradead.org/users/dwmw2/vpnc-scripts.git/blob_plain/HEAD:/vpnc-script
	sudo mkdir /etc/vpnc/
	sudo cp vpnc-script /etc/vpnc/
	sudo chmod +x /etc/vpnc/vpnc-script
	tar zxvf $OPENCONNECT_VERSION.tar.gz
	cd $OPENCONNECT_VERSION
	./configure
	make -j4
	sudo make install
	sudo ln -s /usr/local/lib/libopenconnect.so.3 /usr/lib/libopenconnect.so.3
}

function install_mu {
    mkdir ~/src/
    cd ~/src/
    sudo apt-get install libgmime-2.6-dev libxapian-dev isync texinfo autoconf
    if [ -d mu ]; then
	cd mu/
	git checkout
    else
	git clone git://github.com/djcb/mu.git
    fi
    autoreconf -i && ./configure && make
    sudo make install
}

function install_zsh {
    sudo apt-get install zsh curl
    curl -L http://install.ohmyz.sh | sh
    chsh -s /bin/zsh
}

function install_gpg-agent {
    sudo apt-get install gnupg-agent
}

function install_urxvt {
    sudo apt-get install rxvt-unicode-256color
}

function install_emacs {
	EMACS_VERSION=emacs-24.4
	cd /tmp
	# Install devel files to build emacs
	sudo apt-get install xorg-dev libjpeg-dev libtiff4-dev libpng12-dev libgif-dev libxpm-dev libtinfo-dev libncurses5-dev libgnutls-dev imagemagick libgconf2-dev
	wget http://ftp.download-by.net/gnu/gnu/emacs/$EMACS_VERSION.tar.gz
	tar zxvf $EMACS_VERSION.tar.gz
	cd $EMACS_VERSION
	./configure
	make -j 4
	sudo make install
	sudo ln -s /usr/local/bin/emacs /usr/bin/emacs
}

function install_latex {
    if [ 1 ]; then
    sudo apt-get install texlive-base texlive-fonts-extra texlive-fonts-recommended texlive-lang-danish texlive-latex3 texlive-latex-recommended texlive-science texlive-pictures
    else 
    TL_VERSION=20140813
    cd /tmp
    wget http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz
    sudo mkdir -p /usr/local/texlive/2014
    sudo chown -R christian:christian /usr/local/texlive
    cd install-tl-$TL_VERSION
    ./install-tl -repository http://mirrors.dotsrc.org/ctan/systems/texlive/tlnet/
    fi
}

function install_anaconda {
    cd /tmp
    ANA_VERSION=2.0.1
    wget http://repo.continuum.io/archive/Anaconda-$ANA_VERSION-Linux-x86_64.sh

    sudo mkdir /usr/local/anaconda/
    sudo chmod -R christian:christian /usr/local/anaconda/

    chmod +x Anaconda-$ANA_VERSION-Linux-x86_64.sh
    ./Anaconda-$ANA_VERSION-Linux-x86_64.sh -p /usr/local/anaconda/$ANA_VERSION/ -b
    
    # Install extra packages
    conda install shapely
}

function install_spm {
    install_git
    mkdir ~/code
    cd ~/code
    git clone git@github.com:LinuxChristian/SPM.git
    cd SPM/
    
    # Checkout Erosion branch
    git fetch origin
    git checkout -b Erosion origin/Erosion
}

function install_blender {
    BLENDER_VERSION=2.71
    cd /tmp
    wget http://mirror.cs.umn.edu/blender.org/release/Blender$BLENDER_VERSION/blender-$BLENDER_VERSION-linux-glibc211-x86_64.tar.bz2
    tar xjvf blender-$BLENDER_VERSION-linux-glibc211-x86_64.tar.bz2
    sudo mkdir /usr/local/blender
    sudo chown christian:christian /usr/local/blender
    mv blender-$BLENDER_VERSION-linux-glibc211-x86_64 /usr/local/blender/$BLENDER_VERSION
    
}

function install_unison {
	cd /tmp
	rm -r unison*
	sudo apt-get install libgtk2.0-dev ocaml liblablgtk2-ocaml-dev
	# Get unison from mirror
	UNISON_VERSION=unison-2.40.65
	wget http://www.seas.upenn.edu/~bcpierce/unison//download/releases/$UNISON_VERSION/$UNISON_VERSION.tar.gz
	tar zxvf $UNISON_VERSION.tar.gz
	cd $UNISON_VERSION

	# Build commandline version
	make UISTYLE=text THREADS=true
	sudo cp unison /usr/bin/unison

	make clean
	make UISTYLE=gtk2 THREADS=true
	sudo cp unison /usr/bin/unison-gtk
}

function install_ruby {
    sudo apt-get install ruby
    sudo gem
}

# Taskjuggler for org-mode
# http://orgmode.org/worg/org-tutorials/org-taskjuggler3.html
function install_tj3 {
    sudo gem taskjuggler
}

function install_mbsync {
    sudo apt-get install isync
    echo "To setup isync read: "
    echo "http://www.macs.hw.ac.uk/~rs46/posts/2014-01-13-mu4e-email-client.html"
}

for prog in $PROGRAMLIST; do

    if [ "$1" == "" ]; then
    # Check if mu is installed
    if [ -z "`which $prog`" ]; then
	while true; do
	    echo "$prog is not installed"
	    read -p "Do you wish to install this program?" yn
	    case $yn in
		[Yy]* ) install_$prog; echo -e '\e[0;32mInstalled $prog\e[0m'; break;;
		[Nn]* ) echo "Install skiped"; break;;
		* ) echo "Please answer yes or no.";;
	    esac
	done
    fi
    else
	install_$1
	echo -e '\e[0;32mInstalled' "$1" '\e[0m'
	exit
    fi

done


symlinks

echo "System is up-to-date"
