#!/bin/bash

# exit when any command fails
set -e

# git
sudo apt install git
git config --global user.name "Andrew Kraemer"
git config --global user.email "kidman007@gmail.com"
echo 'git installed'

# htop
sudo apt install htop

# python
cd ~/Downloads
sudo apt install libgl1-mesa-glx libegl1-mesa libxrandr2 libxrandr2 libxss1 libxcursor1 libxcomposite1 libasound2 libxi6 libxtst6
sudo wget --continue https://repo.anaconda.com/archive/Anaconda3-2020.07-Linux-x86_64.sh --output-document anaconda_installer.sh
sudo bash anaconda_installer.sh

sudo pip install flake8 jedi black # required for emacs tools
sudo conda create -n py38 python=3.8
sudo conda activate py38
sudo conda install pip
sudo pip install flake8 jedi black
echo 'python 3.8 enviornment created'

# Looks
sudo apt install gnome-tweak-tool
cd ~/Downloads
sudo git clone https://github.com/EliverLara/Nordic
sudo mkdir ~/.themes
sudo mv Nordic ~/.themes/

sudo git clone https://github.com/zayronxio/Zafiro-icons.git
sudo mv Zafiro-icons ~/.local/share/icons/.
echo 'Set Theme to Nordic using Tweak Tool'
echo 'Set icons to Zafiro using Tweak Tool'

# font
sudo apt-get install font-manager

## fira
sudo add-apt-repository universe
sudo apt install fonts-firacode

## source code pro
wget https://github.com/downloads/adobe/Source-Code-Pro/SourceCodePro_FontsOnly-1.009.zip \
    && unzip SourceCodePro_FontsOnly-1.009.zip \
    && sudo mkdir /usr/share/fonts/truetype/source-code-pro \
    && sudo cp SourceCodePro_FontsOnly-1.009/*.ttf /usr/share/fonts/truetype/source-code-pro


# emacs 
cd ~
sudo apt-get install emacs
sudo git clone https://github.com/akraemer007/emacs_init.git
sudo mv emacs_init .emacs.d
sudo mkdir .emacs.d/tmp

# video
sudo add-apt-repository multiverse
sudo apt install ubuntu-restricted-extras

# pandoc
# may need to run
sudo sed -i '/cdrom/d' /etc/apt/sources.list # in case the cd-rom is in the sources list.
sudo apt-get install pandoc

# latex-tooling
sudo apt-get install texlive
# pdftex blah.tex

# elf shell?
# window setup I like
# background

# software installs
## Dropbox
## Gimp
## 1password (through mozilla)


