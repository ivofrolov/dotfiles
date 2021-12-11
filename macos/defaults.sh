#!/usr/bin/env bash

set -e

# Font
defaults -currentHost write -globalDomain AppleFontSmoothing -int 0

# Dock
defaults -currentHost write com.apple.dock autohide -bool true
defaults -currentHost write com.apple.dock mineffect -string scale
