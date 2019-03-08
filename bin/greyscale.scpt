#!/usr/bin/osascript

quit application "System Preferences"

use application "System Events"
use application "System Preferences"

property process : a reference to process "System Preferences"
property window : a reference to window "Accessibility" of my process
property checkbox : a reference to checkbox "Use greyscale" of group 1 of my window

property pane : a reference to pane id "com.apple.preference.universalaccess"
property anchor : a reference to anchor "Seeing_Display" of my pane

reveal my anchor

repeat until my checkbox exists
end repeat

click my checkbox

quit application "System Preferences"
