# macscript-wm
Tiling window manager for macOS, built on the MacScript library.

### Key bindings

| Hotkey       | Command      |
| ---------- | ------------ |
| alt + shift + i | send focused window to space on the left |
| alt + shift + o | send focused window to space on the right |
| alt + x | toggle normal keyset |
| alt + c | toggle mouse keyset |
| alt + z | toggle window keyset |
| alt + w | close focused window |
| alt + r | rotate layout clockwise |
| alt + n | decrement master/slave split ratio |
| alt + m | increment master/slave split ratio |
| alt + shift + = | equalize master/slave split ratio |
| alt + shift + 1 | send focused window to primary display |
| alt + shift + 2 | send focused window to secondary display |
| alt + 1 | focus primary display |
| alt + 2 | focus secondary display |
| alt + , | increase number of master panes |
| alt + . | decrease number of master panes |
| alt + t | tile focused window |
| alt + u | untile focused window |
| alt + space | switch to next layout |
| alt + return | send focused tile to master pane |

`alt + x/c/z` switch between different keysets. A keyset allows to reuse the same hotkey for different operations. The keyset that is currently active is shown on the status bar with a letter:

| Letter | Key set |
| ------ | ------- |
| N      | Normal key set |
| M      | Mouse key set |
| W      | Window key set |

All keybindings shown above work in any keyset. Keyset-specific bindings are shown below.

#### Normal key set

| Hotkey | Command |
| ------ | ------- |
| alt + j | focus towards tile below |
| alt + k | focus towards tile above |
| alt + h | focus towards tile on the left |
| alt + l | focus towards tile on the right |
| alt + shift + j | swap with tile below |
| alt + shift + k | swap with tile above |
| alt + shift + h | swap with tile on the left |
| alt + shift + l | swap with tile on the right |
| alt + ; | set mouse cursor to center of the screen |

#### Mouse key set 

Note that `alt + ;` waits 1 second before performing a mouse click, so that you have the time to release the keys and avoid triggering an `alt + ; + click` event instead.

| Hotkey | Command |
| ------ | ------- |
| alt + j | move mouse down (coarse) |
| alt + k | move mouse up (coarse) |
| alt + h | move mouse left (coarse) |
| alt + l | move mouse right (coarse) |
| alt + shift + j | move mouse down |
| alt + shift + k | move mouse up |
| alt + shift + h | move mouse left |
| alt + shift + l | move mouse right |
| alt + ; | delayed mouse click |

#### Window key set

| Hotkey | Command |
| ------ | ------- |
| alt + j | move window down (coarse) |
| alt + k | move window up (coarse) |
| alt + h | move window left (coarse) |
| alt + l | move window right (coarse) |
| alt + shift + j | move window down |
| alt + shift + k | move window up |
| alt + shift + h | move window left |
| alt + shift + l | move window right |
