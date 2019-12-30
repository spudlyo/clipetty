# Clipetty

[![Build Status](https://github.com/spudlyo/clipetty/workflows/CI/badge.svg)](https://github.com/spudlyo/clipetty/actions)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Clipetty is a minor mode for terminal (TTY) users that sends text that you kill in Emacs to your Operating System's clipboard. If you predominately use Emacs in GUI (X-Windows, macOS, Windows) frames you don't need Clipetty.

For this to work you need to be using a terminal emulator that supports OSC 52 escape sequences. See the [Terminals](#terminals) section below to check if your favorite terminal emulator is on the list.


## Reasons to Use Clipetty Over Emacs' Built-in Xterm Support

Here are some reasons why you might want to use Clipetty instead of the native OSC 52 functionality found in Emacs' [xterm.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/term/xterm.el):

-   Clipetty supports both GNU screen and Tmux, either locally or over SSH
-   Clipetty supports [nested GNU Screen or Tmux sessions](#nested)
-   Clipetty allows for [seamless detach/re-attach with Tmux](#stale) on remote hosts
-   Clipetty is [compatible](#kitty) with the Kitty terminal emulator


## Install

If you want Clipetty to work everywhere, you can enable `global-clipetty-mode`, otherwise you can enable `clipetty-mode` on a buffer-by-buffer basis.


### Use-package

If you're using `use-package` you can add this to your `init.el` file:

```
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))
```


### Manual

If you manually installed `clipetty.el` somewhere on your `load-path` you can add:

```
(require 'clipetty)
(global-clipetty-mode)
```


### Using Clipetty From a Key Binding

Sometimes it can be annoying to have Clipetty overwrite your system clipboard every time you kill something. Let's say you want to paste something from your system clipboard into Emacs, but before you can paste it, you need to do some minor editing (like hitting `C-k` or `M-d`) which kills some text. Now you've overwritten your system clipboard (which doesn't have a kill ring) and you have to copy it all over again. Doh!

You can invoke Clipetty explicitly from a key binding to copy a region to the clipboard rather than using either the local or global minor modes. To that end, Clipetty has a function called `clipetty-kill-ring-save` which I like to bind to `M-w` like so:

```
(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))
```


## How Clipetty Works

Clipetty does its magic by assigning the `clipetty-cut` function to Emacs' `interprogram-cut-function` variable, which is what happens when you activate `clipetty-mode`. When the mode is active, every time you kill a line or region Clipetty gets sent the content that is destined for the kill ring. The `clipetty-cut` function takes this content, converts it to base64, wraps it in an [ANSI OSC](https://en.wikipedia.org/wiki/ANSI_escape_code#Escape_sequences) 52 escape sequence, and then sends it to your terminal. Terminal programs which support OSC 52 commands will react to this by stripping off the escape sequence, decoding the base64 content, and then inserting the resulting string into the system clipboard.


<a id="terminals"></a>

## Terminals that Support OSC Clipboard Operations

-   [xterm](https://invisible-island.net/xterm/ctlseqs/ctlseqs.txt) (Unix &#x2013; Where OSC 52 escape sequences originated)
-   [iTerm2](https://iterm2.com) (macOS)
-   [Alacritty](https://github.com/jwilm/alacritty) (macOS, Linux, BSD, WIndows)
-   [kitty](https://sw.kovidgoyal.net/kitty/) (macOS, Linux)
-   [mintty](https://mintty.github.io/) (Windows)
-   [hterm](https://hterm.org) (Javascript)

This is not an exhaustive list, these are just the ones I know about. Submit a PR if you know of any I missed.


<a id="kitty"></a>

### Kitty

The `kitty` terminal gets honorable mention for extending the `xterm` protocol to [support larger clipboards](https://sw.kovidgoyal.net/kitty/protocol-extensions.html#pasting-to-clipboard). While Clipetty at this time does not support Kitty's larger clipboard, it is compatible, which means you don't have to disable Kitty's protocol extension with `no-append`.


## Clipetty and Terminal Multiplexers

If you're running Emacs under a terminal multiplexer like `tmux` or `screen`, these programs will intercept these ANSI OSC 52 escape sequences, and if they don't think your terminal supports OSC 52 (i.e. you don't have a very specific `terminfo(5)` capability) they'll not pass them on to your outer terminal. With enough tweaking you can prevent them from doing this, but it can be a challenge. Running Emacs on a remote host with nested terminal multiplexers (something I often do) can further complicate things.

Clipetty attempts to deal with this problem by looking for environment variables that indicate you're using a terminal multiplexer, and then wrapping the OSC 52 escape sequence in a "Device Control String" (DCS). This presence of a DCS tells `tmux` or `screen` to unwrap the message and send it along unmolested, where it can be interpreted by the outer terminal. Clipetty handles the case of nested terminal multiplexers by writing the DCS wrapped OSC 52 escape sequence directly to your `$SSH_TTY` thereby bypassing the terminal multiplexer on the remote host entirely.


<a id="stale"></a>

### Dealing With a Stale `SSH_TTY` Environment Variable

Let's say you SSH into a host, start `tmux`, and then run Emacs. A little later you detach your session and log out. You then SSH back into the same host, and re-attach your session. Your Emacs process is still running right where you left it, but the `$SSH_TTY` environment variable it inherited from the shell is now stale (or longer accurate) as it still points to your *old* SSH tty. This means that Clipetty will no longer function in `tmux` windows that were created during your previous login until you manually update the `$SSH_TTY` environment variable.

Thankfully in `tmux` there is an easy way of dealing with this problem! Add the following to your `.tmux.conf` file:

```
set -ag update-environment "SSH_TTY"
```

This will tell `tmux` to update its local `$SSH_TTY` environment variable when you re-attach, and Clipetty will ask `tmux` about it rather than relying on the (possibly stale) variable that Emacs inherited from the shell.


## Customization

You can run `M-x customize-group RET clipetty RET` to use Emacs' Easy Customization Interface or you can manually set some of the variables below in your `init.el`:


<a id="nested"></a>

### Configuring Multiplexer Nesting

The `clipetty-assume-nested-mux` variable, when set to a non-nill value, tells Clipetty to assume that if you're running a terminal mulitplexer on a remote host that it's nested &#x2013; that is to say that you're also running the *same* terminal multiplexer on the local host.

```
(setq clipetty-assume-nested-mux nil)
```


### Configuring Tmux Integration

The `clipetty-tmux-ssh-tty` variable tells Clipetty how to run `tmux` to query it's local `SSH_TTY` environment variable. This default assumes that `tmux` is on your PATH. If `tmux` lives elsewhere for you, or it is named something else, you can change it here.

```
(setq clipetty-tmux-ssh-tty "tmux show-environment SSH_TTY")
```


## Acknowledgements

This code was inspired by `osc52.el` by the Chromium OS Authors, which was very helpful in showing me how this could be done, but lacked support for `tmux` and nested terminal multiplexers. I'd also like to thank Suraj N. Kurapati, as I learned a lot by studying his shell script `yank`.
