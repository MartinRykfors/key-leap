# Key-leap-mode
## A superb way of leaping between lines

Key-leap-mode is a minor mode for Emacs that allows you to quickly jump to any visible line in the current window. When it is active, it will populate the margin of the window with a set of unique three-letter keywords. It can be enabled in a buffer by calling `key-leap-mode`.

When you call the interactive function `key-leap-start-matching`, it will wait for you to input the three characters of the key corresponding to the line you want to leap to.

It is possible to specify what characters to use for each position in the keywords that key-leap-mode generates for you. Please see the commentary in the `key-leap-mode.el` file for more info on how to customize this mode.

## Motivation

This mode was inspired by the `avy-goto-line` command of [avy-mode](https://github.com/abo-abo/avy). I wanted that kind of functionality but with the following differences:
* The keys should be always visible, no overlays appearing in the buffer's contents.
* There should be more control over in what sequence the characters of the keys appear.
Key-leap-mode was the result.

## Contributions

Bug reports and pull requests are welcome!

## License

GPL v3
