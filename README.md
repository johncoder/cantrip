<div align="center">

<h1>Cantrip</h1>

<p>An Emacs package for building transient prefixes from JSON files.</p>
<hr />

![cantrip example animated gif](https://raw.githubusercontent.com/johncoder/cantrip/master/cantrip-example.gif)

</div>


<hr />

## Installation

Clone this repository:

```sh
git clone https://github.com/johncoder/cantrip
```

Then, add this to your emacs configuration:

```lisp
(load-file "~/path/to/cantrip/cantrip.el")
```
By default, it binds to `C-x a r`, and looks for `package.json` or `scripts.json` in the dominating directory that contains a `.git` folder.

A scripts file looks something like this:

```json
{
  "scripts": {
    "two": "pwd",
    "foo": "ls -la",
    "foo:bar": "git status",
    "foo:baz": "date",
    "foo:bar:qaz": "cat scripts.json | grep \"git\""
  }
}
```

Cantrip uses a `:` delimited convention for chaining transient prefixes. It will automatically select an alias for segments using the unique letters of the label, or overflow to another option when necessary. When you execute one of the scripts, it uses `compile` to run the respective script as a compilation command within the git repository.

## Using Cantrip Globally

You can use `cantrip-define-prefix` to build a prefix manually so that you can make one globally available. Example:

```lisp
(global-set-key (kbd "C-x a g")
                (cantrip-define-prefix "~/.cantrip.json"
                                       "my-cantrip"))
```

## Use `*ansi-term*` To Run Commands

You can change how cantrip dispatches commands! For example, you can change it to send commands directly to your open `*ansi-term*` buffer.

```lisp
(setq cantrip-dispatch-command
      #'(lambda (command)
          (switch-to-buffer-other-window "*ansi-term*")
          (comint-send-string "*ansi-term*" (concat command "\n"))))
```

## Altering Commands

You can alter a command before it is dispatched. For example, if you'd like to run the command using `npm` or `yarn`:

```lisp
(setq cantrip-transform-command
      #'(lambda (key command args)
          (if (locate-dominating-file default-directory "yarn.lock")
              (format "yarn run %s %s" key args)
            (concat command " " args))))
```
