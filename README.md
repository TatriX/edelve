# Edelve — Emacs frontend for delve, the Go debugger

`Edelve` uses [dlv](https://github.com/go-delve/delve) to run and debug your go application.

## Installation
> [!WARNING]
> This project is in very early stages!

The project is not yet ready for wider audience, so if you want to
give it a try, we expect you to know how to install it yourself.

```elisp
(use-package edelve
    :hook (go-mode . edelve-minor-mode))
```

`edelve-minor-mode` enables `edelve-minor-mode-map`:

```
  "<f5>" #'edelve-restart
  "<f8>" #'edelve-continue
  "S-<f8>" #'edelve-halt
  "<f9>" #'edelve-toggle-breakpoint
  "<f10>" #'edelve-next
  "<f11>" #'edelve-step
  "C-c C-p" #'edelve-print-dwim
  "C-c C-u" #'edelve-up
  "C-c C-d" #'edelve-down
  "C-c C-e" #'edelve-eval
```

## Usage

> [!TIP]
> If you experience problems or need more features, please create an issue!

Open any file in your project and run `M-x edelve`.
This will run `dlv debug` under the hood and attach to it.
From there hit `F8` to start the process, since it's halted by default.

Process output goes into `*edelve-process-output*`.

Put a breakpoint with `F9`. Hit it again on a line with a breakpoint to clean it.

> [!NOTE]
> If you edit the code, breakpoint locations will drift creating a
> discrepancy between where we show them and where they are actually
> attached if any. It would be nice if we could make it work better, but
> for now you probably want to know that you can use `M-:
> (remove-overlays)` to clean the fringes.

To halt a program use `Shift-F8`. And `F8` to continue.

`F10` and `F11` for step-over and step-into respectively

Then, whenever you want to recompile and restart, hit `F5`.

> [!NOTE]
> `dlv` creates __debug* files in your working directory, which we should eventually cleanup.

When the program is stopped, use `C-c C-u` and `C-c C-d` to move between stackframes.

You can inspect things using `C-c C-p`. It works when the
point on a symbol of if there is an active region.

Finally, use `M-x edelve-quit` to kill the session.


## How does it work?
`Edelve` works by running an instance of `dlv` in a subprocess and communicates with it using [JSON-RPC interface](https://github.com/go-delve/delve/blob/master/Documentation/api/json-rpc/README.md)
