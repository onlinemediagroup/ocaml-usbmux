This is a library and command line tool to control port forwarding to
iOS devices.

Basically, it lets you do:

```shell
$ ssh root@localhost -p 2000
```

and you get a shell to a iDevice connected over a USB wire.

# Installation

In this cloned repository, do: 

```shell
$ opam pin add usbmux . -y
```

This should install both the command line tool gandalf and the usbmux
OCaml library.

# gandalf usage.

You can use the command line tool just by using it on the comamnd
line.

```shell
$ gandalf
```

This will start up `gandalf` in listen mode, that is it will print out
whenever a device connects or disconnects.

Doing:

```shell
$ gandalf -m mapping
```

&#x2026;where m is a mapping file of lines in the form of:

`b686cf1a8fa87fa861462955edf5811a71841447:2000`
hashmarks are comments.

you can also daemonize `gandalf` with the `-d` flag.

`gandalf` can be louder about what's happening behind the scenes by
invoking it with the `-v`, `--verbose` flag.

Check out the man page, accessible with:

```shell
$ gandalf --help
```

or 

```shell
$ man gandalf
```
