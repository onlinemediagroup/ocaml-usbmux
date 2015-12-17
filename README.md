This is a library and command line tool to control port forwarding to
iOS devices.

Basically, it lets you do:

```shell
$ ssh root@localhost -p 2000
```

and you get a shell to a iDevice connected over a USB wire.

# Installation

First you need to have `plist` installed on your machine.

```shell
$ opam install plist
```

Now in this cloned repository, do: 

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

This will start up `gandalf` which will forward requests&#x2026;.

to be continued&#x2026;

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
