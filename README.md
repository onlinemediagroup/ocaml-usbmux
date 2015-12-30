This is a library and command line tool to control port forwarding to
jailbroken iOS devices.

Basically, it lets you do:

```shell
$ ssh root@localhost -p 2000
```

and you get a shell to an iDevice connected over a USB wire.

The command line tool is called `gandalf` and it requires that
`usbmuxd` be running. If on OS X then you don't have to do anything,
if on Linux then you need to have the open source version of
[usbmuxd](https://github.com/libimobiledevice/usbmuxd). I recommend compiling from source, versions on most package
managers are old.

# Installation

You need to have opam installed, it is OCaml's package manager.

Once you have opam on your system clone this repository and do: 

```shell
$ opam pin add usbmux . -y
```

This should install both the command line tool gandalf and the usbmux
OCaml library.

# gandalf usage.

The following are a series of usages of `gandalf`, all short form
arguments have long-forms as well and `-v` can be added at any time.

1.  See with realtime updates what devices are connected 
    
    ```shell
    $ gandalf
    ```
    
    This will start up `gandalf` in listen mode, that is it will print
    out whenever a device connects or disconnects.

2.  Start with a mapping file which is of the form `<udid>:<port>`. The
    `#` character starts comments
    
    ```shell
    $ gandalf -m mapping
    ```

2.1) You can also daemonize `gandalf` with the `-d` flag. **NOTE**: You
might need to end up doing that under sudo as `gandalf` needs to
make a pid file under `/var/run`.

1.  To see a pretty JSON representation of devices and their ports that
    are currently connected, do:
    
    ```shell
    $ gandalf -s
    ```

2.  To reload the `gandalf` with a new set of mappings, do:
    
    ```shell
    $ gandalf -r
    ```
    
    This will cancel all running threads and reload from the original
    mappings file, so make your changes there.

3.  To cleanly exit `gandalf`, do:
    **NOTE** This might require super user permissions.
    
    ```shell
    $ gandalf -e
    ```

Check out the man page, accessible with:

```shell
$ gandalf --help
```

or 

```shell
$ man gandalf
```
