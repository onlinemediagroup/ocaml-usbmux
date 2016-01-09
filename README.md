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

On `OS X` you can do:

```shell
$ brew install opam
```

(If on Linux, then get opam via your package manager, aka apt-get or
whatever). It is important that your compiler is up to date, you can
check with `opam switch`, make sure its at least >= 4.02.0

then

```shell
$ opam install usbmux
```

This will install the command line tool `gandalf` and an OCaml
library.

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

Simple invocation:

```shell
$ sudo `which gandalf` --mappings etc/mapping --daemonize --verbose
```

# Important Notes and Catches

1.  If you are running this on Linux, then you might get issues with
    `usbmuxd` having issues when more than around 7 devices are plugged
    in. This is because multiple threads are trying to call various
    `libxml2` freeing functions. I have a forked version of `libplist`
    that `usbmuxd` uses, sans the memory freeing calls. Its available
    [here](https://github.com/onlinemediagroup/libplist). Compile and install that, then compile and install `usbmuxd`
    from source. This will leak memory but its not that much at all and
    I believe it to be a fixed amount.

2.  Another issue you might have is USB3.0. The Linux kernel might crap
    out on you after 13 devices. This is a combination of the kernel
    not giving enough resources and the host controller on your
    motherboard being crappy. The solution to this problem is to
    disable USB3.0 in your BIOS. To verify that USB3.0 isn't working
    check with `lsusb`

For reference, this project is currently in use given these two issues
and works fine with > 20 iPhones.
