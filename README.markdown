# erlang-history #

erlang-history (eh) is a tiny pair of files that can be used to patch an Erlang-OTP system to add support for history in the Erlang shell.

The history supported is the one available through up/down arrows on the keyboard.

## How to install ##

Automatically (you may need to run this command with `sudo`):

 `$ make install`

Manually:

1. Find out what version of the Kernel library you're using by using `application:which_applications()` in the Erlang shell. The version number is the last element of the tuple.
2. Compile the files (`erl -make`).
3. Take the `.beam` files in `ebin/$VSN/` and move them to `$ROOT/lib/kernel-$VSN/ebin/` for the OTP release of your choice.
4. Open the kernel app file (`$ROOT/lib/kernel-$VSN/ebin/kernel.app`) and add `group_history` to the modules list.
5. Start the Erlang shell associated with this version of the Erlang/OTP kernel to gain shell history.
6. In case you want to remove the patch, just recompile `$ROOT$/lib/kernel-$VSN/src/group.erl`, and move the resulting `.beam` back into the `ebin/` directory. Alternatively, make a backup beforehand. Don't forget to remove the `group_history` module from the kernel app file's modules list.

## Configuration Options & Features ##

By default, the shell history will be enabled. To disable it, the kernel application variable `hist` can be set to `false` to disable history.

Options include:

- `hist` - `true | false`: enables or disables shell history. Default value is `true`
- `hist_file` - `string()`: gives the path to where the history should be saved. By default, the file sits in your home directory as `.erlang-history.$NODENAME`. The node name will *always* be appended to the file name as a way to manage conflicts and accounts.
- `hist_size` - `1..N`: how many input lines the shell should remember. By default, the value is set to 500.
- `hist_drop` - `["some", "string", ...]`: lines you do not want to be saved in the history. As an example, setting `hist_drop` to `["q().","init:stop().","halt()."]` will avoid saving most manual ways of shutting down a shell. By default, no terms are dropped.

If you are not familiar with Erlang application variables, there are two principal ways to handle them. The first one is to pass the arguments manually to `erl` as follows:

    erl -kernel hist_size 120 -kernel hist_drop '["q().","init:stop()."]'

The other way is to create a configuration file, looking a bit as follows:

    [{kernel,[
      {hist_size, 120},
      {hist_drop, ["q().", "init:stop()."]}
    ]}].

Then start the Erlang shell by doing:

    erl -config hist.config

that is, if we assume `hist.config` is the name of your configuration file. If you're in a unix-like system, you can then alias the 'erl' command to run whatever you need:

    alias erl='erl -config hist.config'

And then use it everywhere. Last but not least, if you feel dirty, you can directly find the `.app` file of the kernel application (in its `ebin/` directory) and write the values in there.

## FAQ (or planned FAQs) ##

### Is this going to be in OTP? ###

So far I haven't planned to push this into OTP. I have no idea how reliable the code is, wrote pretty much no tests (testing this shit is hard, although unit tests would be possible) outside of trying stuff for myself.

I also feel the whole thing is a bit too hackish and I do not believe it would be up to the OTP team's standard, but if it were to be, why not? I could commit that stuff inside OTP for sure.

### How do you store history? ###

I've used DETS tables at this point in time, as it was (at first) easy to store stuff that way. The old requests are injected into the shell when it first starts up (and it does so for all instances of the shell on a given node. Every time a new line is typed in (as the existing shell sees it fit -- I just tied myself into the existing code), it is saved into the database.

Yes, this means there is a 2GB hard limit on how much history you can store per Erlang node. I hope you won't need that, but oh well. Hopefully you don't need that much stuff.

### What's the license? ###

I don't know yet! I'm hesitating between MIT and BSD, although if this were to make it into Erlang/OTP, I'd go with the Erlang Public License to fit with the rest. We'll see how it goes and what people want.

### I'm a man with the head of a horse, the torso of a wasp and the legs of a spider ###

Congratulations, I guess.

### Does this handle more advanced history functionality than just up/down arrows? ###

No, it doesn't.

It would be nice to support backwards search with ^R, but I didn't go this far into implementing things.

Users of the shell history functions (`h()` and `v(N)`) will be disappointed to know I haven't added history support for that. Although it is possible (and I have some demo code to do so), I decided it was not worth including because of all the weird problems created. These commands save the function called as Erlang term tokens (after parsing, before evaluation) and the full results of the calls. This means that a lot of data or state could be carried over sessions while no longer making sense; things like pids, sockets, etc. Rather than dealing with that, I decided it would be simpler for people to just replay the previous functions.

If you want the demo code that handled these things, let me know.

### What if the database gets corrupted? ###

DETS repairing of tables should be properly supported. In case of a database corrupted beyond repair, removing the DB file and starting over again will work fine.

### What Versions of Erlang/OTP does this work with? ###

I've tested it with all versions from R13B04 up to 17.0. It worked fine for them. I plan on supporting all newer versions. ALL OF THEM.

## Author ##

- Fred Hebert (ferd, MononcQc).

## Thanks ##

Thanks to Robert Virding & Felix Lange for the guidance through Erlang's IO system and the fun discussions at the 2011 EUC's hackathon. It was a pretty fun day and that's where I first prototyped this.

Thanks to Richard Jones for providing the original Makefile for this and fixing a bug when generating releases, Alexander Alexeev for making the installing procedure more general, and Radosław Szymczyszyn for the fixes to make things work with R13B04.

For other bug fixes, we have to thank @jan--f and @hypernumbers
