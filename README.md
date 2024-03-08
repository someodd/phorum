# gopherden: Gopher Protocol BBS

An anonymous, ephemeral text bulletin board system for the Gopher Protocol
written in Haskell.

## Try it out

I feel these might be true for why you should try Nix:

  * Easier to build on a variety of systems (mac, linux)
  * Easier to get a development system up and running (a demo database, gopher
    client)

If you have `nix` installed and `experimental-features = flakes nix-command` in
your `~/.config/nix/nix.conf`, I think this should work:

```
nix develop
nix run .#gopherden -- launch
```

Within `nix develop` you can also use `gopher -p "/" localhost 7000` to visit
the gopherhole.

I believe the demo database will be deleted when you exit any `nix develop`
session!

Here are some commands related to banning:

  * `nix run .#gopherden -- ban --post 1 --delete --reason "rule violation"`:
    ban user for post #1 and delete said post for the `--reason` supplied.
  * `nix run .#gopherden -- unban --ip someipv6addr`: remove the ipv6 address
    from the ban table

If you want to just use `cabal` to run just use `cabal run . -- launch` and the
like.

## Features

Here are some features:

  * View threads as an ASCII-art text file
  * Navigate the board as a menu
  * Post threads and replies
  * Ban system
  * Post rate limiting
  * Customizable! TOML configuration file!

## Features probably coming soon

  * Text wrapping
  * Captcha-like system
  * Debian package + systemd daemon
  * Search!
  * Post length limit (tools!)
