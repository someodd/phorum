# gopherden: Gopher Protocol BBS

An anonymous, ephemeral text bulletin board system for the Gopher Protocol written in Haskell.

## Try it out

Run the server:

```
cabal run . -- launch
```

Visit the gopherhole:

```
gopher -p "/" localhost 7000
```

Here are some commands related to banning:

  * `cabal run . -- ban --post 1 --delete --reason "rule violation"`: ban user for post #1 and delete said post for the `--reason` supplied.
  * `cabal run . -- unban --ip someipv6addr`: remove the ipv6 address from the ban table

## Features

  * View threads as an ASCII-art text file
  * Navigate the board as a menu
  * Post threads and replies
  * Ban system

## Features probably coming soon

  * Currently has a `Config.hs` with many configurable parameters. Coming soon is a config
    file to set these parameters. This also allows for internationalization. This config
    file will also allow you to set some server parameters.
  * Text wrapping
  * Captcha-like system
  * Build with nix
  * Debian package + systemd daemon
  * Search!
  * Post rate limit
  * Configuration file
