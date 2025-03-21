# phorum: Gopher Protocol BBS

An anonymous, ephemeral text bulletin board system for the Gopher Protocol
written in Haskell.

There's a Debian package you can install on Ubuntu and Debian alike, I believe.

It's running on [gopher://gopher.someodd.zip/phorum/](gopher://gopher.someodd.zip/1/phorum/)

## Try it out + more info

Please see [my showcase for phorum on my website](https://someodd.github.io/showcase/phorum/),
there you will (hopefully) find:

  * More info about phorum
  * How to connect to a live server running `phorum` (actually test out a `phorum` server!)
  * More info on running a server

## Running from repo

Use stack to run:

```
stack run -- --config etc/config.toml launch
```

You may want to run a demo postgres server first:

```
docker run -d --restart unless-stopped --name phorum-postgres-server -e POSTGRES_USER=your_username -e POSTGRES_PASSWORD=your_password -e POSTGRES_DB=your_database -p 5432:5432 -d postgres
```

## Example usage

Here are some commands related to banning:

  * `phorum --config config.toml ban --post 1 --delete --reason "rule violation"`:
    ban user for post #1 and delete said post for the `--reason` supplied.
  * `phorum --config config.toml unban --ip someipv6addr`: remove the ipv6 address
    from the ban table

## Features

Here are some features:

  * View threads as an ASCII-art text file
  * Navigate the board as a menu
  * Post threads and replies
  * Ban system
  * Prevents some reposting and unoriginal threads
  * Post rate limiting
  * Customizable! TOML configuration file!
  * Configurable "secret codes"
  * Handles/parses Gopher and HTTP(S) URIs into menu items