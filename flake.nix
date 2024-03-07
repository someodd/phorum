{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          # The base package set representing a specific GHC version.
          # By default, this is pkgs.haskellPackages.
          # You may also create your own. See https://community.flake.parts/haskell-flake/package-set
          # basePackages = pkgs.haskellPackages;

          # Extra package information. See https://community.flake.parts/haskell-flake/dependency
          #
          # Note that local packages are automatically included in `packages`
          # (defined by `defaults.packages` option).
          #
          packages = {
            # aeson.source = "1.5.0.0"; # Hackage version override
            # shower.source = inputs.shower;
          };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
          };

          devShell = {
            # Enabled by default
            # enable = true;
            tools = hp: {
              inherit (hp) fourmolu;
              ghcid = null;
	      gopher = pkgs.gopher;
              postgresql = pkgs.postgresql;
            };

            mkShellArgs = {

shellHook='' 
  # Setup the PostgreSQL data directory relative to the current project directory
  export PGDATA="$PWD/.pgdata"

  # Initialize the PostgreSQL data directory if it hasn't been initialized
  if [ ! -f "$PGDATA/PG_VERSION" ]; then
    echo "Initializing PostgreSQL data directory..."
    initdb -D "$PGDATA" -E UTF8 --no-locale -A trust
  fi

  # Start PostgreSQL
  echo "Starting PostgreSQL..."
  pg_ctl -D "$PGDATA" -l "$PGDATA/server.log" -o "-k $PGDATA" start
  sleep 3 # Give the server time to start

  export PGHOST="$PGDATA" # Ensure PGHOST points to the directory containing the socket

  # Creating the user and database, ensuring to use the socket directory for connection
  echo "Creating user and database..."
  psql -h "$PGHOST" -d postgres -c "CREATE USER your_username WITH PASSWORD 'your_password';" || echo "User your_username might already exist."
  psql -h "$PGHOST" -d postgres -c "CREATE DATABASE your_database OWNER your_username;" || echo "Database your_database might already exist."

  # Cleanup function
  function cleanup {
    echo "Stopping PostgreSQL server and cleaning up..."
    pg_ctl -D "$PGDATA" stop
    rm -rf "$PGDATA"
  }

  trap cleanup EXIT

  echo "PostgreSQL is ready. Use Ctrl+D or type 'exit' to stop and clean up."
'';



            };

            # Programs you want to make available in the shell.
            # Default programs can be disabled by setting to 'null'
            # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

            hlsCheck.enable = pkgs.stdenv.isDarwin; # On darwin, sandbox is disabled, so HLS can use the network.
          };
        };

        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.gopherden;
      };
    };
}
