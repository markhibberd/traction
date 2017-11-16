traction
========

```
To me, a story can be both concrete and abstract, or a concrete story
can hold abstractions. And abstractions are things that really can't
be said so well with words.
 - David Lynch

Also, strings are not abstractions.
 - A Chanelling of T. Ma
```

A few tools for using postgresql-simple with minimal abstraction.


## Development

Postgres Install / Configure

OS-X

```
## install postgresql
brew install postgresql
## to initialize (once only)
initdb /usr/local/var/postgres -E utf8
## to start
pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start
## to stop
pg_ctl -D /usr/local/var/postgres stop -s -m fast
```

Setting up DB for tests.
```
./bin/db-refresh
./bin/test   # or:
             #     ./mafia quick -p test/test.hs
             #     Main.main
```

Linux (Debian)

```
## install postgresql
apt-get install postgresql postgresql-client libpq-dev

# For the bin/db* scripts to work you have to setup your user to have superuser permissions
# Something like:

# Login as the default database user postgres
sudo -u postgres bash

# Start the client
psql

# Create a user with your username and set permissions
createuser <your-username>;
alter role <your-username> with superuser;

```
