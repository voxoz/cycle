Deploy Server
=============

Build, Releasing and Deploy Agent for Synrc Cloud.

Overview
--------

Deploy Server is a Cloud Agent that contstructs a node from nothing.
All needed information is provided by GitHub and Synrc Cloud client
that select Erlang applications to include into a set of Erlang Releases.

* Fetch Deps (rebar get-deps)
* Compile (rebar compile)
* Define Application Set (reltool.config)
* Make Releases (release.sh)
* Configure Releases (cluster_configure.sh)
* Deploy (enable releases, switch ports)

Credits
-------

* Maxim Sokhatsky
* Roman Chvanikoff

OM A HUM
