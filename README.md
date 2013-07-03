Release Server
==============

Build, Release and Deploy within Customer LXC Container in Synrc PaaS.

Overview
--------

Deploy Server is a Cloud Agent that contstructs a node from nothing.
All needed information is provided by GitHub and Synrc Cloud client
that select Erlang applications to include into a set of Erlang Releases.

* Fetch Deps (rebar get-deps)
* Compile (rebar compile)
* Make Projects
* Make Releases (reltool.config)
* Configure Releases (Add/Remove Apps)
* Commit/Push Release/Project/App
* Continuous Integration Web Pages
* REST API
* Deploy inplace (enable releases, switch ports)
* Deploy to target (XEN, other destinations)

Credits
-------

* Maxim Sokhatsky

OM A HUM
