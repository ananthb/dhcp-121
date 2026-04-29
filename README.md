# Calculate DHCPv4 Option 121 Values

Calculate values for the Classless Static Route Option for
Dynamic Host Configuration Protocol (DHCP) version 4.

[![Deploy to Cloudflare](https://deploy.workers.cloudflare.com/button)](https://deploy.workers.cloudflare.com/?url=https://github.com/ananthb/dhcp-121)

## Build

Install [Elm](https://elm-lang.org/).
Run `elm make` to build the project.

```console
$ elm make src/Main.elm --output=public/app.js
Success! Compiled 1 module.

    Main ─> public/app.js

```

Serve the `public` directory using a web server.
