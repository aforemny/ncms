# ncms

- Initialize submodules:
```
$ git submodule init
$ git submodule update
```

- Run ncms backend:
```
$ nix-shell default.nix -A ncms --run "runghc ncms"
```

- Run make:
```
$ make
```

- Open in browser:
```
$ firefox http://0.0.0.0:8000
```

You will be asked to log in with a
[GitHub OAuth App](https://github.com/settings/applications/new).

The frontend assumes that you have write access to the GitHub repository
`aforemny/ncms` on branch `gh-pages`. To make it work for you change the
following occurences:

```
$ ag "aforemny" src
src/Main.elm
663: src/Main.elm   "aforemny"
```
