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
