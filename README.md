# ncms

- Initialize submodules:
```
$ git submodule init
$ git submodule update
```

- Build:
```
$ nix-build . -A ncms
```

- Run:
```
$ mkdir example
$ cd example
$ ../result/bin/ncms
```

- Open in browser:
```
$ firefox http://0.0.0.0:8000
```
