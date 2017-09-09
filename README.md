# ncms

- Initialize submodules:
```
$ git submodule init
$ git submodule update
```

- Edit `file-reader/src/Native/FileReader.js`

```
$ sed -i 's/_simonh1000\$file_reader/_aforemny$ncms/' file-reader/src/Native/FileReader.js
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
