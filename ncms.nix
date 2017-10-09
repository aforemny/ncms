{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, directory, filepath, mtl, process, snap, snap-server, stdenv
, text, unordered-containers, vector
}:
mkDerivation {
  pname = "ncms";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers directory filepath mtl
    process snap snap-server text unordered-containers vector
  ];
  homepage = "https://aforemny.github.io/ncms/";
  license = stdenv.lib.licenses.bsd3;
}
