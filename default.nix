{ mkDerivation, acme-iot, base, csound-catalog, csound-expression
, csound-sampler, distributive, extra, hpack, lens, lib, listsafe
, MonadRandom, mtl, random, random-shuffle, sort, split
}:
mkDerivation {
  pname = "funktor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acme-iot base csound-catalog csound-expression csound-sampler
    distributive extra lens listsafe MonadRandom mtl random
    random-shuffle sort split
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    acme-iot base csound-catalog csound-expression csound-sampler
    distributive extra lens listsafe MonadRandom mtl random
    random-shuffle sort split
  ];
  testHaskellDepends = [
    acme-iot base csound-catalog csound-expression csound-sampler
    distributive extra lens listsafe MonadRandom mtl random
    random-shuffle sort split
  ];
  prePatch = "hpack";
  homepage = "https://github.com/harryaskham/funktor#readme";
  license = lib.licenses.bsd3;
}
