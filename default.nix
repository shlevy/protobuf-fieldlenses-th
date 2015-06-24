{ mkDerivation, base, lens, protobuf, stdenv, template-haskell
, text
}:
mkDerivation {
  pname = "protobuf-fieldlenses-th";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base lens protobuf template-haskell ];
  testDepends = [ base protobuf text ];
  description = "Template Haskell functions for generating protobuf field lenses";
  license = stdenv.lib.licenses.mit;
}
