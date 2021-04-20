{ mkDerivation, base, brick, containers, deepseq, lib, linear
, microlens, microlens-th, ncurses, process, random, turtle, vty
}:
mkDerivation {
  pname = "cellularAutomata";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base brick containers deepseq linear microlens microlens-th process
    random turtle vty
  ];
  executableSystemDepends = [ ncurses ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
