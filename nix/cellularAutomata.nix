{ mkDerivation, base, brick, lib, ncurses, process, random, turtle
}:
mkDerivation {
  pname = "cellularAutomata";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base brick process random turtle ];
  executableSystemDepends = [ ncurses ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
