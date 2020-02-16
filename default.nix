{}:
  let
    pkgs = import ./packages.nix {};
    spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
    easy-ps = import (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "refs/heads/master";
      sha256 = "0lmkppidmhnayv0919990ifdd61f9d23dzjzr8amz7hjgc74yxs0";
    }) { inherit pkgs; };
    
    yarn2nix = import /Users/Donna/Code/K0TT/github/yarn2nix { inherit pkgs; };
    npm = yarn2nix.mkYarnPackage {
      name = "frontend-npm";
      src = ./.;
      packageJSON = ./package.json;
      yarnLock  = ./yarn.lock;
    };

  in
    pkgs.stdenv.mkDerivation {
      name = "donnabot.dev-frontend";
      src = ./.;
      buildInputs = [ 
        easy-ps.purs easy-ps.spago npm pkgs.nodejs-12_x
      ];
      buildPhase = ''
        ${spagoPkgs.installSpagoStyle}
        mkdir -p $out
        purs compile "$src/**/*.purs" ${builtins.toString
          (builtins.map
            (x: ''"${x.outPath}/src/**/*.purs"'')
            (builtins.attrValues spagoPkgs.inputs))}
        mv output $out
        cp -r ${npm}/libexec/blog/node_modules $out
      '';

      installPhase = ''
        mkdir -p $out/output/donnabot

        cd $out && spago bundle-app --main Server --no-install --no-build --to $out/output/donnabot/server.js

        echo "#!/usr/bin/env bash" >> $out/run.sh
        echo "node $out/output/donnabot/server.js" >> $out/run.sh
        chmod +x $out/run.sh
        echo "Build Done"
      '';

    }
