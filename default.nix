{ yarn2nixPath ? /Users/Donna/Code/K0TT/github/yarn2nix }:
  let
    pkgs = import ./packages.nix {};
    spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
    easy-ps = import (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "refs/heads/master";
      sha256 = "1q2ciwd3193kig1paidzrgxl60y4rb39bsi97lk7m6ff8mis6z6i";
    }) { inherit pkgs; };

     removeHashBang = drv: drv.overrideAttrs (oldAttrs: {
       buildCommand = builtins.replaceStrings ["#!/usr/bin/env"] [""] oldAttrs.buildCommand;
     });
    
    yarn2nix = import yarn2nixPath { inherit pkgs; };
    npm = yarn2nix.mkYarnPackage {
      name = "frontend-npm";
      src = ./.;
      packageJSON = ./package.json;
      yarnLock  = ./yarn.lock;
    };

    nodejs = pkgs.nodejs-12_x;
  in
    pkgs.stdenv.mkDerivation {
      name = "donnabot.dev-frontend";
      src = ./.;
      buildInputs = [ 
        easy-ps.purs easy-ps.spago npm pkgs.nodejs-12_x
      ];
      buildPhase = ''
        ${removeHashBang spagoPkgs.installSpagoStyle}
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
        mkdir -p $out/static

        # Copy static files
        cp -r $src/static/img $out/static
        cp -r $src/static/style $out/static
        cp -r $src/static/views $out/static

        # Make bundle
        cd $out && spago bundle-app --no-install --no-build --to $out/static/build/index.js

        # Browserify bundle
        cd $out && ${npm}/libexec/blog/node_modules/parcel/bin/cli.js build $out/static/build/index.js -d $out/static/dist

        # Bundle Server
        cd $out && spago bundle-app --main Server --no-install --no-build --to $out/output/donnabot/server.js

        echo "#!/usr/bin/env bash" >> $out/run.sh
        echo "${nodejs}/bin/node $out/output/donnabot/server.js" >> $out/run.sh
        chmod +x $out/run.sh
        echo "Build Done"
      '';

    }
