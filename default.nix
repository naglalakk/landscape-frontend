{ port ? "8080"
, apiURL ? ""
, apiKey ? ""
, environment ? "Production"
}:
  let
    pkgs = import ./packages.nix {};
    spagoPkgs = import ./spago-packages.nix { inherit pkgs; };
    easy-ps = import (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "340e82b6ecaccc4059740e69f8ec18546b527481";
      sha256 = "1q2ciwd3193kig1paidzrgxl60y4rb39bsi97lk7m6ff8mis6z6i";
    }) { inherit pkgs; };

    gitignoreSrc = pkgs.fetchFromGitHub { 
      owner = "hercules-ci";
      repo = "gitignore.nix";
      rev = "7415c4feb127845553943a3856cbc5cb967ee5e0";
      sha256 = "sha256:1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
    };
    inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
    
    yarn = pkgs.fetchFromGitHub {
      owner = "moretea";
      repo  = "yarn2nix";
      rev = "9e7279edde2a4e0f5ec04c53f5cd64440a27a1ae";
      sha256 = "sha256:0zz2lrwn3y3rb8gzaiwxgz02dvy3s552zc70zvfqc0zh5dhydgn7";
    };

    yarn2nix = import yarn { inherit pkgs; };

    npm = yarn2nix.mkYarnPackage {
      name = "frontend-npm";
      src = gitignoreSource ./.;
      packageJSON = ./package.json;
      yarnLock  = ./yarn.lock;
    };

    nodejs = pkgs.nodejs-12_x;
  in
    pkgs.stdenv.mkDerivation {
      name = "donnacodes.dev-frontend";
      src = gitignoreSource ./.;
      buildInputs = [ 
        easy-ps.purs easy-ps.spago npm pkgs.nodejs-12_x
      ];
      buildPhase = ''
        mkdir -p $out
        ${spagoPkgs.installSpagoStyle}/bin/install-spago-style
        purs compile "$src/**/*.purs" ${builtins.toString
          (builtins.map
            (x: ''"${x.outPath}/src/**/*.purs"'')
            (builtins.attrValues spagoPkgs.inputs))}
        mv output $out
        cp -r ${npm}/libexec/donnacodes.dev/node_modules $out
      '';

      installPhase = ''
        mkdir -p $out/output/donnabot
        mkdir -p $out/static

        # Copy static files
        cp -r $src/static/img   $out/static
        cp -r $src/static/style $out/static
        cp -r $src/static/views $out/static

        echo PORTNR=${port} >> $out/.env
        echo API_URL=${apiURL} >> $out/.env
        echo API_KEY=${apiKey} >> $out/.env
        echo ENVIRONMENT=${environment} >> $out/.env

        # Make bundle
        cd $out && spago bundle-app --no-install --no-build --to $out/static/build/index.js

        # Browserify bundle
        $out/node_modules/parcel/bin/cli.js build $out/static/build/index.js -d $out/static/dist

        # Bundle Server
        cd $out && spago bundle-app --main Server --no-install --no-build --to $out/output/donnabot/server.js

        echo "#!/usr/bin/env bash" >> $out/run.sh
        echo "${nodejs}/bin/node $out/output/donnabot/server.js" >> $out/run.sh
        chmod +x $out/run.sh
        echo "Build Done"
      '';
    }
