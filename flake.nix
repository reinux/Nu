{
  description = "nu dev environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
    let 
      allSystems = [
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs allSystems (system: f {
        pkgs = import nixpkgs {inherit system; config.allowUnfree = true;};
      });
    in {

      #packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
      #packages.x86_64-linux.default = self.packages.x86_64-linux.hello;
      devShells = forAllSystems ({pkgs}: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            dotnetCorePackages.sdk_8_0_1xx
          ];
          LD_LIBRARY_PATH = with pkgs; pkgs.lib.makeLibraryPath [
            SDL2
            SDL2_ttf
            # SDL2_ttf_2_0_15
            SDL2_gfx
            SDL2_sound
            SDL2_mixer
            SDL2_image
            SDL2_Pango
            assimp
            renderdoc
            renderdoc.out

            libwebp
            libjpeg
            libtiff
            libmikmod
            libfishsound
            smpeg
            liboggz
            flac
            fluidsynth

            xorg.libX11
          ];
        };
      });
    };
}
