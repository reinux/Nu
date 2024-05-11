let
  pkgs = import<nixpkgs> {};
in
  pkgs.mkShell rec {
    name = "dotnet-env";
    packages = with pkgs; [
      dotnetCorePackages.sdk_8_0
      # steam-run
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

    ] + ":/run/opengl-driver/lib:/run/opengl-driver-32/lib";
  }

