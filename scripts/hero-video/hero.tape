# The landing page hero. Render from the repo root, after a debug build:
#   dotnet build src/Fantomas/Fantomas.fsproj
#   vhs scripts/hero-video/hero.tape
Output "docs/videos/hero.mp4"
Output "docs/videos/hero.webm"
Set Shell bash
Set FontSize 28
Set Width 1200
Set Height 570
Set Padding 40
Set Theme "Catppuccin Mocha"
Set TypingSpeed 60ms
Set Framerate 30
Hide
Type `export FANTOMAS_REPO="$PWD" && rm -rf /tmp/hero-demo && cp -r scripts/hero-video/fixture /tmp/hero-demo && cd /tmp/hero-demo` Enter
Type `export PATH="$FANTOMAS_REPO/scripts/hero-video/bin:$PATH"` Enter
Type `export PS1="\[\e[38;5;117m\]~/app\[\e[0m\] $ "` Enter
Type `clear` Enter
Show
Sleep 1s
Type `fantomas check src` Sleep 500ms Enter
Sleep 2500ms
Type `fantomas src` Sleep 500ms Enter
Sleep 2500ms
Type `fantomas check src` Sleep 500ms Enter
Sleep 3s
