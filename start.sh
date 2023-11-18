#!/usr/bin/env sh
set -e
elm make src/Main.elm --output build/main.js --optimize
rm -f build/main.mjs 
echo -e 'var global = {};\n' > build/main.mjs 
sed 's/this..;/global));/' < build/main.js >> build/main.mjs 
echo -e '\n\nexport const Elm = global.Elm;' >> build/main.mjs 
echo -e '================================\n' 
node index.mjs dump-rules
echo "Done"
