# wilks-calculator

## Building

```bash
mkdir -p target
elm-make src/main.elm --output target/wilks.js
```

## Optimize
Download the Google closure compiler: [Link](https://dl.google.com/closure-compiler/compiler-latest.zip)

```bash
wget https://dl.google.com/closure-compiler/compiler-latest.zip -P ./bin
unzip ./bin/compiler-latest.zip -d ./bin
java -jar ./bin/closure-compiler-*.jar --js target/wilks.js --js_output_file target/wilks-compiled.js
```
