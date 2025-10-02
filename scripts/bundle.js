const esbuild = require("esbuild");
esbuild
  .build({
    entryPoints: ["output/Main/index.js"],
    bundle: true,
    platform: "node",
    outfile: "dist/wordladder",
    format: "cjs",
    target: "node14", // Enable this - defines minimum Node version
    minify: true, // Minify the output
    treeShaking: true, // Remove unused code
    sourcemap: false, // Set to true for debugging
    banner: {
      js: "#!/usr/bin/env node"
    },
    footer: {
      js: "module.exports.main();" // Simpler approach
    },
    external: ["fs", "path", "readline"] // Don't bundle Node built-ins
  })
  .then(() => {
    console.log("✓ Build successful!");
    console.log("Run with: `node dist/wordladder` or `./dist/wordladder`");
    process.exit(0);
  })
  .catch((err) => {
    console.error("✗ Build failed:", err);
    process.exit(1);
  });
