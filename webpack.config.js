const path = require('path');
const webpack = require('webpack');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const shell = require('shelljs');

const distPath = path.resolve(__dirname, process.env.DIST_FOLDER || 'dist');

module.exports = (env, options) => {
  const config = {
    entry: {
      main: './src/index.tsx',
      'editor.worker': 'monaco-editor/esm/vs/editor/editor.worker.js',
      'json.worker': 'monaco-editor/esm/vs/language/json/json.worker'
    },
    output: {
      filename: '[name].bundle.js',
      chunkFilename: '[name].bundle.js',
      path: distPath,
      publicPath: '/',
      globalObject: 'this'
    },

    // Enable sourcemaps for debugging webpack's output.
    devtool: options.mode === 'production' ? false : 'eval-source-map',

    devServer: {
      historyApiFallback: true,
      liveReload: false,
      static: path.resolve(__dirname),
      port: process.env.PORT || 8080,
      headers: {
        'Cross-Origin-Resource-Policy': 'cross-origin',
        'Cross-Origin-Opener-Policy': 'same-origin',
        'Cross-Origin-Embedder-Policy': 'require-corp'
      },
      client: {
        overlay: {
          errors: true,
          warnings: false
        }
      }
    },

    resolve: {
      alias: {
        'react-dom': '@hot-loader/react-dom'
      },
      // Add '.ts' and '.tsx' as resolvable extensions.
      extensions: ['.ts', '.tsx', '.js', '.json', '.wasm'],
      fallback: {
        fs: false,
        tls: false,
        net: false,
        os: false,
        url: false,
        path: false,
        assert: false,
        http: false,
        crypto: false,
        'process/browser': require.resolve('process/browser'),
        buffer: require.resolve('buffer/'),
        stream: require.resolve('stream-browserify'),
        https: require.resolve('https-browserify')
      }
    },
    stats: 'errors-only',

    module: {
      rules: [
        {
          test: /\.rs$/,
          use: ['raw-loader']
        },
        { test: /\.css$/, use: ['style-loader', 'css-loader'] },
        { test: /\.(png|woff|woff2|eot|ttf|svg)$/, use: ['file-loader'] },
        // All files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'.
        {
          test: /\.(j|t)s(x)?$/,
          exclude: /node_modules\//,
          use: 'ts-loader'
        },

        // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
        { enforce: 'pre', test: /\.js$/, loader: 'source-map-loader', exclude: path.resolve(__dirname, 'node_modules') }
      ]
    },

    // When importing a module whose path matches one of the following, just
    // assume a corresponding global variable exists and use that instead.
    // This is important because it allows us to avoid bundling all of our
    // dependencies, which allows browsers to cache those libraries between builds.

    plugins: [
      options.mode !== 'production' && new ForkTsCheckerWebpackPlugin(),
      new webpack.EnvironmentPlugin({
        SERVICE_URL: false,
        NODE_ENV: options.mode,
        LCD: 'https://testnet.lcd.orai.io'
      }),
      new webpack.ProvidePlugin({
        process: 'process/browser'
      })
    ].filter(Boolean),
    experiments: {
      asyncWebAssembly: true
    },
    // maximum 20 MB
    performance: {
      hints: false,
      maxEntrypointSize: 40480000,
      maxAssetSize: 40480000
    }
  };

  if (options.mode === 'production') {
    config.plugins.push({
      apply: (compiler) => {
        compiler.hooks.afterEmit.tap('AfterEmitPlugin', (compilation) => {
          // copy file to destination
          shell.exec(`yarn templates ${distPath}/templates`);
          shell.cp(['index.html', 'change.json'], distPath);
          shell.cp('-r', 'lib', distPath);
          shell.cp('-r', ['notes', 'assets'], distPath);
        });
      }
    });
  }
  return config;
};
