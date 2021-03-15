const path = require('path');
const webpack = require('webpack');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const ForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin');
const shell = require('shelljs');

const distPath = path.resolve(__dirname, process.env.DIST_FOLDER || 'dist');

module.exports = (env, options) => {
  const config = {
    entry: './src/index.tsx',
    output: {
      filename: '[name].bundle.js',
      chunkFilename: '[name].bundle.js',
      path: distPath,
      publicPath: '/',
      globalObject: 'this'
    },

    // Enable sourcemaps for debugging webpack's output.
    devtool: 'source-map',

    devServer: {
      liveReload: false
    },

    resolve: {
      alias: {
        'react-dom': '@hot-loader/react-dom'
      },
      // Add '.ts' and '.tsx' as resolvable extensions.
      extensions: ['.ts', '.tsx', '.js', '.json']
    },

    module: {
      rules: [
        { test: /\.css$/, use: ['style-loader', 'css-loader'] },
        { test: /\.(png|woff|woff2|eot|ttf|svg)$/, loader: 'url-loader?limit=100000' },
        // All files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'.
        {
          test: /\.(j|t)s(x)?$/,
          exclude: /node_modules/,
          use: {
            loader: 'babel-loader',
            options: {
              cacheDirectory: true,
              babelrc: false,
              presets: [
                [
                  '@babel/preset-env',
                  { targets: { browsers: 'last 2 versions' } } // or whatever your project requires
                ],
                '@babel/preset-typescript',
                '@babel/preset-react'
              ],
              plugins: [
                '@babel/plugin-transform-modules-commonjs',
                // plugin-proposal-decorators is only needed if you're using experimental decorators in TypeScript
                ['@babel/plugin-proposal-decorators', { legacy: true }],
                ['@babel/plugin-proposal-class-properties', { loose: true }],
                '@babel/transform-runtime',
                'react-hot-loader/babel'
              ]
            }
          }
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
      new ForkTsCheckerWebpackPlugin(),
      new MonacoWebpackPlugin(),
      new webpack.DefinePlugin({
        'process.env.SERVICE_URL': JSON.stringify(process.env.SERVICE_URL)
      })
    ],
    optimization: {
      splitChunks: {
        cacheGroups: {
          editor: {
            // Editor bundle
            test: /[\\/]node_modules\/(monaco-editor\/esm\/vs\/(nls\.js|editor|platform|base|basic-languages|language\/(css|html|json|typescript)\/monaco\.contribution\.js)|style-loader\/lib|css-loader\/lib\/css-base\.js)/,
            name: 'monaco-editor',
            chunks: 'async'
          },
          languages: {
            // Language bundle
            test: /[\\/]node_modules\/monaco-editor\/esm\/vs\/language\/(css|html|json|typescript)\/(_deps|lib|fillers|languageFeatures\.js|workerManager\.js|tokenization\.js|(tsMode|jsonMode|htmlMode|cssMode)\.js|(tsWorker|jsonWorker|htmlWorker|cssWorker)\.js)/,
            name: 'monaco-languages',
            chunks: 'async'
          }
        }
      }
    }
  };

  if (options.mode === 'production') {
    config.plugins.push({
      apply: (compiler) => {
        compiler.hooks.afterEmit.tap('AfterEmitPlugin', (compilation) => {
          // copy file to destination
          shell.cp(path.resolve(__dirname, 'index.html'), distPath);
          shell.cp(path.resolve(__dirname, 'config.json'), distPath);
          shell.cp('-r', path.resolve(__dirname, 'style'), distPath);
          shell.cp('-r', path.resolve(__dirname, 'fonts'), distPath);
          shell.cp('-r', path.resolve(__dirname, 'lib'), distPath);
          shell.cp('-r', path.resolve(__dirname, 'img'), distPath);
        });
      }
    });
  }
  return config;
};
