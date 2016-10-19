const validator = require("webpack-validator");
const merge = require("webpack-merge");
const webpack = require("webpack");
const path = require("path");
const WebpackMd5Hash = require("webpack-md5-hash");

const baseConfig = require("./webpack-base.config.js");

const config = merge.smart(baseConfig, {
    devtool: "cheap-module-source-map",
    output: {
        path: path.join(__dirname, "./static"),
        filename: "[name].[chunkhash].bundle.js",
        publicPath: "/"
    },
    plugins: [
        new webpack.optimize.UglifyJsPlugin({
            beautify: false,
            comments: false,
            compress: {
                warnings: false,
                drop_console: true
            },
            cacheFolder: path.resolve(__dirname, "public/cached_uglify/"),
            debug: true,
            minimize: true,
            sourceMap: false,
            output: {
                comments: false
            },
            mangle: {
                except: ["$"],
                screw_ie8: true,
                keep_fnames: false
            }
        }),
        new webpack.optimize.DedupePlugin(),
        new webpack.DefinePlugin({
            "process.env": {
                "NODE_ENV": JSON.stringify("production")
            }
        }),
        new WebpackMd5Hash()
    ]
});

module.exports = validator(config);
