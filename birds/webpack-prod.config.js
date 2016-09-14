const validator = require("webpack-validator")
const merge = require("webpack-merge");
const webpackUglifyJsPlugin = require('webpack-uglify-js-plugin');
const webpack = require("webpack");
const path = require("path");

const baseConfig = require("./webpack-base.config.js");

const config = merge.smart(baseConfig, {
    devtool: "cheap-module-source-map",
    plugins: [
        new webpackUglifyJsPlugin({
            cacheFolder: path.resolve(__dirname, 'public/cached_uglify/'),
            debug: true,
            minimize: true,
            sourceMap: false,
            output: {
                comments: false
            },
            compressor: {
                warnings: false
            }
        }),
        new webpack.DefinePlugin({
            "process.env": {
                "NODE_ENV": JSON.stringify("production")
            }
        }),
    ]
});

module.exports = validator(config);
