const validator = require("webpack-validator");
const merge = require("webpack-merge");
const webpack = require("webpack");

const baseConfig = require("./webpack-base.config.js");

const config = merge.smart(baseConfig, {
    devtool: "cheap-module-source-map",
    plugins: [
        new webpack.optimize.UglifyJsPlugin({
            beautify: false,
            comments: false,
            compress: {
                warnings: false,
                drop_console: true
            },
            mangle: {
                except: ["$"],
                screw_ie8 : true,
                keep_fnames: false
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
