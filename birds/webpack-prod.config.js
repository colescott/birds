const validator = require("webpack-validator");
const merge = require("webpack-merge");
const webpack = require("webpack");

const baseConfig = require("./webpack-base.config.js");

const config = merge.smart(baseConfig, {
    devtool: "cheap-module-source-map",
    plugins: [
<<<<<<< dd1e266e8472835e42a4b66f64cdcedbc864bad3
        new webpack.optimize.UglifyJsPlugin({
            beautify: false,
            comments: false,
            compress: {
                warnings: false,
                drop_console: true
=======
        /*
        new webpackUglifyJsPlugin({
            cacheFolder: path.resolve(__dirname, "public/cached_uglify/"),
            debug: true,
            minimize: true,
            sourceMap: false,
            output: {
                comments: false
>>>>>>> add form validation
            },
            mangle: {
                except: ["$"],
                screw_ie8 : true,
                keep_fnames: false
            }
        }),
        */
        new webpack.DefinePlugin({
            "process.env": {
                "NODE_ENV": JSON.stringify("production")
            }
        }),
    ]
});

module.exports = validator(config);
