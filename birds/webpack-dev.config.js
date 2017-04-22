const validator = require("webpack-validator");
const merge = require("webpack-merge");
const WebpackOnBuildPlugin = require("on-build-webpack");
const notifier = require("node-notifier");
const webpack = require("webpack");
const path = require("path");

const baseConfig = require("./webpack-base.config.js");

const config = merge.smart(baseConfig, {
    devtool: "eval-source-map",
    devServer: {
        inline: true
    },
    output: {
        path: path.join(__dirname, "./static"),
        filename: "[name].[hash].bundle.js",
        publicPath: "/"
    },
    plugins: [
        new WebpackOnBuildPlugin(stats => {
            const seconds = Math.ceil((Number(stats.endTime) - Number(stats.startTime)) / 1000);
            notifier.notify({
                title: "Webpack",
                message: `Build took ${seconds} seconds.`
            });
        }),
        new webpack.DefinePlugin({
            "process.env": {
                NODE_ENV: JSON.stringify("development")
            }
        })
    ]
});

module.exports = validator(config);
