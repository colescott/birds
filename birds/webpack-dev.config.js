const validator = require("webpack-validator")
const merge = require("webpack-merge");
const WebpackOnBuildPlugin = require('on-build-webpack');
const notifier = require("node-notifier");
const moment = require("moment");
const webpack = require("webpack");

const baseConfig = require("./webpack-base.config.js");

const config = merge.smart(baseConfig, {
    devtool: "eval-source-map",
    devServer: {
        inline: true
    },
    plugins: [
        new WebpackOnBuildPlugin((stats) => {
            const seconds = Math.ceil((Number(stats.endTime) - Number(stats.startTime))/1000);
            notifier.notify({
                "title": "Webpack",
                "message": `Build took ${seconds} seconds.`
            });
        }),
        new webpack.DefinePlugin({
            URL_PREFIX: JSON.stringify("http://localhost:8000")
        })
    ],
});

module.exports = validator(config);
