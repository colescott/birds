const path = require("path");
const validator = require("webpack-validator");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const config = {
    entry: ["babel-polyfill", path.join(__dirname, "./src/index.js")],
    output: {
        path: path.join(__dirname, "./static"),
        filename: "bundle.js",
        publicPath: "/"
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: "Birds"
        }),
        new webpack.DefinePlugin({
            URL_PREFIX: JSON.stringify(process.env.URL_PREFIX || "")
        }),
        new webpack.ProvidePlugin({
            React: "react"
        })
    ],
    module: {
        loaders: [
            {
                loaders: ["babel"],
                test: /\.js$/
            },
            {
                test: /\.scss$/,
                loaders: ["style", "css", "sass"]
            }
        ],
    }
};

module.exports = validator(config);
