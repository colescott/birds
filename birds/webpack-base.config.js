const path = require("path");
const validator = require("webpack-validator");
const webpack = require("webpack");
const HtmlWebpackPlugin = require("html-webpack-plugin");

const config = {
    entry: {
        app: ["babel-polyfill", path.join(__dirname, "./src/index.js")],
        vendor: ["react", "react-dom"]
    },
    plugins: [
        new HtmlWebpackPlugin({
            title: "Birds"
        }),
        new webpack.DefinePlugin({
            URL_PREFIX: JSON.stringify(process.env.URL_PREFIX || "")
        }),
        new webpack.optimize.CommonsChunkPlugin({
            name: "common"
        })
    ],
    module: {
        loaders: [
            {
                loader: "babel",
                test: /\.js$/,
                query: {
                    presets: [["es2015", { modules: false }], "react", "stage-2"],
                    babelrc: false
                }
            },
            {
                test: /\.css$/,
                loaders: ["style", "css-loader"]
            },
            {
                test: /\.json$/,
                loader: "json"
            }
        ]
    }
};

module.exports = validator(config);
