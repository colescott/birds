const path = require("path");
const validator = require('webpack-validator');

const HtmlWebpackPlugin = require('html-webpack-plugin');

const config = {
    entry: [path.join(__dirname, "./src/index.js")],
    output: {
        path: path.join(__dirname, "./static"),
        filename: "bundle.js"
    },
    plugins: [
        new HtmlWebpackPlugin()
    ],
    module: {
        loaders: [
            {
                loader: "babel",
                test: /\.js$/,
                exclude: /node_modules/
            }
        ]
    }
};

module.exports = validator(config);
