const webpackDevMiddleware = require("webpack-dev-middleware");
const webpackHotMiddleware = require("webpack-hot-middleware");
const webpack  = require("webpack");
const config = require("./birds/webpack-dev.config.js");
const express = require("express");
const compiler = webpack(config);

const app = express();

app.use(webpackDevMiddleware(compiler, {
    noInfo: true,
    publicPath: "/"
}));

app.use(webpackHotMiddleware(compiler));

app.listen(8081);
