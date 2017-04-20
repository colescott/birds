module.exports.forceSSL = (req, res, next) => {
    // This code checks the protocol from heroku proxy
    if (req.headers[ "x-forwarded-proto" ] !== "https") {
        // Redirect with a 301
        return res.redirect(301, ["https://", req.get("Host"), req.url].join(""));
    }
    return next();
 };