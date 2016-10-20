module.exports = {
    "parser": "babel-eslint",
    "env": {
        "browser": true,
        "node": true,
        "mocha": true,
        "es6": true,
    },
    "parserOptions": {
        "ecmaVersion": 6,
        "sourceType": "module",
        "ecmaFeatures": {
            "jsx": true
        }
    },
    "plugins": [
        "react"
    ],
    "globals": {
        URL_PREFIX: true
    },
    "extends": "eslint:recommended",
    "rules": {
        "no-console": ["error", { "allow": ["warn", "error"] }],
        "use-isnan": "error",
        "valid-typeof": "error",
        "dot-notation": "error",
        "default-case": "error",
        "array-callback-return": "error",
        "no-alert": "error",
        "no-caller": "error",
        "no-global-assign": "error",
        "no-labels": "error",
        "no-invalid-this": "error",
        "no-new-wrappers": "error",
        "no-param-reassign": "error",
        "no-self-compare": "error",
        "no-with": "error",
        "vars-on-top": "error",
        "no-catch-shadow": "error",
        "no-undefined": "error",
        "brace-style": "error",
        "camelcase": "error",
        "no-bitwise": "error",
        "no-continue": "error",
        "no-inline-comments": "error",
        "arrow-spacing": "error",
        "generator-star-spacing": ["error", {"before": false, "after": true}],
        "no-duplicate-imports": "error",
        "prefer-template": "error",
        "prefer-spread": "error",
        "prefer-rest-params": "error",
        "array-bracket-spacing": ["error", "never"],
        "block-spacing": "error",
        "comma-spacing": ["error", { "before": false, "after": true }],
        "computed-property-spacing": ["error", "always"],
        "func-call-spacing": ["error", "never"],
        "key-spacing": ["error", { "beforeColon": false, "afterColon": true, "mode": "strict" }],
        "keyword-spacing": ["error", { "before": true, "after": true }],
        "object-curly-spacing": ["error", "always"],
        "semi-spacing": ["error", {"before": false, "after": false}],
        "space-before-blocks": "error",
        "space-before-function-paren": ["error", "never"],
        "space-in-parens": ["error", "never"],
        "space-infix-ops": "error",
        "quotes": [
            "error",
            "double"
        ],
        "semi": [
            "error",
            "always"
        ],
        "react/jsx-uses-react": "error",
        "react/jsx-uses-vars": "error",
    }
}
