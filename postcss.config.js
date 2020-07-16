const merge = require("webpack-merge");

var MODE =
    process.env.npm_lifecycle_event === "prod" ? "production" : "development";

var common = {
    plugins: [
        require('tailwindcss'),
        require('autoprefixer')
    ]
}

if (MODE === "production") {
    module.exports = merge(common, {
        plugins: [
            require('@fullhuman/postcss-purgecss')({
                content: [
                    'src/**/*.elm'
                ],
                defaultExtractor: content => content.match(/[a-z0-9-:]+/g) || []
            })
        ]
    })
} else {
    module.exports = common;
}
