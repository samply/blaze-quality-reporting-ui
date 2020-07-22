module.exports =
    process.env.npm_lifecycle_event === "start"
        ? {
            plugins: [
                require('tailwindcss'),
                require('autoprefixer')
            ]
        }
        : {
            plugins: [
                require('tailwindcss'),
                require('autoprefixer'),
                require('@fullhuman/postcss-purgecss')({
                    content: [
                        'src/**/*.elm'
                    ],
                    defaultExtractor: content => content.match(/[a-z0-9-:]+/g) || []
                })
            ]
        }
