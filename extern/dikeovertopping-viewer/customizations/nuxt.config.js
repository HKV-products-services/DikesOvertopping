import dotenv from 'dotenv-safe';

dotenv.config();

module.exports = {
  srcDir: 'src',

  env: {
    dikeOvertoppingApi: process.env.DIKE_OVERTOPPING_API,
    rootPath : process.env.ROOT_PATH,
  },
  router: {
    base: process.env.ROOT_PATH,
  },
  head: {
    title: 'Deltares BM Overslag',
    htmlAttrs: {
      lang: 'nl-NL',
    },
    meta: [
      { charset: 'utf-8' },
      { name: 'viewport', content: 'width=device-width, initial-scale=1' },
      {
        hid: 'description',
        name: 'description',
        content: 'Deltares BM overslag',
      },
    ],
    link: [{ rel: 'icon', type: 'image/x-icon', href: process.env.ROOT_PATH + '/favicon.ico' }],
  },

  loading: { color: '#01689b' },

  plugins: [
    { src: './plugins/persist-state', ssr: false },
  ],

  build: {
    extend(config, ctx) {
      // Run ESLint on save
      if (ctx.isDev && ctx.isClient) {
        config.module.rules.push({
          enforce: 'pre',
          test: /\.(js|vue)$/,
          loader: 'eslint-loader',
          exclude: /(node_modules)/,
        });
      }
    },
    postcss: {
      preset: {
        preserve: false,
        importFrom: './src/assets/variables.css',
      },
    },
  },
};
