import type { Config } from "tailwindcss";

export default {
  content: [
    "./src/**/*.{js,elm,ts,css,html}",
    "./.elm-land/**/*.{js,elm,ts,css,html}",
  ],
  theme: {
    extend: {},
  },
  plugins: [
     require('@tailwindcss/forms')
  ],
} satisfies Config;
