import { defineConfig } from "astro/config";

import tailwind from "@astrojs/tailwind";
import typography from "@tailwindcss/typography";

import mdx from "@astrojs/mdx";

// https://astro.build/config
export default defineConfig({
  site: "https://victorlymar.com",
  integrations: [tailwind(), mdx()],
  markdown: {
    shikiConfig: {
      // Choose from Shiki's built-in themes (or add your own)
      // https://shiki.style/themes
      //theme: 'dracula',
      theme: "material-theme-palenight",
      // theme: 'github-light',
      // Disable the default colors
      // https://shiki.style/guide/dual-themes#without-default-color
      // (Added in v4.12.0)
      defaultColor: false,
      // Enable word wrap to prevent horizontal scrolling
      wrap: true,
      // Add custom transformers: https://shiki.style/guide/transformers
      // Find common transformers: https://shiki.style/packages/transformers
      transformers: [],
    },
  },
  plugins: [typography()],
});
