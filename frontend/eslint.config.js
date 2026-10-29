import globals from "globals";
import eslint from "@eslint/js";
import tseslint from "typescript-eslint";
import { fixupPluginRules } from "@eslint/compat";
import eslintPluginPrettierRecommended from "eslint-plugin-prettier/recommended";
import eslintPluginReactHooks from "eslint-plugin-react-hooks";

export default tseslint.config(
  { ignores: ["dist"] },
  eslint.configs.recommended,
  ...tseslint.configs.recommended,
  eslintPluginPrettierRecommended,

  { files: ["**/*.{js,mjs,cjs,ts,jsx,tsx}"] },
  {
    languageOptions: {
      parserOptions: { ecmaFeatures: { jsx: true } },
      globals: { ...globals.browser },
    },
  },
  {
    plugins: {
      "react-hooks": fixupPluginRules(eslintPluginReactHooks),
    },
  },
  {
    rules: {
      // ...
      ...eslintPluginReactHooks.configs.recommended.rules,
    },
  },
);
