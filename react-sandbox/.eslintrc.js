module.exports = {
  env: {
    browser: true,
    es2020: true,
  },
  extends: [
    'plugin:react/recommended',
    'airbnb',
    'plugin:@typescript-eslint/recommended',
  ],
  parser: '@typescript-eslint/parser',
  parserOptions: {
    ecmaFeatures: {
      jsx: true,
    },
    ecmaVersion: 11,
    sourceType: 'module',
  },
  plugins: [
    'react',
    '@typescript-eslint',
  ],
  rules: {
    'react/jsx-filename-extension': [2, { extensions: ['.jsx', '.tsx'] }],
    'import/extensions': [2, { extensions: ['.js', '.jsx', '.json', '.ts', '.tsx'] }],
    'no-continue': 'off',
    'jsx-quotes': [2, 'prefer-single'],
  },
  settings: {
    'import/resolver': {
      typescript: {},
    },
  },
};
