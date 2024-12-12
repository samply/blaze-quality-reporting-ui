**This project isn't used/supported anymore. It was archived at Dec 12 2024.**

# Blaze Quality Reporting UI

## Usage

The Blaze Quality Reporting UI is a desktop app. Please download an installer for our platform from GitHub releases.

## Development

You need node v14. First run:

```sh
npm install
```

The Blaze Quality Reporting UI is an [Electron][1] app and will start in development mode after running:

```sh
npm run start
```

### Production Build

A production build will create various artifacts for macOS, Linux and Windows in the `out` dir.

```sh
npm run make
```

## License

Copyright 2019 - 2021 The Samply Community

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

[1]: <https://www.electronjs.org>
